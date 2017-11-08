(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
(** Functions relating to Xen domains *)

open Printf
open Xenops_utils
open Xenstore
open Cancel_utils

open Xenops_helpers
open Device_common
open Xenops_task

module D = Debug.Make(struct let name = "xenops" end)
open D

type create_info = {
  ssidref: int32;
  hvm: bool;
  hap: bool;
  name: string;
  xsdata: (string * string) list;
  platformdata: (string * string) list;
  bios_strings: (string * string) list;
  has_vendor_device: bool;
} [@@deriving rpc]

type build_hvm_info = {
  shadow_multiplier: float;
  video_mib: int;
} [@@deriving rpc]

type build_pv_info = {
  cmdline: string;
  ramdisk: string option;
} [@@deriving rpc]

type builder_spec_info = BuildHVM of build_hvm_info | BuildPV of build_pv_info
[@@deriving rpc]

type build_info = {
  memory_max: int64;    (* memory max in kilobytes *)
  memory_target: int64; (* memory target in kilobytes *)
  kernel: string;       (* in hvm case, point to hvmloader *)
  vcpus: int;           (* vcpus max *)
  priv: builder_spec_info;
} [@@deriving rpc]

type domid = int

let allowed_xsdata_prefixes = [ "vm-data"; "FIST" ]

let filtered_xsdata =
  (* disallowed by default; allowed only if it has one of a set of prefixes *)
  let allowed (x, _) = List.fold_left (||) false (List.map (fun p -> String.startswith (p ^ "/") x) allowed_xsdata_prefixes) in
  List.filter allowed

exception Suspend_image_failure
exception Not_enough_memory of int64
exception Domain_build_failed
exception Domain_build_pre_failed of string
exception Domain_restore_failed
exception Domain_restore_truncated_hvmstate
exception Domain_restore_truncated_vgpustate
exception Xenguest_protocol_failure of string (* internal protocol failure *)
exception Xenguest_failure of string (* an actual error is reported to us *)
exception Emu_manager_protocol_failure (* internal protocol failure *)
exception Emu_manager_failure of string (* an actual error is reported to us *)
exception Timeout_backend
exception Could_not_read_file of string (* eg linux kernel/ initrd *)
exception Domain_stuck_in_dying_state of Xenctrl.domid

let releaseDomain = "@releaseDomain"
let introduceDomain = "@introduceDomain"

module Uuid = Uuidm

let log_exn_continue msg f x = try f x with e -> debug "Safely ignoring exception: %s while %s" (Printexc.to_string e) msg

let log_exn_rm ~xs x = log_exn_continue ("xenstore-rm " ^ x) xs.Xs.rm x

let set_difference a b = List.filter (fun x -> not(List.mem x b)) a

let assert_file_is_readable filename =
  try Unix.access filename [ Unix.F_OK; Unix.R_OK ]
  with _ ->
    error "Cannot read file %s" filename;
    raise (Could_not_read_file filename)
let maybe f = function None -> () | Some x -> f x

(* Recursively iterate over a directory and all its children, calling fn for each *)
let rec xenstore_iter t fn path =
  fn path;
  match t.Xst.directory path with
  | [] -> ()
  | names -> List.iter (fun n -> if n <> "" then xenstore_iter t fn (path ^ "/" ^ n)) names

let xenstore_read_dir t path =
  let rec inner acc nodes =
    match nodes with
    | [] -> acc
    | n::ns ->
      let v = try t.Xst.read n with _ -> "" in
      let children = match t.Xst.directory n with
        | [] | [""] -> []
        | x -> List.map (Printf.sprintf "%s/%s" n) x
      in
      inner ((n, v) :: acc) (children @ ns)
  in
  inner [] [path] |> List.fast_sort compare

type domarch = Arch_HVM | Arch_native | Arch_X64 | Arch_X32

let string_of_domarch = function
  | Arch_HVM    -> "hvm"
  | Arch_native -> ""
  | Arch_X64    -> "x64"
  | Arch_X32    -> "x32"

let domarch_of_string = function
  | "hvm" -> Arch_HVM
  | "x64" -> Arch_X64
  | "x32" -> Arch_X32
  | _     -> Arch_native

let get_uuid ~xc domid =
  Xenctrl_uuid.uuid_of_handle (Xenctrl.domain_getinfo xc domid).Xenctrl.handle

let wait_xen_free_mem ~xc ?(maximum_wait_time_seconds=64) required_memory_kib : bool =
  let open Memory in
  let rec wait accumulated_wait_time_seconds =
    let host_info = Xenctrl.physinfo xc in
    let free_memory_kib =
      kib_of_pages (Int64.of_nativeint host_info.Xenctrl.free_pages) in
    let scrub_memory_kib =
      kib_of_pages (Int64.of_nativeint host_info.Xenctrl.scrub_pages) in
    (* At exponentially increasing intervals, write  *)
    (* a debug message saying how long we've waited: *)
    if is_power_of_2 accumulated_wait_time_seconds then debug
        "Waited %i second(s) for memory to become available: \
         			%Ld KiB free, %Ld KiB scrub, %Ld KiB required"
        accumulated_wait_time_seconds
        free_memory_kib scrub_memory_kib required_memory_kib;
    if free_memory_kib >= required_memory_kib
    (* We already have enough memory. *)
    then true else
    if scrub_memory_kib = 0L
    (* We'll never have enough memory. *)
    then false else
    if accumulated_wait_time_seconds >= maximum_wait_time_seconds
    (* We've waited long enough. *)
    then false else
      begin
        Thread.delay 1.0;
        wait (accumulated_wait_time_seconds + 1)
      end in
  wait 0


let make ~xc ~xs vm_info uuid =
  let flags = if vm_info.hvm then begin
      let default_flags =
        (if vm_info.hvm then [ Xenctrl.CDF_HVM ] else []) @
        (if (vm_info.hvm && vm_info.hap) then [ Xenctrl.CDF_HAP ] else []) in
      if (List.mem_assoc "hap" vm_info.platformdata) then begin
        let hap = List.assoc "hap" vm_info.platformdata in
        if hap = "false" then begin
          info "VM = %s; Hardware Assisted Paging (HAP) disabled" (Uuid.to_string uuid);
          [ Xenctrl.CDF_HVM ]
        end else if hap = "true" then begin
          info "VM = %s; Hardware Assisted Paging (HAP) will be enabled." (Uuid.to_string uuid);
          [ Xenctrl.CDF_HVM; Xenctrl.CDF_HAP ]
        end else begin
          warn "VM = %s; Unrecognized value platform/hap=\"%s\".  Hardware Assisted Paging will be %s." (Uuid.to_string uuid) hap (if List.mem Xenctrl.CDF_HAP default_flags then "enabled" else "disabled");
          default_flags
        end
      end else begin
        info "VM = %s; Hardware Assisted Paging will be %s. Use platform/hap=(true|false) to override" (Uuid.to_string uuid) (if List.mem Xenctrl.CDF_HAP default_flags then "enabled" else "disabled");
        default_flags
      end
    end else [] in
  let domid = Xenctrl.domain_create xc vm_info.ssidref flags (Uuidm.to_string uuid) in
  let name = if vm_info.name <> "" then vm_info.name else sprintf "Domain-%d" domid in
  try
    let dom_path = xs.Xs.getdomainpath domid in
    let xenops_dom_path = xenops_path_of_domain domid in
    let vm_path = "/vm/" ^ (Uuid.to_string uuid) in
    let vss_path = "/vss/" ^ (Uuid.to_string uuid) in
    let roperm = Xenbus_utils.roperm_for_guest domid in
    let rwperm = Xenbus_utils.rwperm_for_guest domid in
    let zeroperm = Xenbus_utils.rwperm_for_guest 0 in
    debug "VM = %s; creating xenstored tree: %s" (Uuid.to_string uuid) dom_path;

    let create_time = Oclock.gettime Oclock.monotonic in
    Xs.transaction xs (fun t ->
        (* Clear any existing rubbish in xenstored *)
        t.Xst.rm dom_path;
        t.Xst.rm xenops_dom_path;

        t.Xst.mkdirperms dom_path roperm;
        t.Xst.mkdirperms xenops_dom_path zeroperm;

        (* The /vm path needs to be shared over a localhost migrate *)
        let vm_exists = try ignore(t.Xst.read vm_path); true with _ -> false in
        if vm_exists then
          xenstore_iter t (fun d -> t.Xst.setperms d roperm) vm_path
        else begin
          t.Xst.mkdirperms vm_path roperm;
          t.Xst.writev vm_path [
            "uuid", (Uuid.to_string uuid);
            "name", name;
          ];
        end;
        t.Xst.write (Printf.sprintf "%s/domains/%d" vm_path domid) dom_path;
        t.Xst.write (Printf.sprintf "%s/domains/%d/create-time" vm_path domid) (Int64.to_string create_time);

        t.Xst.rm vss_path;
        t.Xst.mkdirperms vss_path rwperm;

        t.Xst.writev dom_path [
          "vm", vm_path;
          "vss", vss_path;
          "name", name;
        ];

        (* create cpu and memory directory with read only perms *)
        List.iter (fun dir ->
            let ent = sprintf "%s/%s" dom_path dir in
            t.Xst.mkdirperms ent roperm
          ) [ "cpu"; "memory" ];

        let mksubdirs base dirs perms =
          List.iter (fun dir ->
              let ent = base ^ "/" ^ dir in
              t.Xst.mkdirperms ent perms
            ) dirs in

        let device_dirs = ["device"; "device/vbd"; "device/vif"] in

        (* create read/write nodes for the guest to use *)
        mksubdirs
          dom_path
          (device_dirs @ [
              "feature";
              "error";
              "drivers";
              "control";
              "attr";
              "data";
              "messages";
              "vm-data";
              "hvmloader";
              "rrd";
            ])
          rwperm;

        (* ...and a few corresponding private nodes for us to use. *)
        mksubdirs xenops_dom_path device_dirs zeroperm;
      );

    xs.Xs.writev dom_path (filtered_xsdata vm_info.xsdata);
    xs.Xs.writev (dom_path ^ "/platform") vm_info.platformdata;

    xs.Xs.writev (dom_path ^ "/bios-strings") vm_info.bios_strings;

    (* If a toolstack sees a domain which it should own in this state then the
       		   domain is not completely setup and should be shutdown. *)
    xs.Xs.write (dom_path ^ "/action-request") "poweroff";

    xs.Xs.write (dom_path ^ "/control/platform-feature-multiprocessor-suspend") "1";

    xs.Xs.write (dom_path ^ "/control/has-vendor-device") (if vm_info.has_vendor_device then "1" else "0");

    (* CA-30811: let the linux guest agent easily determine if this is a fresh domain even if
       		   the domid hasn't changed (consider cross-host migrate) *)
    xs.Xs.write (dom_path ^ "/unique-domain-id") (Uuid.to_string (Uuid.create `V4));

    info "VM = %s; domid = %d" (Uuid.to_string uuid) domid;
    domid
  with e ->
    debug "VM = %s; domid = %d; Caught exception while creating xenstore tree: %s" (Uuid.to_string uuid) domid (Printexc.to_string e);
    raise e

type shutdown_reason = PowerOff | Reboot | Suspend | Crash | Halt | S3Suspend | Unknown of int

(** Strings suitable for putting in the control/shutdown xenstore entry *)
let string_of_shutdown_reason = function
  | PowerOff -> "poweroff"
  | Reboot   -> "reboot"
  | Suspend  -> "suspend"
  | Crash    -> "crash" (* this one makes no sense to send to a guest *)
  | Halt     -> "halt"
  | S3Suspend -> "s3"
  | Unknown x -> sprintf "(unknown %d)" x (* or this one *)

(** Decode the shutdown_reason contained within the dominfo struct *)
let shutdown_reason_of_int = function
  | 0 -> PowerOff
  | 1 -> Reboot
  | 2 -> Suspend
  | 3 -> Crash
  | 4 -> Halt
  | x -> Unknown x

let shutdown_to_xc_shutdown = function
  | PowerOff -> Xenctrl.Poweroff
  | Reboot   -> Xenctrl.Reboot
  | Suspend  -> Xenctrl.Suspend
  | Crash    -> Xenctrl.Crash
  | Halt     -> Xenctrl.Poweroff
  | S3Suspend -> raise (Invalid_argument "unknown")
  | Unknown _-> raise (Invalid_argument "unknown")

(** Immediately change the domain state to shutdown *)
let hard_shutdown ~xc domid req =
  Xenctrl.domain_shutdown xc domid (shutdown_to_xc_shutdown req)

(** Return the path in xenstore watched by the PV shutdown driver *)
let control_shutdown ~xs domid = xs.Xs.getdomainpath domid ^ "/control/shutdown"

(** Raised if a domain has vanished *)
exception Domain_does_not_exist

(** Request a shutdown, return without waiting for acknowledgement *)
let shutdown ~xc ~xs domid req =
  let uuid = get_uuid ~xc domid in
  debug "VM = %s; domid = %d; Requesting domain %s" (Uuid.to_string uuid) domid (string_of_shutdown_reason req);

  let reason = string_of_shutdown_reason req in
  let path = control_shutdown ~xs domid in
  let domainpath = xs.Xs.getdomainpath domid in
  Xs.transaction xs
    (fun t ->
       (* Fail if the directory has been deleted *)
       let domain_exists = try ignore (t.Xst.read domainpath); true with Xs_protocol.Enoent _ -> false in
       if not domain_exists then raise Domain_does_not_exist;
       (* Delete the node if it already exists. NB: the guest may well still shutdown for the
          				previous reason... we only want to give it a kick again just in case. *)
       (try t.Xst.rm path with _ -> ());
       t.Xst.write path reason
    )

(** If domain is PV, signal it to shutdown. If the PV domain fails to respond then throw a Watch.Timeout exception.
    	All other exceptions imply the domain has disappeared. *)
let shutdown_wait_for_ack (t: Xenops_task.task_handle) ~timeout ~xc ~xs domid req =
  let di = Xenctrl.domain_getinfo xc domid in
  let uuid = get_uuid ~xc domid in
  if (di.Xenctrl.hvm_guest) && not (Xenctrl.hvm_check_pvdriver xc domid) then begin
    debug "VM = %s; domid = %d; HVM guest without PV drivers: not expecting any acknowledgement" (Uuid.to_string uuid) domid;
    Xenctrl.domain_shutdown xc domid (shutdown_to_xc_shutdown req)
  end else begin
    debug "VM = %s; domid = %d; Waiting for PV domain to acknowledge shutdown request" (Uuid.to_string uuid) domid;
    let path = control_shutdown ~xs domid in
    let cancel = Domain domid in
    if cancellable_watch cancel [ Watch.value_to_become path ""] [ Watch.key_to_disappear path ] t ~xs ~timeout ()
    then info "VM = %s; domid = %d; Domain acknowledged shutdown request" (Uuid.to_string uuid) domid
    else debug "VM = %s; domid = %d; Domain disappeared" (Uuid.to_string uuid) domid
  end

let sysrq ~xs domid key =
  let path = xs.Xs.getdomainpath domid ^ "/control/sysrq" in
  xs.Xs.write path (String.make 1 key)

let destroy (task: Xenops_task.task_handle) ~xc ~xs ~qemu_domid ~dm domid =
  let dom_path = xs.Xs.getdomainpath domid in
  let xenops_dom_path = xenops_path_of_domain domid in
  let uuid = get_uuid ~xc domid in

  (* Move this out of the way immediately *)
  let s = Printf.sprintf "deadbeef-dead-beef-dead-beef0000%04x" domid in
  Xenctrl.domain_sethandle xc domid s;

  (* These are the devices with a frontend in [domid] and a well-formed backend
     	   in some other domain *)
  let all_devices = list_frontends ~xs domid in

  debug "VM = %s; domid = %d; Domain.destroy: all known devices = [ %a ]"
    (Uuid.to_string uuid) domid
    (fun () -> String.concat "; ")
    (List.map string_of_device all_devices);

  (* Any other domains with the same UUID as the one we are destroying.
     	 * There can be one during a localhost migration. *)
  let other_domains = Xenops_helpers.domains_of_uuid ~xc uuid in
  debug "VM = %s; domid = %d; Domain.destroy: other domains with the same UUID = [ %a ]"
    (Uuid.to_string uuid) domid
    (fun () -> String.concat "; ")
    (List.map (fun x -> string_of_int x.Xenctrl.domid) other_domains);

  (* reset PCI devices before xc.domain_destroy otherwise we lot all IOMMU mapping *)
  let _, all_pci_devices = List.split (Device.PCI.list xs domid) in
  List.iter
    (fun pcidev ->
       let open Xenops_interface.Pci in
       log_exn_continue
         ("Deassign PCI device " ^ string_of_address pcidev)
         (fun () ->
            Xenctrl.domain_deassign_device xc domid
              (pcidev.domain, pcidev.bus, pcidev.dev, pcidev.fn)) ())
    all_pci_devices;
  List.iter
    (fun pcidev ->
       let open Xenops_interface.Pci in
       log_exn_continue
         ("Reset PCI device " ^ string_of_address pcidev)
         (fun () -> Device.PCI.reset ~xs pcidev) ())
    all_pci_devices;

  (* Now we should kill the domain itself *)
  debug "VM = %s; domid = %d; Domain.destroy calling Xenctrl.domain_destroy" (Uuid.to_string uuid) domid;
  log_exn_continue "Xenctrl.domain_destroy" (Xenctrl.domain_destroy xc) domid;

  log_exn_continue "Error stoping device-model, already dead ?"
    (fun () -> Device.Dm.stop ~xs ~qemu_domid ~dm domid) ();
  log_exn_continue "Error stoping vncterm, already dead ?"
    (fun () -> Device.PV_Vnc.stop ~xs domid) ();

  (* Forcibly shutdown every backend *)
  List.iter
    (fun device ->
       try
         Device.hard_shutdown task ~xs device
       with e ->
         (* If this fails we may have a resource leak. We should prevent
            		  this from happening! *)
         error "VM = %s; domid = %d; Caught exception %s while destroying device %s"
           (Uuid.to_string uuid) domid
           (Printexc.to_string e) (string_of_device device);
         (* Keep going on a best-effort basis *)
    ) all_devices;

  (* For each device which has a hotplug entry, perform the cleanup. Even if one
     	   fails, try to cleanup the rest anyway.*)
  let released = ref [] in
  List.iter (fun x ->
      log_exn_continue ("waiting for hotplug for " ^ (string_of_device x))
        (fun () ->
           Hotplug.release task ~xc ~xs x; released := x :: !released
        ) ()
    ) all_devices;

  (* If we fail to release a device we leak resources. If we are to tolerate this
     	   then we need an async cleanup thread. *)
  let failed_devices = List.filter (fun x -> not(List.mem x !released)) all_devices in
  List.iter (fun dev ->
      error "VM = %s; domid = %d; Domain.destroy failed to release device: %s"
        (Uuid.to_string uuid) domid
        (string_of_device dev)) failed_devices;

  (* Remove our reference to the /vm/<uuid> directory *)
  let vm_path = try Some (xs.Xs.read (dom_path ^ "/vm")) with _ -> None in
  Opt.iter (fun vm_path -> log_exn_rm ~xs (vm_path ^ "/domains/" ^ (string_of_int domid))) vm_path;

  (* Delete /local/domain/<domid>, /xenops/domain/<domid>
     	 * and all the backend device paths *)
  debug "VM = %s; domid = %d; xenstore-rm %s" (Uuid.to_string uuid) domid dom_path;
  xs.Xs.rm dom_path;
  xs.Xs.rm xenops_dom_path;
  debug "VM = %s; domid = %d; deleting backends" (Uuid.to_string uuid) domid;
  let backend_path = xs.Xs.getdomainpath 0 ^ "/backend" in
  let all_backend_types = try xs.Xs.directory backend_path with _ -> [] in
  List.iter (fun ty -> log_exn_rm ~xs (Printf.sprintf "%s/%s/%d" backend_path ty domid)) all_backend_types;

  (* If all devices were properly un-hotplugged, then zap the private tree in
     	 * xenstore.  If there was some error leave the tree for debugging / async
     	 * cleanup.  If there are any remaining domains with the same UUID, then
     	 * zap only the hotplug tree for the destroyed domain. *)
  if failed_devices = [] then begin
    if List.length other_domains < 1 then
      log_exn_rm ~xs (Device_common.get_private_path_by_uuid uuid)
    else
      log_exn_rm ~xs (Hotplug.get_hotplug_base_by_uuid uuid domid)
  end;
  (* Also zap any remaining cancellation paths in xenstore *)
  Cancel_utils.cleanup_for_domain ~xs domid;

  (* Block waiting for the dying domain to disappear: aim is to catch shutdown errors early*)
  let still_exists () =
    try
      let _ = Xenctrl.domain_getinfo xc domid in
      debug "VM = %s; domid = %d; Domain still exist, waiting for it to disappear." (Uuid.to_string uuid) domid;
      true
    with
    | Xenctrl.Error err ->
      debug "VM = %s; domid = %d; Domain nolonger exists (%s)" (Uuid.to_string uuid) domid err;
      false
    | e ->
      error "VM = %s; domid = %d; Xenctrl.domain_getinfo threw: %s" (Uuid.to_string uuid) domid (Printexc.to_string e);
      raise e in
  let start = Unix.gettimeofday () in
  let timeout = 60. in
  while still_exists () && (Unix.gettimeofday () -. start < timeout) do
    Thread.delay 5.
  done;
  if still_exists () then begin
    error "VM = %s; domid = %d; Domain stuck in dying state after 30s; resetting UUID to %s. This probably indicates a backend driver bug." (Uuid.to_string uuid) domid s;
    raise (Domain_stuck_in_dying_state domid)
  end


let pause ~xc domid =
  Xenctrl.domain_pause xc domid

let unpause ~xc domid =
  Xenctrl.domain_unpause xc domid

let set_action_request ~xs domid x =
  let path = xs.Xs.getdomainpath domid ^ "/action-request" in
  match x with
  | None -> xs.Xs.rm path
  | Some v -> xs.Xs.write path v

let get_action_request ~xs domid =
  let path = xs.Xs.getdomainpath domid ^ "/action-request" in
  try
    Some (xs.Xs.read path)
  with Xs_protocol.Enoent _ -> None

let maybe_ca_140252_workaround ~xc ~vcpus domid =
  if !Xenopsd.ca_140252_workaround then
    debug "Allocating %d I/O req evtchns in advance for device model" vcpus;
  for i = 1 to vcpus do
    ignore_int (Xenctrl.evtchn_alloc_unbound xc domid 0)
  done

(** create store and console channels *)
let create_channels ~xc uuid domid =
  let store = Xenctrl.evtchn_alloc_unbound xc domid 0 in
  let console = Xenctrl.evtchn_alloc_unbound xc domid 0 in
  debug "VM = %s; domid = %d; store evtchn = %d; console evtchn = %d" (Uuid.to_string uuid) domid store console;
  store, console

let build_pre ~xc ~xs ~vcpus ~xen_max_mib ~shadow_mib ~required_host_free_mib domid =
  let uuid = get_uuid ~xc domid in
  debug "VM = %s; domid = %d; waiting for %Ld MiB of free host memory" (Uuid.to_string uuid) domid required_host_free_mib;
  (* CA-39743: Wait, if necessary, for the Xen scrubber to catch up. *)
  if not(wait_xen_free_mem ~xc (Memory.kib_of_mib required_host_free_mib)) then begin
    error "VM = %s; domid = %d; Failed waiting for Xen to free %Ld MiB"
      (Uuid.to_string uuid) domid required_host_free_mib;
    raise (Not_enough_memory (Memory.bytes_of_mib required_host_free_mib))
  end;

  let shadow_mib = Int64.to_int shadow_mib in

  let dom_path = xs.Xs.getdomainpath domid in
  let read_platform flag = xs.Xs.read (dom_path ^ "/platform/" ^ flag) in
  let int_platform_flag flag = try Some (int_of_string (read_platform flag)) with _ -> None in
  let timer_mode = int_platform_flag "timer_mode" in

  let log_reraise call_str f =
    debug "VM = %s; domid = %d; %s" (Uuid.to_string uuid) domid call_str;
    try ignore (f ())
    with e ->
      let err_msg =
        Printf.sprintf "Calling '%s' failed: %s" call_str (Printexc.to_string e)
      in
      error "VM = %s; domid = %d; %s" (Uuid.to_string uuid) domid err_msg;
      raise (Domain_build_pre_failed err_msg)
  in

  maybe (fun mode ->
      log_reraise (Printf.sprintf "domain_set_timer_mode %d" mode) (fun () ->
          Xenctrlext.domain_set_timer_mode xc domid mode
        )
    ) timer_mode;

  log_reraise (Printf.sprintf "domain_max_vcpus %d" vcpus) (fun () ->
      Xenctrl.domain_max_vcpus xc domid vcpus
    );

  if not (Xenctrl.((domain_getinfo xc domid).hvm_guest)) then begin
    let kib = Memory.kib_of_mib xen_max_mib in
    log_reraise (Printf.sprintf "domain_set_memmap_limit %Ld KiB" kib) (fun () ->
        Xenctrl.domain_set_memmap_limit xc domid kib
      )
  end;

  log_reraise (Printf.sprintf "shadow_allocation_set %d MiB" shadow_mib) (fun () ->
      Xenctrl.shadow_allocation_set xc domid shadow_mib
    );

  create_channels ~xc uuid domid

let resume_post ~xc ~xs domid =
  let uuid = get_uuid ~xc domid in
  let dom_path = xs.Xs.getdomainpath domid in
  let store_mfn_s = xs.Xs.read (dom_path ^ "/store/ring-ref") in
  let store_mfn = Nativeint.of_string store_mfn_s in
  let store_port = int_of_string (xs.Xs.read (dom_path ^ "/store/port")) in
  debug "VM = %s; domid = %d; @introduceDomain" (Uuid.to_string uuid) domid;
  xs.Xs.introduce domid store_mfn store_port

(* puts value in store after the domain build succeed *)
let build_post ~xc ~xs ~vcpus ~static_max_mib ~target_mib domid
    store_mfn store_port ents vments =
  let uuid = get_uuid ~xc domid in
  let dom_path = xs.Xs.getdomainpath domid in
  (* Unit conversion. *)
  let static_max_kib = Memory.kib_of_mib static_max_mib in
  let target_kib = Memory.kib_of_mib target_mib in
  (* expand local stuff with common values *)
  let ents =
    [ ("memory/static-max", Int64.to_string static_max_kib);
      ("memory/target", Int64.to_string target_kib);
      ("domid", string_of_int domid);
      ("store/port", string_of_int store_port);
      ("store/ring-ref", sprintf "%nu" store_mfn);
    ] @ ents in
  Xs.transaction xs (fun t -> t.Xst.writev dom_path ents);
  if vments <> [] then (
    let vm_path = xs.Xs.read (dom_path ^ "/vm") in
    Xs.transaction xs (fun t -> t.Xst.writev vm_path vments)
  );
  debug "VM = %s; domid = %d; @introduceDomain" (Uuid.to_string uuid) domid;
  xs.Xs.introduce domid store_mfn store_port

(** build a linux type of domain *)
let build_linux (task: Xenops_task.task_handle) ~xc ~xs ~store_domid ~console_domid ~static_max_kib ~target_kib ~kernel ~cmdline ~ramdisk
    ~vcpus ~extras xenguest_path domid force =
  let uuid = get_uuid ~xc domid in
  assert_file_is_readable kernel;
  maybe assert_file_is_readable ramdisk;

  (* Convert memory configuration values into the correct units. *)
  let static_max_mib = Memory.mib_of_kib_used static_max_kib in
  let target_mib     = Memory.mib_of_kib_used target_kib in

  (* Sanity check. *)
  assert (target_mib <= static_max_mib);

  (* Adapt memory configuration values for Xen and the domain builder. *)
  let video_mib = 0 in
  let build_max_mib =
    Memory.Linux.build_max_mib static_max_mib video_mib in
  let build_start_mib =
    Memory.Linux.build_start_mib target_mib video_mib in
  let xen_max_mib =
    Memory.Linux.xen_max_mib static_max_mib in
  let shadow_multiplier =
    Memory.Linux.shadow_multiplier_default in
  let shadow_mib =
    Memory.Linux.shadow_mib static_max_mib vcpus shadow_multiplier in
  let required_host_free_mib =
    Memory.Linux.footprint_mib target_mib static_max_mib vcpus shadow_multiplier in

  let store_port, console_port = build_pre ~xc ~xs
      ~xen_max_mib ~shadow_mib ~required_host_free_mib ~vcpus domid in
  let force_arg = if force then ["--force"] else [] in
  let line = Emu_manager.with_connection task xenguest_path domid
      ([
        "-mode"; "linux_build";
        "-domid"; string_of_int domid;
        "-mem_max_mib"; Int64.to_string build_max_mib;
        "-mem_start_mib"; Int64.to_string build_start_mib;
        "-image"; kernel;
        "-ramdisk"; (match ramdisk with Some x -> x | None -> "");
        "-cmdline"; cmdline;
        "-features"; "";
        "-flags"; "0";
        "-store_port"; string_of_int store_port;
        "-store_domid"; string_of_int store_domid;
        "-console_port"; string_of_int console_port;
        "-console_domid"; string_of_int console_domid;
        "-fork"; "true";
      ] @ force_arg @ extras) []
      Emu_manager.receive_success in

  let store_mfn, console_mfn, protocol =
    (* the "protocol" (ie the domU architecture) was only needed for very
       		   old kernels which had bugs preventing them succesfully autonegotiating
       		   the 64-bit version of the protocol. If we don't know the architecture,
       		   it should be safe to assume "native" i.e. let the domU do its thing. *)
    match Stdext.Xstringext.String.split ' ' line with
    | [ store_mfn; console_mfn; protocol ] ->
      debug "VM = %s; domid = %d; store_mfn = %s; console_mfn = %s; protocol = %s" (Uuid.to_string uuid) domid store_mfn console_mfn protocol;
      Nativeint.of_string store_mfn, Nativeint.of_string console_mfn, protocol
    | [ store_mfn; console_mfn ] ->
      debug "VM = %s; domid = %d; store_mfn = %s; console_mfn = %s; protocol unavailable, assuming 'native'" (Uuid.to_string uuid) domid store_mfn console_mfn;
      Nativeint.of_string store_mfn, Nativeint.of_string console_mfn, ""
    | _ ->
      error "VM = %s; domid = %d; domain builder returned invalid result: \"%s\"" (Uuid.to_string uuid) domid line;
      raise Domain_build_failed in

  let local_stuff = [
    "serial/0/limit",    string_of_int 65536;
    "console/port",      string_of_int console_port;
    "console/ring-ref",  sprintf "%nu" console_mfn;
    "console/limit",     string_of_int 65536;
  ] in
  let vm_stuff = [] in
  build_post ~xc ~xs ~vcpus ~target_mib ~static_max_mib
    domid store_mfn store_port local_stuff vm_stuff;
  match protocol with
  | "x86_32-abi" -> Arch_X32
  | "x86_64-abi" -> Arch_X64
  | _            -> Arch_native

(** build hvm type of domain *)
let build_hvm (task: Xenops_task.task_handle) ~xc ~xs ~store_domid ~console_domid ~static_max_kib ~target_kib ~shadow_multiplier ~vcpus
    ~kernel ~timeoffset ~video_mib ~extras xenguest_path domid force =
  let uuid = get_uuid ~xc domid in
  assert_file_is_readable kernel;

  (* Convert memory configuration values into the correct units. *)
  let static_max_mib = Memory.mib_of_kib_used static_max_kib in
  let target_mib     = Memory.mib_of_kib_used target_kib in

  (* Sanity check. *)
  assert (target_mib <= static_max_mib);

  (* Adapt memory configuration values for Xen and the domain builder. *)
  let build_max_mib =
    Memory.HVM.build_max_mib static_max_mib video_mib in
  let build_start_mib =
    Memory.HVM.build_start_mib target_mib video_mib in
  let xen_max_mib =
    Memory.HVM.xen_max_mib static_max_mib in
  let shadow_mib =
    Memory.HVM.shadow_mib static_max_mib vcpus shadow_multiplier in
  let required_host_free_mib =
    Memory.HVM.footprint_mib target_mib static_max_mib vcpus shadow_multiplier in

  maybe_ca_140252_workaround ~xc ~vcpus domid;
  let store_port, console_port = build_pre ~xc ~xs
      ~xen_max_mib ~shadow_mib ~required_host_free_mib ~vcpus domid in
  let force_arg = if force then ["--force"] else [] in
  let line = Emu_manager.with_connection task xenguest_path domid
      ([
        "-mode"; "hvm_build";
        "-domid"; string_of_int domid;
        "-store_port"; string_of_int store_port;
        "-store_domid"; string_of_int store_domid;
        "-console_port"; string_of_int console_port;
        "-console_domid"; string_of_int console_domid;
        "-image"; kernel;
        "-mem_max_mib"; Int64.to_string build_max_mib;
        "-mem_start_mib"; Int64.to_string build_start_mib;
        "-fork"; "true";
      ] @ force_arg @ extras) [] Emu_manager.receive_success in

  (* XXX: domain builder will reduce our shadow allocation under our feet.
     	   Detect this and override. *)
  let requested_shadow_mib = Int64.to_int shadow_mib in
  let actual_shadow_mib = Xenctrl.shadow_allocation_get xc domid in
  if actual_shadow_mib < requested_shadow_mib then begin
    warn
      "VM = %s; domid = %d; HVM domain builder reduced our \
       			shadow memory from %d to %d MiB; reverting"
      (Uuid.to_string uuid) domid
      requested_shadow_mib actual_shadow_mib;
    Xenctrl.shadow_allocation_set xc domid requested_shadow_mib;
    let shadow = Xenctrl.shadow_allocation_get xc domid in
    debug "VM = %s; domid = %d; Domain now has %d MiB of shadow"
      (Uuid.to_string uuid) domid shadow;
  end;

  let store_mfn, console_mfn =
    match Stdext.Xstringext.String.split ' ' line with
    | [ store_mfn; console_mfn] ->
      debug "VM = %s; domid = %d; store_mfn = %s; console_mfn = %s" (Uuid.to_string uuid) domid store_mfn console_mfn;
      Nativeint.of_string store_mfn, Nativeint.of_string console_mfn
    | _ ->
      error "VM = %s; domid = %d; domain builder returned invalid result: \"%s\"" (Uuid.to_string uuid) domid line;
      raise Domain_build_failed in

  let local_stuff = [
    "serial/0/limit",    string_of_int 65536;
    "console/port",      string_of_int console_port;
    "console/ring-ref",  sprintf "%nu" console_mfn;
    "console/limit",     string_of_int 65536;
  ] in
(*
	let store_mfn =
		try Nativeint.of_string line
		with _ -> raise Domain_build_failed in
*)
  let vm_stuff = [
    "rtc/timeoffset",    timeoffset;
  ] in

  build_post ~xc ~xs ~vcpus ~target_mib ~static_max_mib
    domid store_mfn store_port local_stuff vm_stuff;

  Arch_HVM

let build (task: Xenops_task.task_handle) ~xc ~xs ~store_domid ~console_domid ~timeoffset ~extras info xenguest_path domid force =
  match info.priv with
  | BuildHVM hvminfo ->
    build_hvm task ~xc ~xs ~store_domid ~console_domid ~static_max_kib:info.memory_max ~target_kib:info.memory_target
      ~shadow_multiplier:hvminfo.shadow_multiplier ~vcpus:info.vcpus
      ~kernel:info.kernel ~timeoffset ~video_mib:hvminfo.video_mib ~extras xenguest_path domid force
  | BuildPV pvinfo   ->
    build_linux task ~xc ~xs ~store_domid ~console_domid ~static_max_kib:info.memory_max ~target_kib:info.memory_target
      ~kernel:info.kernel ~cmdline:pvinfo.cmdline ~ramdisk:pvinfo.ramdisk
      ~vcpus:info.vcpus ~extras xenguest_path domid force

let resume (task: Xenops_task.task_handle) ~xc ~xs ~hvm ~cooperative ~qemu_domid domid =
  if not cooperative
  then failwith "Domain.resume works only for collaborative domains";
  Xenctrl.domain_resume_fast xc domid;
  resume_post ~xc	~xs domid;
  if hvm then Device.Dm.resume task ~xs ~qemu_domid domid

type suspend_flag = Live | Debug

module type SUSPEND_RESTORE = sig
  val restore:
    Xenops_task.task_handle
    -> xc: Xenctrl.handle
    -> xs: Xenstore.Xs.xsh
    -> store_domid:int
    -> console_domid:int
    -> no_incr_generationid:bool
    -> timeoffset:string
    -> extras:string list
    -> build_info
    -> manager_path: string
    -> domid
    -> Unix.file_descr
    -> Unix.file_descr option
    -> unit
  val suspend:
    Xenops_task.task_handle
    -> xc: Xenctrl.handle
    -> xs: Xenstore.Xs.xsh
    -> hvm: bool
    -> dm:Device.Profile.t
    -> manager_path: string
    -> string
    -> domid
    -> Unix.file_descr
    -> Unix.file_descr option
    -> suspend_flag list
    -> ?progress_callback: (float -> unit)
    -> qemu_domid: int
    -> (unit -> 'a)
    -> unit
end

module Suspend_restore_emu_manager : SUSPEND_RESTORE = struct

  let with_emu_manager_restore (task: Xenops_task.task_handle) ~hvm ~store_port ~console_port ~extras manager_path domid uuid main_fd vgpu_fd f =
    let fd_uuid = Uuid.(to_string (create `V4)) in
    let vgpu_args, vgpu_cmdline =
      match vgpu_fd with
      | Some fd when fd = main_fd ->
        [fd_uuid, main_fd],
        ["-dm"; "vgpu:" ^ fd_uuid]
      | Some fd ->
        let vgpu_fd_uuid = Uuid.(to_string (create `V4)) in
        [vgpu_fd_uuid, fd],
        ["-dm"; "vgpu:" ^ vgpu_fd_uuid]
      | None -> [], []
    in
    let fds = [ fd_uuid, main_fd ] @ vgpu_args in
    let args = [
      "-mode"; if hvm then "hvm_restore" else "restore";
      "-domid"; string_of_int domid;
      "-fd"; fd_uuid;
      "-store_port"; string_of_int store_port;
      "-console_port"; string_of_int console_port;
      "-fork"; "true";
    ] @ extras @ vgpu_cmdline
    in
    Emu_manager.with_connection task manager_path domid args fds f

  let restore_libxc_record cnx domid uuid =
    let open Emu_manager in
    send_restore cnx Xenguest;
    let res = receive_success cnx in
    match parse_result res with
    | Xenguest_result (store, console) ->
      debug "VM = %s; domid = %d; store_mfn = %nd; console_mfn = %nd" (Uuid.to_string uuid) domid store console;
      store, console
    | _                  ->
      error "VM = %s; domid = %d; domain builder returned invalid result: \"%s\"" (Uuid.to_string uuid) domid res;
      raise Domain_restore_failed

  let consume_qemu_record fd limit domid uuid =
    if limit > 1_048_576L then begin (* 1MB *)
      error "VM = %s; domid = %d; QEMU record length in header too large (%Ld bytes)"
        (Uuid.to_string uuid) domid limit;
      raise Suspend_image_failure
    end;
    let file = sprintf qemu_restore_path domid in
    let fd2 = Unix.openfile file
        [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC; ] 0o640
    in
    finally (fun () ->
        debug "VM = %s; domid = %d; reading %Ld bytes from %s"
          (Uuid.to_string uuid) domid limit file;
        let bytes =
          try
            Unixext.copy_file ~limit fd fd2
          with Unix.Unix_error (e, s1, s2) ->
            error "VM = %s; domid = %d; %s, %s, %s" (Uuid.to_string uuid) domid (Unix.error_message e) s1 s2;
            Unixext.unlink_safe file;
            raise Suspend_image_failure
        in
        if bytes <> limit
        then begin
          error "VM = %s; domid = %d; qemu save file was truncated"
            (Uuid.to_string uuid) domid;
          raise Domain_restore_truncated_hvmstate
        end
      ) (fun () -> Unix.close fd2)

  let restore_common (task: Xenops_task.task_handle) ~xc ~xs ~hvm ~store_port ~store_domid
      ~console_port ~console_domid ~no_incr_generationid ~vcpus ~extras
      manager_path domid main_fd vgpu_fd =

    let module DD = Debug.Make(struct let name = "mig64" end) in
    let open DD in
    let uuid = get_uuid ~xc domid in
    let open Suspend_image in
    match read_save_signature main_fd with
    | `Ok Legacy ->
      debug "Detected legacy suspend image! Piping through conversion tool.";
      let (store_mfn, console_mfn) =
        begin match
            with_conversion_script task "Emu_manager" hvm main_fd (fun pipe_r ->
                with_emu_manager_restore task ~hvm ~store_port ~console_port ~extras manager_path domid uuid main_fd vgpu_fd (fun cnx ->
                    restore_libxc_record cnx domid uuid
                  )
              )
          with
          | `Ok (s, c) -> (s, c)
          | `Error e ->
            error "Caught error when using converison script: %s" (Printexc.to_string e);
            Xenops_task.cancel task;
            raise e
        end
      in
      (* Consume the (legacy) QEMU Record *)
      if hvm
      then begin
        debug "Reading legacy (Xenops-level) QEMU record signature";
        let length = begin match (read_legacy_qemu_header main_fd) with
          | `Ok length -> length
          | `Error e ->
            error "VM = %s; domid = %d; Error reading QEMU signature: %s"
              (Uuid.to_string uuid) domid e;
            raise Suspend_image_failure
        end in
        debug "Consuming QEMU record into file";
        consume_qemu_record main_fd length domid uuid
      end;
      store_mfn, console_mfn
    | `Ok Structured ->
      let open Suspend_image.M in
      let open Emu_manager in
      let fds =
        match vgpu_fd with
        | Some fd when fd <> main_fd -> [main_fd; fd]
        | _ -> [main_fd]
      in
      with_emu_manager_restore task ~hvm ~store_port ~console_port ~extras manager_path domid uuid main_fd vgpu_fd (fun cnx ->
          (* Maintain a list of results returned by emu-manager that are expected
           * by the reader threads. Contains the emu for which a result is wanted
           * plus an event channel for waking up the reader once the result is in. *)
          let thread_requests = ref [] in
          let thread_requests_m = Mutex.create () in
          let emu_manager_send_m = Mutex.create () in
          let restore_and_wait emu =
            (* Called by a reader thread to send a "restore" request to emu-manager
             * and wait for the result. Results from emu-manager come in on the main
             * thread, and collected there. All we need to do here is block until
             * this has happened before sending the next request to emu-manager. *)
            let wakeup = Event.new_channel () in
            Mutex.execute thread_requests_m (fun () ->
                thread_requests := (emu, wakeup) :: !thread_requests
              );
            wrap (fun () -> Mutex.execute emu_manager_send_m (fun () ->
                send_restore cnx emu
              )) >>= fun () ->
            debug "Sent restore:%s to emu-manager. Waiting for result..." (string_of_emu emu);
            (* Block until woken up by the main thread once the result has been received. *)
            Event.receive wakeup |> Event.sync;
            Mutex.execute thread_requests_m (fun () ->
                thread_requests := List.remove_assoc emu !thread_requests
              );
            return ()
          in
          let rec process_header fd res =
            (* Read and process the next bit from the suspend-image fd. *)
            debug "Reading next header... (fd=%d)" (Obj.magic fd);
            read_header fd >>= function
            | Xenops, len ->
              debug "Read Xenops record header (length=%Ld)" len;
              let rec_str = Io.read fd (Io.int_of_int64_exn len) in
              debug "Read Xenops record contents";
              Xenops_record.of_string rec_str >>= fun (_ : Xenops_record.t) ->
              debug "Validated Xenops record contents";
              process_header fd res
            | Libxc, _ ->
              debug "Read Libxc record header";
              restore_and_wait Xenguest >>= fun () ->
              debug "Restored Libxc state";
              process_header fd res
            | Libxc_legacy, _ ->
              debug "Read Libxc_legacy record header";
              restore_and_wait Xenguest >>= fun () ->
              debug "Restored Libxc state";
              process_header fd res
            | Qemu_trad, len ->
              debug "Read Qemu_trad header (length=%Ld)" len;
              consume_qemu_record fd len domid uuid;
              process_header fd res
            | Demu, _ ->
              debug "Read DEMU header";
              restore_and_wait Vgpu >>= fun () ->
              debug "Restored DEMU state";
              process_header fd res
            | End_of_image, _ ->
              debug "Read suspend image footer";
              res
            | _ -> `Error Suspend_image_failure
          in
          let handle_results () =
            (* Wait for results coming in from emu-manager, and match them
             * up with requests from the reader threads.
             * Emu-manager exits when it is done, so we stop when receiving
             * an EOF on the control channel. *)
            let rec loop results =
              try
                debug "Waiting for response from emu-manager";
                return (receive_success cnx) >>= fun response ->
                debug "Received response from emu-manager: %s" response;
                wrap (fun () -> parse_result response) >>= fun result ->
                let emu = emu_of_result result in
                (* Wake up the reader that has requested a result for this emu *)
                if List.mem_assoc emu !thread_requests then begin
                  let wakeup = List.assoc emu !thread_requests in
                  Event.send wakeup () |> Event.sync;
                  loop (result :: results)
                end else begin
                  error "Received unexpected response from emu-manager";
                  `Error Domain_restore_failed
                end
              with End_of_file ->
                debug "Finished emu-manager result processing";
                return results
            in
            loop [] >>= fun results ->
            (* We are only really interested in the result from xenguest *)
            List.fold_left (function acc -> function
                | Xenguest_result (store, console) -> return (Some (store, console))
                | _ -> acc
              ) (return None) results
          in
          let start_reader_thread fd =
            (* Start a reader thread on the given fd. Add a channel back to the
             * main thread for status reporting *)
            debug "Starting reader thread (fd=%d)" (Obj.magic fd);
            let ch = Event.new_channel () in
            let th = Thread.create (fun () ->
                wrap_exn (fun () -> process_header fd (return ()))
                |> Event.send ch
                |> Event.sync
              ) () in
            th, ch
          in
          let receive_thread_status threads_and_channels =
            (* Receive the status from all reader threads and let them exit *)
            fold (fun (th, ch) _ ->
                let status = Event.receive ch |> Event.sync in
                Thread.join th;
                status
              ) threads_and_channels () >>= fun () ->
            debug "Reader threads completed successfully";
            return ()
          in
          let res =
            (* Start a reader thread on each fd *)
            let threads_and_channels = List.map start_reader_thread fds in
            (* Handle results returned by emu-manager *)
            handle_results () >>= fun result ->
            (* Wait for reader threads to complete *)
            receive_thread_status threads_and_channels >>= fun () ->
            (* And we are done! *)
            return result
          in
          begin match res with
            | `Ok (Some (store_mfn, console_mfn)) ->
              debug "VM = %s; domid = %d; store_mfn = %nd; console_mfn = %nd"
                (Uuid.to_string uuid) domid store_mfn console_mfn;
              store_mfn, console_mfn
            | `Ok None -> failwith "Well formed, but useless stream"
            | `Error e -> raise e
          end
        )
    | `Error e ->
      error "VM = %s; domid = %d; Error reading save signature: %s" (Uuid.to_string uuid) domid e;
      raise Suspend_image_failure

  let pv_restore (task: Xenops_task.task_handle) ~xc ~xs ~store_domid ~console_domid
      ~no_incr_generationid ~static_max_kib ~target_kib ~vcpus ~extras
      manager_path domid fd vgpu_fd =

    (* Convert memory configuration values into the correct units. *)
    let static_max_mib = Memory.mib_of_kib_used static_max_kib in
    let target_mib     = Memory.mib_of_kib_used target_kib in

    (* Sanity check. *)
    assert (target_mib <= static_max_mib);

    (* Adapt memory configuration values for Xen and the domain builder. *)
    let xen_max_mib =
      Memory.Linux.xen_max_mib static_max_mib in
    let shadow_multiplier =
      Memory.Linux.shadow_multiplier_default in
    let shadow_mib =
      Memory.Linux.shadow_mib static_max_mib vcpus shadow_multiplier in
    let required_host_free_mib =
      Memory.Linux.footprint_mib target_mib static_max_mib vcpus shadow_multiplier in

    let store_port, console_port = build_pre ~xc ~xs
        ~xen_max_mib ~shadow_mib ~required_host_free_mib ~vcpus domid in

    let store_mfn, console_mfn = restore_common task ~xc ~xs ~hvm:false
        ~store_port ~store_domid
        ~console_port ~console_domid
        ~no_incr_generationid
        ~vcpus ~extras manager_path domid fd vgpu_fd in

    let local_stuff = [
      "serial/0/limit",    string_of_int 65536;
      "console/port",     string_of_int console_port;
      "console/ring-ref", sprintf "%nu" console_mfn;
      "console/limit",    string_of_int 65536;
    ] in
    let vm_stuff = [] in
    build_post ~xc ~xs ~vcpus ~target_mib ~static_max_mib
      domid store_mfn store_port local_stuff vm_stuff

  let hvm_restore (task: Xenops_task.task_handle) ~xc ~xs ~store_domid ~console_domid
      ~no_incr_generationid ~static_max_kib ~target_kib ~shadow_multiplier ~vcpus
      ~timeoffset ~extras manager_path domid fd vgpu_fd =

    (* Convert memory configuration values into the correct units. *)
    let static_max_mib = Memory.mib_of_kib_used static_max_kib in
    let target_mib     = Memory.mib_of_kib_used target_kib in

    (* Sanity check. *)
    assert (target_mib <= static_max_mib);

    (* Adapt memory configuration values for Xen and the domain builder. *)
    let xen_max_mib =
      Memory.HVM.xen_max_mib static_max_mib in
    let shadow_mib =
      Memory.HVM.shadow_mib static_max_mib vcpus shadow_multiplier in
    let required_host_free_mib =
      Memory.HVM.footprint_mib target_mib static_max_mib vcpus shadow_multiplier in

    maybe_ca_140252_workaround ~xc ~vcpus domid;
    let store_port, console_port = build_pre ~xc ~xs
        ~xen_max_mib ~shadow_mib ~required_host_free_mib ~vcpus domid in

    let store_mfn, console_mfn = restore_common task ~xc ~xs ~hvm:true
        ~store_port ~store_domid
        ~console_port ~console_domid
        ~no_incr_generationid
        ~vcpus ~extras manager_path domid fd vgpu_fd in
    let local_stuff = [
      "serial/0/limit",    string_of_int 65536;
      "console/port",     string_of_int console_port;
      "console/ring-ref", sprintf "%nu" console_mfn;
      "console/limit",    string_of_int 65536;
    ] in
    let vm_stuff = [
      "rtc/timeoffset",    timeoffset;
    ] in
    (* and finish domain's building *)
    build_post ~xc ~xs ~vcpus ~target_mib ~static_max_mib
      domid store_mfn store_port local_stuff vm_stuff

  let restore (task: Xenops_task.task_handle) ~xc ~xs ~store_domid ~console_domid
      ~no_incr_generationid ~timeoffset ~extras info ~manager_path domid fd vgpu_fd =
    let restore_fct = match info.priv with
      | BuildHVM hvminfo ->
        hvm_restore task ~shadow_multiplier:hvminfo.shadow_multiplier
          ~timeoffset
      | BuildPV pvinfo   ->
        pv_restore task
    in
    restore_fct ~xc ~xs ~store_domid ~console_domid ~no_incr_generationid
      ~static_max_kib:info.memory_max ~target_kib:info.memory_target ~vcpus:info.vcpus ~extras
      manager_path domid fd vgpu_fd

  let suspend_emu_manager' ~(task: Xenops_task.task_handle) ~xc ~xs ~hvm ~dm ~manager_path ~domid
      ~uuid ~main_fd ~vgpu_fd ~flags ~progress_callback ~qemu_domid ~do_suspend_callback =
    let open Suspend_image in let open Suspend_image.M in
    let open Emu_manager in

    let fd_uuid = Uuid.(to_string (create `V4)) in

    let vgpu_args, vgpu_cmdline =
      match vgpu_fd with
      | Some fd when fd = main_fd ->
        [fd_uuid, main_fd],
        ["-dm"; "vgpu:" ^ fd_uuid]
      | Some fd ->
        let vgpu_fd_uuid = Uuid.(to_string (create `V4)) in
        [vgpu_fd_uuid, fd],
        ["-dm"; "vgpu:" ^ vgpu_fd_uuid]
      | None -> [], []
    in

    let cmdline_to_flag flag =
      match flag with
      | Live -> [ "-live"; "true" ]
      | Debug -> [ "-debug"; "true" ]
    in
    let flags' = List.map cmdline_to_flag flags in

    let args = [
      "-fd"; fd_uuid;
      "-mode"; if hvm then "hvm_save" else "save";
      "-domid"; string_of_int domid;
      "-fork"; "true";
    ] @ (List.concat flags') @ vgpu_cmdline in

    let fds = [fd_uuid, main_fd] @ vgpu_args in

    (* Start the emu-manager process and connect to the control socket *)
    with_connection task manager_path domid args fds (fun cnx ->
        (* Callback to monitor the debug (stderr) output of the process and
                		   spot the progress indicator *)
        let callback txt =
          let prefix = "\\b\\b\\b\\b" in
          if String.startswith prefix txt then
            let rest = String.sub txt (String.length prefix)
                (String.length txt - (String.length prefix)) in
            match Stdext.Xstringext.String.split_f (fun c -> c = ' ' || c = '%') rest with
            | [ percent ] -> (
                try
                  let percent = int_of_string percent in
                  debug "VM = %s; domid = %d; progress = %d / 100" (Uuid.to_string uuid) domid percent;
                  progress_callback (float_of_int percent /. 100.)
                with e ->
                  error "VM = %s; domid = %d; failed to parse progress update: \"%s\"" (Uuid.to_string uuid) domid percent;
                  (* MTC: catch exception by progress_callback, for example,
                     an abort request, and re-raise them *)
                  raise e
              )
            | _ -> ()
          else
            debug "VM = %s; domid = %d; %s" (Uuid.to_string uuid) domid txt
        in

        (* Process started; wait for and respond to instructions *)
        let rec wait_for_message () =
          debug "VM = %s; domid = %d; waiting for emu-manager..." (Uuid.to_string uuid) domid;
          let message = non_debug_receive ~debug_callback:callback cnx in
          debug "VM = %s; domid = %d; message from emu-manager: %s"
            (Uuid.to_string uuid) domid (string_of_message message);
          match message with
          | Suspend ->
            do_suspend_callback ();
            if hvm then (
              debug "VM = %s; domid = %d; suspending qemu-dm" (Uuid.to_string uuid) domid;
              Device.Dm.suspend task ~xs ~qemu_domid ~dm domid;
            );
            send_done cnx;
            wait_for_message ()
          | Prepare x when x = "xenguest" ->
            debug "Writing Libxc header";
            write_header main_fd (Libxc, 0L) >>= fun () ->
            debug "Writing Libxc record";
            send_done cnx;
            wait_for_message ()
          | Prepare x when x = "vgpu" ->
            (match vgpu_fd with
             | Some fd ->
               debug "Writing DEMU header";
               write_header fd (Demu, 0L) >>= fun () ->
               debug "Writing DEMU record";
               send_done cnx;
               wait_for_message ()
             | None ->
               `Error (Emu_manager_failure ("Received prepare:vgpu from emu-manager, but there is no vGPU fd")))
          | Result x ->
            debug "VM = %s; domid = %d; emu-manager completed successfully" (Uuid.to_string uuid) domid;
            return ()
          | Error x ->
            error "VM = %s; domid = %d; emu-manager failed: \"%s\"" (Uuid.to_string uuid) domid x;
            `Error (Emu_manager_failure (Printf.sprintf "Received error from emu-manager: %s" x))
          | _                       ->
            error "VM = %s; domid = %d; unexpected message from emu-manager" (Uuid.to_string uuid) domid;
            `Error (Emu_manager_protocol_failure)
        in
        wait_for_message ()
      )

  let suspend_emu_manager ~(task: Xenops_task.task_handle) ~xc ~xs ~hvm ~dm ~manager_path ~domid
      ~uuid ~main_fd ~vgpu_fd ~flags ~progress_callback ~qemu_domid ~do_suspend_callback =
    Device.Dm.with_dirty_log dm domid ~f:(fun () ->
        suspend_emu_manager' ~task ~xc ~xs ~hvm ~dm ~manager_path ~domid
          ~uuid ~main_fd ~vgpu_fd ~flags ~progress_callback ~qemu_domid ~do_suspend_callback
      )

  let write_qemu_record domid uuid fd =
    let file = sprintf qemu_save_path domid in
    let fd2 = Unix.openfile file [ Unix.O_RDONLY ] 0o640 in
    finally (fun () ->
        let size = Int64.of_int Unix.((stat file).st_size) in
        let open Suspend_image in let open Suspend_image.M in
        debug "Writing Qemu_trad header with length %Ld" size;
        write_header fd (Qemu_trad, size) >>= fun () ->
        debug "VM = %s; domid = %d; writing %Ld bytes from %s" (Uuid.to_string uuid) domid size file;
        if Unixext.copy_file ~limit:size fd2 fd <> size
        then failwith "Failed to write whole qemu-dm state file";
        return ()
      ) (fun () ->
        Unix.unlink file;
        Unix.close fd2
      )

  (* suspend register the callback function that will be call by linux_save
   * and is in charge to suspend the domain when called. the whole domain
   * context is saved to fd
  *)
  let suspend (task: Xenops_task.task_handle) ~xc ~xs ~hvm ~dm ~manager_path vm_str domid main_fd vgpu_fd flags ?(progress_callback = fun _ -> ()) ~qemu_domid do_suspend_callback =
    let module DD = Debug.Make(struct let name = "mig64" end) in
    let open DD in
    let uuid = get_uuid ~xc domid in
    debug "VM = %s; domid = %d; suspend live = %b" (Uuid.to_string uuid) domid (List.mem Live flags);
    let open Suspend_image in let open Suspend_image.M in
    (* Suspend image signature *)
    debug "Writing save signature: %s" save_signature;
    Io.write main_fd save_signature;
    (* CA-248130: originally, [xs_subtree] contained [xenstore_read_dir t
       	 * (xs.Xs.getdomainpath domid)] and this data was written to [fd].
       	 * However, on the receiving side this data is never used. As a
       	 * short-term fix, we sent nothing but keep the write to maintain the
       	 * protocol.
       	 *)
    let res =
      let xs_subtree = [] in
      Xenops_record.(to_string (make ~xs_subtree ~vm_str ())) >>= fun xenops_record ->
      let xenops_rec_len = String.length xenops_record in
      debug "Writing Xenops header (length=%d)" xenops_rec_len;
      write_header main_fd (Xenops, Int64.of_int xenops_rec_len) >>= fun () ->
      debug "Writing Xenops record contents";
      Io.write main_fd xenops_record;
      suspend_emu_manager ~task ~xc ~xs ~hvm ~dm ~manager_path ~domid ~uuid ~main_fd ~vgpu_fd ~flags
        ~progress_callback ~qemu_domid ~do_suspend_callback >>= fun () ->
      (* Qemu record (if this is a hvm domain) *)
      (* Currently Qemu suspended inside above call with the libxc memory
         		* image, we should try putting it below in the relevant section of the
         		* suspend-image-writing *)
      (if hvm then write_qemu_record domid uuid main_fd else return ()) >>= fun () ->
      debug "Qemu record written";
      debug "Writing End_of_image footer(s)";
      progress_callback 1.;
      (* Close all streams *)
      let fds = Stdext.Listext.List.setify (main_fd :: Opt.to_list vgpu_fd) in
      fold (fun fd () -> write_header fd (End_of_image, 0L)) fds ()
    in match res with
    | `Ok () ->
      debug "VM = %s; domid = %d; suspend complete" (Uuid.to_string uuid) domid
    | `Error e -> raise e

end

module Suspend_restore_xenguest: SUSPEND_RESTORE = struct

  let restore_libxc_record (task: Xenops_task.task_handle) ~hvm ~store_port ~console_port ~extras manager_path domid uuid fd =
    let fd_uuid = Uuid.(to_string (create `V4)) in
    let line = XenguestHelper.with_connection task manager_path domid
        ([
          "-mode"; if hvm then "hvm_restore" else "restore";
          "-domid"; string_of_int domid;
          "-fd"; fd_uuid;
          "-store_port"; string_of_int store_port;
          "-console_port"; string_of_int console_port;
          "-fork"; "true";
        ] @ extras) [ fd_uuid, fd ] XenguestHelper.receive_success in
    match Stdext.Xstringext.String.split ' ' line with
    | [ store; console ] ->
      debug "VM = %s; domid = %d; store_mfn = %s; console_mfn = %s" (Uuid.to_string uuid) domid store console;
      Nativeint.of_string store, Nativeint.of_string console
    | _                  ->
      error "VM = %s; domid = %d; domain builder returned invalid result: \"%s\"" (Uuid.to_string uuid) domid line;
      raise Domain_restore_failed

  let consume_qemu_record fd limit domid uuid =
    if limit > 1_048_576L then begin (* 1MB *)
      error "VM = %s; domid = %d; QEMU record length in header too large (%Ld bytes)"
        (Uuid.to_string uuid) domid limit;
      raise Suspend_image_failure
    end;
    let file = sprintf qemu_restore_path domid in
    let fd2 = Unix.openfile file
        [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC; ] 0o640
    in
    finally (fun () ->
        debug "VM = %s; domid = %d; reading %Ld bytes from %s"
          (Uuid.to_string uuid) domid limit file;
        let bytes =
          try
            Unixext.copy_file ~limit fd fd2
          with Unix.Unix_error (e, s1, s2) ->
            error "VM = %s; domid = %d; %s, %s, %s" (Uuid.to_string uuid) domid (Unix.error_message e) s1 s2;
            Unixext.unlink_safe file;
            raise Suspend_image_failure
        in
        if bytes <> limit
        then begin
          error "VM = %s; domid = %d; qemu save file was truncated"
            (Uuid.to_string uuid) domid;
          raise Domain_restore_truncated_hvmstate
        end
      ) (fun () -> Unix.close fd2)

  let consume_demu_record fd limit domid uuid =
    let file = sprintf demu_restore_path domid in
    let fd2 = Unix.openfile file
        [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC; ] 0o640
    in
    finally (fun () ->
        debug "VM = %s; domid = %d; reading %Ld bytes from %s" (Uuid.to_string uuid) domid limit file;
        let bytes =
          try
            Unixext.copy_file ~limit fd fd2
          with Unix.Unix_error (e, s1, s2) ->
            error "VM = %s; domid = %d; %s, %s, %s" (Uuid.to_string uuid) domid (Unix.error_message e) s1 s2;
            Unixext.unlink_safe file;
            raise Suspend_image_failure
        in
        if bytes <> limit
        then begin
          error "VM = %s; domid = %d; qemu save file was truncated"
            (Uuid.to_string uuid) domid;
          raise Domain_restore_truncated_vgpustate
        end
      ) (fun () -> Unix.close fd2)

  let restore_common (task: Xenops_task.task_handle) ~xc ~xs ~hvm ~store_port ~store_domid ~console_port ~console_domid ~no_incr_generationid ~vcpus ~extras manager_path domid fd =
    let module DD = Debug.Make(struct let name = "mig64" end) in
    let open DD in
    let uuid = get_uuid ~xc domid in
    let open Suspend_image in
    match read_save_signature fd with
    | `Ok Legacy ->
      debug "Detected legacy suspend image! Piping through conversion tool.";
      let (store_mfn, console_mfn) =
        begin match
            with_conversion_script task "XenguestHelper" hvm fd (fun pipe_r ->
                restore_libxc_record task ~hvm ~store_port ~console_port
                  ~extras manager_path domid uuid pipe_r
              )
          with
          | `Ok (s, c) -> (s, c)
          | `Error e ->
            error "Caught error when using converison script: %s" (Printexc.to_string e);
            Xenops_task.cancel task;
            raise e
        end
      in
      (* Consume the (legacy) QEMU Record *)
      if hvm
      then begin
        debug "Reading legacy (Xenops-level) QEMU record signature";
        let length = begin match (read_legacy_qemu_header fd) with
          | `Ok length -> length
          | `Error e ->
            error "VM = %s; domid = %d; Error reading QEMU signature: %s"
              (Uuid.to_string uuid) domid e;
            raise Suspend_image_failure
        end in
        debug "Consuming QEMU record into file";
        consume_qemu_record fd length domid uuid
      end;
      store_mfn, console_mfn
    | `Ok Structured ->
      let open Suspend_image.M in
      let rec process_header res =
        debug "Reading next header...";
        read_header fd >>= function
        | Xenops, len ->
          debug "Read Xenops record header (length=%Ld)" len;
          let rec_str = Io.read fd (Io.int_of_int64_exn len) in
          debug "Read Xenops record contents";
          Xenops_record.of_string rec_str >>= fun (_ : Xenops_record.t) ->
          debug "Validated Xenops record contents";
          process_header res
        | Libxc, _ ->
          debug "Read Libxc record header";
          let res = restore_libxc_record task ~hvm ~store_port ~console_port
              ~extras manager_path domid uuid fd
          in
          process_header (return (Some res))
        | Libxc_legacy, _ ->
          debug "Read Libxc_legacy record header";
          let res = restore_libxc_record task ~hvm ~store_port ~console_port
              ~extras manager_path domid uuid fd
          in
          process_header (return (Some res))
        | Qemu_trad, len ->
          debug "Read Qemu_trad header (length=%Ld)" len;
          consume_qemu_record fd len domid uuid;
          process_header res
        | Demu, len ->
          debug "Read Demu header (length=%Ld)" len;
          consume_demu_record fd len domid uuid;
          process_header res
        | End_of_image, _ ->
          debug "Read suspend image footer";
          res
        | _ -> failwith "Unsupported"
      in
      begin match process_header (return None) with
        | `Ok (Some (store_mfn, console_mfn)) -> store_mfn, console_mfn
        | `Ok None -> failwith "Well formed, but useless stream"
        | `Error e -> raise e
      end
    | `Error e ->
      error "VM = %s; domid = %d; Error reading save signature: %s" (Uuid.to_string uuid) domid e;
      raise Suspend_image_failure

  let pv_restore (task: Xenops_task.task_handle) ~xc ~xs ~store_domid ~console_domid ~no_incr_generationid ~static_max_kib ~target_kib ~vcpus ~extras manager_path domid fd =

    (* Convert memory configuration values into the correct units. *)
    let static_max_mib = Memory.mib_of_kib_used static_max_kib in
    let target_mib     = Memory.mib_of_kib_used target_kib in

    (* Sanity check. *)
    assert (target_mib <= static_max_mib);

    (* Adapt memory configuration values for Xen and the domain builder. *)
    let xen_max_mib =
      Memory.Linux.xen_max_mib static_max_mib in
    let shadow_multiplier =
      Memory.Linux.shadow_multiplier_default in
    let shadow_mib =
      Memory.Linux.shadow_mib static_max_mib vcpus shadow_multiplier in
    let required_host_free_mib =
      Memory.Linux.footprint_mib target_mib static_max_mib vcpus shadow_multiplier in

    let store_port, console_port = build_pre ~xc ~xs
        ~xen_max_mib ~shadow_mib ~required_host_free_mib ~vcpus domid in

    let store_mfn, console_mfn = restore_common task ~xc ~xs ~hvm:false
        ~store_port ~store_domid
        ~console_port ~console_domid
        ~no_incr_generationid
        ~vcpus ~extras manager_path domid fd in

    let local_stuff = [
      "serial/0/limit",    string_of_int 65536;
      "console/port",     string_of_int console_port;
      "console/ring-ref", sprintf "%nu" console_mfn;
    ] in
    let vm_stuff = [] in
    build_post ~xc ~xs ~vcpus ~target_mib ~static_max_mib
      domid store_mfn store_port local_stuff vm_stuff

  let hvm_restore (task: Xenops_task.task_handle) ~xc ~xs ~store_domid ~console_domid ~no_incr_generationid ~static_max_kib ~target_kib ~shadow_multiplier ~vcpus ~timeoffset ~extras manager_path domid fd =

    (* Convert memory configuration values into the correct units. *)
    let static_max_mib = Memory.mib_of_kib_used static_max_kib in
    let target_mib     = Memory.mib_of_kib_used target_kib in

    (* Sanity check. *)
    assert (target_mib <= static_max_mib);

    (* Adapt memory configuration values for Xen and the domain builder. *)
    let xen_max_mib =
      Memory.HVM.xen_max_mib static_max_mib in
    let shadow_mib =
      Memory.HVM.shadow_mib static_max_mib vcpus shadow_multiplier in
    let required_host_free_mib =
      Memory.HVM.footprint_mib target_mib static_max_mib vcpus shadow_multiplier in

    maybe_ca_140252_workaround ~xc ~vcpus domid;
    let store_port, console_port = build_pre ~xc ~xs
        ~xen_max_mib ~shadow_mib ~required_host_free_mib ~vcpus domid in

    let store_mfn, console_mfn = restore_common task ~xc ~xs ~hvm:true
        ~store_port ~store_domid
        ~console_port ~console_domid
        ~no_incr_generationid
        ~vcpus ~extras manager_path domid fd in
    let local_stuff = [
      "serial/0/limit",    string_of_int 65536;
      "console/port",     string_of_int console_port;
      "console/ring-ref", sprintf "%nu" console_mfn;
    ] in
    let vm_stuff = [
      "rtc/timeoffset",    timeoffset;
    ] in
    (* and finish domain's building *)
    build_post ~xc ~xs ~vcpus ~target_mib ~static_max_mib
      domid store_mfn store_port local_stuff vm_stuff

  let restore (task: Xenops_task.task_handle) ~xc ~xs ~store_domid ~console_domid ~no_incr_generationid ~timeoffset ~extras info ~manager_path domid fd vgpu_fd =
    let restore_fct = match info.priv with
      | BuildHVM hvminfo ->
        hvm_restore task ~shadow_multiplier:hvminfo.shadow_multiplier
          ~timeoffset
      | BuildPV pvinfo   ->
        pv_restore task
    in
    restore_fct ~xc ~xs ~store_domid ~console_domid ~no_incr_generationid
      ~static_max_kib:info.memory_max ~target_kib:info.memory_target ~vcpus:info.vcpus ~extras
      manager_path domid fd

  let write_libxc_record' (task: Xenops_task.task_handle) ~xc ~xs ~hvm ~dm manager_path domid uuid fd flags progress_callback qemu_domid do_suspend_callback =
    let fd_uuid = Uuid.(to_string (create `V4)) in

    let cmdline_to_flag flag =
      match flag with
      | Live -> [ "-live"; "true" ]
      | Debug -> [ "-debug"; "true" ]
    in
    let flags' = List.map cmdline_to_flag flags in

    let xenguestargs = [
      "-fd"; fd_uuid;
      "-mode"; if hvm then "hvm_save" else "save";
      "-domid"; string_of_int domid;
      "-fork"; "true";
    ] @ (List.concat flags') in

    XenguestHelper.with_connection task manager_path domid xenguestargs [ fd_uuid, fd ]
      (fun cnx ->
         debug "VM = %s; domid = %d; waiting for xenguest to call suspend callback" (Uuid.to_string uuid) domid;

         (* Monitor the debug (stderr) output of the xenguest helper and
            		   spot the progress indicator *)
         let callback txt =
           let prefix = "\\b\\b\\b\\b" in
           if String.startswith prefix txt then
             let rest = String.sub txt (String.length prefix)
                 (String.length txt - (String.length prefix)) in
             match Stdext.Xstringext.String.split_f (fun c -> c = ' ' || c = '%') rest with
             | [ percent ] -> (
                 try
                   let percent = int_of_string percent in
                   debug "VM = %s; domid = %d; progress = %d / 100" (Uuid.to_string uuid) domid percent;
                   progress_callback (float_of_int percent /. 100.)
                 with e ->
                   error "VM = %s; domid = %d; failed to parse progress update: \"%s\"" (Uuid.to_string uuid) domid percent;
                   (* MTC: catch exception by progress_callback, for example,
                      an abort request, and re-raise them *)
                   raise e
               )
             | _ -> ()
           else
             debug "VM = %s; domid = %d; %s" (Uuid.to_string uuid) domid txt
         in

         (match XenguestHelper.non_debug_receive ~debug_callback:callback cnx with
          | XenguestHelper.Suspend ->
            debug "VM = %s; domid = %d; suspend callback called" (Uuid.to_string uuid) domid;
          | XenguestHelper.Error x ->
            error "VM = %s; domid = %d; xenguesthelper failed: \"%s\"" (Uuid.to_string uuid) domid x;
            raise (Xenguest_failure (Printf.sprintf "Error while waiting for suspend notification: %s" x))
          | msg ->
            let err = Printf.sprintf "expected %s got %s"
                (XenguestHelper.string_of_message XenguestHelper.Suspend)
                (XenguestHelper.string_of_message msg) in
            error "VM = %s; domid = %d; xenguesthelper protocol failure %s" (Uuid.to_string uuid) domid err;
            raise (Xenguest_protocol_failure err));
         do_suspend_callback ();
         if hvm then (
           debug "VM = %s; domid = %d; suspending qemu-dm" (Uuid.to_string uuid) domid;
           Device.Dm.suspend task ~xs ~qemu_domid ~dm domid;
         );
         XenguestHelper.send cnx "done\n";

         let msg = XenguestHelper.non_debug_receive cnx in
         progress_callback 1.;
         match msg with
         | XenguestHelper.Result x ->
           debug "VM = %s; domid = %d; xenguesthelper returned \"%s\"" (Uuid.to_string uuid) domid x
         | XenguestHelper.Error x  ->
           error "VM = %s; domid = %d; xenguesthelper failed: \"%s\"" (Uuid.to_string uuid) domid x;
           raise (Xenguest_failure (Printf.sprintf "Received error from xenguesthelper: %s" x))
         | _                       ->
           error "VM = %s; domid = %d; xenguesthelper protocol failure" (Uuid.to_string uuid) domid;
      )

  let write_libxc_record (task: Xenops_task.task_handle) ~xc ~xs ~hvm ~dm manager_path domid uuid fd flags progress_callback qemu_domid do_suspend_callback =
    Device.Dm.with_dirty_log dm domid ~f:(fun () ->
        write_libxc_record' task ~xc ~xs ~hvm ~dm manager_path domid uuid fd flags progress_callback qemu_domid do_suspend_callback
      )

  let write_qemu_record domid uuid legacy_libxc fd =
    let file = sprintf qemu_save_path domid in
    let fd2 = Unix.openfile file [ Unix.O_RDONLY ] 0o640 in
    finally (fun () ->
        let size = Int64.of_int Unix.((stat file).st_size) in
        let open Suspend_image in let open Suspend_image.M in
        (if legacy_libxc then begin
            debug "Writing Qemu signature suitable for legacy libxc with length %Ld" size;
            write_qemu_header_for_legacy_libxc fd size
          end else begin
           debug "Writing Qemu_trad header with length %Ld" size;
           write_header fd (Qemu_trad, size)
         end) >>= fun () ->
        debug "VM = %s; domid = %d; writing %Ld bytes from %s" (Uuid.to_string uuid) domid size file;
        if Unixext.copy_file ~limit:size fd2 fd <> size
        then failwith "Failed to write whole qemu-dm state file";
        return ()
      ) (fun () ->
        Unix.unlink file;
        Unix.close fd2
      )

  let write_demu_record domid uuid fd =
    let file = sprintf demu_save_path domid in
    let fd2 = Unix.openfile file [ Unix.O_RDONLY ] 0o640 in
    finally (fun () ->
        let size = Int64.of_int Unix.((stat file).st_size) in
        let open Suspend_image in
        let open Suspend_image.M in
        debug "Writing Demu header with length %Ld" size;
        write_header fd (Demu, size) >>= fun () ->
        debug "VM = %s; domid = %d; writing %Ld bytes from %s" (Uuid.to_string uuid) domid size file;
        if Unixext.copy_file ~limit:size fd2 fd <> size
        then failwith "Failed to write whole demu state file";
        return ()
      ) (fun () ->
        Unix.unlink file;
        Unix.close fd2
      )

  (* suspend register the callback function that will be call by linux_save
   * and is in charge to suspend the domain when called. the whole domain
   * context is saved to fd
  *)
  let suspend (task: Xenops_task.task_handle) ~xc ~xs ~hvm ~dm ~manager_path vm_str domid main_fd vgpu_fd flags ?(progress_callback = fun _ -> ()) ~qemu_domid do_suspend_callback =
    let module DD = Debug.Make(struct let name = "mig64" end) in
    let open DD in
    let uuid = get_uuid ~xc domid in
    debug "VM = %s; domid = %d; suspend live = %b" (Uuid.to_string uuid) domid (List.mem Live flags);
    let open Suspend_image in let open Suspend_image.M in
    (* Suspend image signature *)
    debug "Writing save signature: %s" save_signature;
    Io.write main_fd save_signature;
    (* CA-248130: originally, [xs_subtree] contained [xenstore_read_dir t
       	 * (xs.Xs.getdomainpath domid)] and this data was written to [fd].
       	 * However, on the receiving side this data is never used. As a
       	 * short-term fix, we sent nothing but keep the write to maintain the
       	 * protocol.
       	 *)
    let res =
      let xs_subtree = [] in
      Xenops_record.(to_string (make ~xs_subtree ~vm_str ())) >>= fun xenops_record ->
      let xenops_rec_len = String.length xenops_record in
      debug "Writing Xenops header (length=%d)" xenops_rec_len;
      write_header main_fd (Xenops, Int64.of_int xenops_rec_len) >>= fun () ->
      debug "Writing Xenops record contents";
      Io.write main_fd xenops_record;
      (* Libxc record *)
      let legacy_libxc = not (XenguestHelper.supports_feature manager_path "migration-v2") in
      debug "Writing Libxc%s header" (if legacy_libxc then "_legacy" else "");
      let libxc_header = (if legacy_libxc then Libxc_legacy else Libxc) in
      write_header main_fd (libxc_header, 0L) >>= fun () ->
      debug "Writing Libxc record";
      write_libxc_record task ~xc ~xs ~hvm ~dm manager_path domid uuid main_fd flags
        progress_callback qemu_domid do_suspend_callback;
      (* Qemu record (if this is a hvm domain) *)
      (* Currently Qemu suspended inside above call with the libxc memory
         		* image, we should try putting it below in the relevant section of the
         		* suspend-image-writing *)
      (if hvm then write_qemu_record domid uuid legacy_libxc main_fd else return ()) >>= fun () ->
      debug "Qemu record written";
      let vgpu = Sys.file_exists (sprintf demu_save_path domid) in
      (if vgpu then write_demu_record domid uuid main_fd else return ()) >>= fun () ->
      debug "Writing End_of_image footer";
      write_header main_fd (End_of_image, 0L)
    in match res with
    | `Ok () ->
      debug "VM = %s; domid = %d; suspend complete" (Uuid.to_string uuid) domid
    | `Error e -> raise e

end

let use_emu_manager () =
  try
    Filename.concat !Xenopsd.feature_flags_path "vgpu_migration"
    |> fun path -> Stdext.Unixext.read_lines ~path
                   |> String.concat ""
                   |> String.trim
                   |> String.lowercase
                   |> fun f -> f = "true" || f = "1"
  with _ ->
    false

let restore (task: Xenops_task.task_handle) ~xc ~xs ~store_domid ~console_domid ~no_incr_generationid ~timeoffset ~extras info ~xenguest_path ~emu_manager_path domid fd vgpu_fd =
  let restore =
    if use_emu_manager () then
      Suspend_restore_emu_manager.restore ~manager_path:emu_manager_path
    else
      Suspend_restore_xenguest.restore ~manager_path:xenguest_path
  in
  restore task xc xs store_domid console_domid no_incr_generationid timeoffset extras info domid fd vgpu_fd


let suspend (task: Xenops_task.task_handle) ~xc ~xs ~hvm ~dm ~xenguest_path ~emu_manager_path vm_str domid fd vgpu_fd flags ?(progress_callback = fun _ -> ()) ~qemu_domid do_suspend_callback =
  let suspend =
    if use_emu_manager () then
      Suspend_restore_emu_manager.suspend ~manager_path:emu_manager_path
    else
      Suspend_restore_xenguest.suspend ~manager_path:xenguest_path
  in
  suspend task ~xc ~xs ~hvm ~dm vm_str domid fd vgpu_fd flags ~progress_callback ~qemu_domid do_suspend_callback

let send_s3resume ~xc domid =
  let uuid = get_uuid ~xc domid in
  debug "VM = %s; domid = %d; send_s3resume" (Uuid.to_string uuid) domid;
  Xenctrlext.domain_send_s3resume xc domid

let vcpu_affinity_set ~xc domid vcpu cpumap =
 (*
	let bitmap = ref Int64.zero in
	if Array.length cpumap > 64 then
		invalid_arg "affinity_set";
	let bit_set bitmap n =
		Int64.logor bitmap (Int64.shift_left 1L n) in
	(* set bits in the bitmap that are true *)
	Array.iteri (fun i has_affinity ->
		if has_affinity then bitmap := bit_set !bitmap i
		) cpumap;
	(*Xenctrl.vcpu_affinity_set xc domid vcpu !bitmap*)
	*)
  let uuid = get_uuid ~xc domid in
  debug "VM = %s; domid = %d; vcpu_affinity_set %d <- %s" (Uuid.to_string uuid) domid vcpu
    (String.concat "" (List.map (fun b -> if b then "1" else "0") (Array.to_list cpumap)));
  Xenctrl.vcpu_affinity_set xc domid vcpu cpumap


let vcpu_affinity_get ~xc domid vcpu =
 (*
	let pcpus = (Xenctrl.physinfo xc).Xenctrl.max_nr_cpus in
	(* NB we ignore bits corresponding to pCPUs which we don't have *)
	let bitmap = Xenctrl.vcpu_affinity_get xc domid vcpu in
	let bit_isset bitmap n =
		(Int64.logand bitmap (Int64.shift_left 1L n)) > 0L in
	let cpumap = Array.of_list (List.map (bit_isset bitmap) (List.range 0 pcpus)) in
	cpumap
	*)
  let uuid = get_uuid ~xc domid in
  debug "VM = %s; domid = %d; vcpu_affinity_get %d" (Uuid.to_string uuid) domid vcpu;
  Xenctrl.vcpu_affinity_get xc domid vcpu

let set_memory_dynamic_range ~xc ~xs ~min ~max domid =
  let kvs = [
    "dynamic-min", string_of_int min;
    "dynamic-max", string_of_int max;
  ] in
  let uuid = get_uuid ~xc domid in
  debug "VM = %s; domid = %d; set_memory_dynamic_range min = %d; max = %d" (Uuid.to_string uuid) domid min max;
  xs.Xs.writev (Printf.sprintf "%s/memory" (xs.Xs.getdomainpath domid)) kvs

let add_ioport ~xc domid start_port end_port =
  let uuid = get_uuid ~xc domid in
  let nr_ports = end_port - start_port in
  debug "VM = %s; domid = %d; ioport add %#x-%#x" (Uuid.to_string uuid) domid start_port (start_port + nr_ports);
  Xenctrl.domain_ioport_permission xc domid start_port nr_ports true

let del_ioport ~xc domid start_port end_port =
  let uuid = get_uuid ~xc domid in
  let nr_ports = end_port - start_port in
  debug "VM = %s; domid = %d; ioport del %#x-%#x" (Uuid.to_string uuid) domid start_port (start_port + nr_ports);
  Xenctrl.domain_ioport_permission xc domid start_port nr_ports false

(* start_address and end_address are potentially 64 bit? *)
let add_iomem ~xc domid start_address end_address =
  let uuid = get_uuid ~xc domid in
  let mem_to_pfn m = Int64.to_nativeint (Int64.div m 4096L) in
  let start_pfn = mem_to_pfn start_address and end_pfn = mem_to_pfn end_address in
  let nr_pfns = Nativeint.sub end_pfn start_pfn in
  debug "VM = %s; domid = %d; iomem add %#nx-%#nx" (Uuid.to_string uuid) domid start_pfn end_pfn;
  Xenctrl.domain_iomem_permission xc domid start_pfn nr_pfns true

let del_iomem ~xc domid start_address end_address =
  let uuid = get_uuid ~xc domid in
  let mem_to_pfn m = Int64.to_nativeint (Int64.div m 4096L) in
  let start_pfn = mem_to_pfn start_address and end_pfn = mem_to_pfn end_address in
  let nr_pfns = Nativeint.sub end_pfn start_pfn in
  debug "VM = %s; domid = %d; iomem del %#nx-%#nx" (Uuid.to_string uuid) domid start_pfn end_pfn;
  Xenctrl.domain_iomem_permission xc domid start_pfn nr_pfns false

let add_irq ~xc domid irq =
  let uuid = get_uuid ~xc domid in
  debug "VM = %s; domid = %d; irq add %#x" (Uuid.to_string uuid) domid irq;
  Xenctrl.domain_irq_permission xc domid irq true

let del_irq ~xc domid irq =
  let uuid = get_uuid ~xc domid in
  debug "VM = %s; domid = %d; irq del %#x" (Uuid.to_string uuid) domid irq;
  Xenctrl.domain_irq_permission xc domid irq false

let set_machine_address_size ~xc domid width =
  match width with
  | Some width -> begin
      let uuid = get_uuid ~xc domid in
      debug "VM = %s; domid = %d; domain_set_machine_address_size %d bits" (Uuid.to_string uuid) domid width;
      Xenctrl.domain_set_machine_address_size xc domid width
    end
  | None -> ()

let suppress_spurious_page_faults ~xc domid =
  let uuid = get_uuid ~xc domid in
  debug "VM = %s; domid = %d; domain_suppress_spurious_page_faults" (Uuid.to_string uuid) domid;
  Xenctrlext.domain_suppress_spurious_page_faults xc domid

(** Sets the current memory target for a running VM, to the given value (in KiB), *)
(** by writing the target to XenStore. The value is automatically rounded down to *)
(** the nearest page boundary.                                                    *)
let set_memory_target ~xs domid mem_kib =
  let mem_kib = Memory.round_kib_down_to_nearest_page_boundary mem_kib in
  let dompath = xs.Xs.getdomainpath domid in
  xs.Xs.write (dompath ^ "/memory/target") (Int64.to_string mem_kib);
  (* Debugging information: *)
  let mem_mib = Memory.mib_of_kib_used mem_kib in
  debug "domain %d set memory target to %Ld MiB" domid mem_mib


let set_xsdata ~xs domid xsdata =
  let dom_path = Printf.sprintf "/local/domain/%d" domid in
  Xs.transaction xs (fun t ->
      List.iter (fun x -> t.Xst.rm (dom_path ^ "/" ^ x)) allowed_xsdata_prefixes;
      t.Xst.writev dom_path (filtered_xsdata xsdata);
    )
