(*
 * Copyright (C) 2023 Cloud Software Group
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
module D = Debug.Make (struct let name = "tracing" end)

open D

type endpoint = Bugtool | Url of string [@@deriving rpcty]

module SpanKind = struct
  type t = Server | Consumer | Client | Producer | Internal [@@deriving rpcty]

  let to_string = function
    | Server ->
        "SERVER"
    | Consumer ->
        "CONSUMER"
    | Client ->
        "CLIENT"
    | Producer ->
        "PRODUCER"
    | Internal ->
        "INTERNAL"
end

type provider_config_t = {
    name_label: string
  ; tags: (string * string) list
  ; endpoints: endpoint list
  ; filters: string list
  ; processors: string list
  ; enabled: bool
}
[@@deriving rpcty]

let endpoint_of_string = function "bugtool" -> Bugtool | url -> Url url

module Status = struct
  type status_code = Unset | Ok | Error [@@deriving rpcty]

  type t = {status_code: status_code; description: string option}
  [@@deriving rpcty]

  let status_to_string status =
    let desc =
      Option.fold ~none:"" ~some:(fun d -> " " ^ d) status.description
    in
    let value =
      match status.status_code with
      | Unset ->
          "Unset"
      | Ok ->
          "Ok"
      | Error ->
          "Error"
    in
    value ^ desc
end

module SpanContext = struct
  type t = {trace_id: string; span_id: string} [@@deriving rpcty]

  let to_traceparent t = Printf.sprintf "00-%s-%s-00" t.trace_id t.span_id

  let of_traceparent traceparent =
    let elements = String.split_on_char '-' traceparent in
    match elements with
    | ["00"; trace_id; span_id; "00"] ->
        Some {trace_id; span_id}
    | _ ->
        None
end

module Span = struct
  type t = {
      context: SpanContext.t
    ; mutable span_kind: SpanKind.t
    ; status: Status.t
    ; parent: t option
    ; name: string
    ; begin_time: float
    ; end_time: float option
    ; tags: (string * string) list
  }
  [@@deriving rpcty]

  let get_context t = t.context

  let generate_id n = String.init n (fun _ -> "0123456789abcdef".[Random.int 16])

  let start ?(tags = []) ~name ~parent ~span_kind () =
    let trace_id =
      match parent with
      | None ->
          generate_id 32
      | Some span_parent ->
          span_parent.context.trace_id
    in
    let span_id = generate_id 16 in
    let context : SpanContext.t = {trace_id; span_id} in
    let begin_time = Unix.gettimeofday () in
    let end_time = None in
    let status : Status.t = {status_code= Status.Unset; description= None} in
    {context; span_kind; status; parent; name; begin_time; end_time; tags}

  let finish ?(tags = []) ~span () =
    let status = span.status in
    let tags =
      if status.status_code = Error then
        ( Status.status_to_string status
        , Option.value ~default:"" status.Status.description
        )
        :: tags
      else
        tags
    in
    {span with end_time= Some (Unix.gettimeofday ()); tags= span.tags @ tags}

  let set_span_kind span kind = span.span_kind <- kind

  let set_error span exn =
    let backtrace = Printexc.get_backtrace () in
    let description =
      Some
        (Printf.sprintf "Error: %s Backtrace: %s" (Printexc.to_string exn)
           backtrace
        )
    in
    let status_code = Status.Error in
    match span.status.status_code with
    | Unset ->
        {span with status= {status_code; description}}
    | _ ->
        span

  let set_ok span =
    let description = None in
    let status_code = Status.Ok in
    match span.status.status_code with
    | Unset ->
        {span with status= {status_code; description}}
    | _ ->
        span

  let to_string s = Rpcmarshal.marshal t.Rpc.Types.ty s |> Jsonrpc.to_string

  let of_string s =
    Jsonrpc.of_string s
    |> Rpcmarshal.unmarshal t.Rpc.Types.ty
    |> Result.to_option
end

module Spans = struct
  let lock = Mutex.create ()

  let spans = Hashtbl.create 100

  let finished_spans = Hashtbl.create 100

  let add_to_spans ~(span : Span.t) =
    Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
        let key = span.context.trace_id in
        match Hashtbl.find_opt spans key with
        | None ->
            Hashtbl.add spans key [span]
        | Some span_list ->
            if List.length span_list < 1000 then
              Hashtbl.replace spans key (span :: span_list)
    )

  let mark_finished ~(span : Span.t) =
    Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
        let key = span.context.trace_id in
        match Hashtbl.find_opt spans key with
        | None ->
            debug "Span does not exist or already finished"
        | Some span_list -> (
          match
            List.filter (fun x -> x.Span.context <> span.context) span_list
          with
          | [] ->
              Hashtbl.remove spans key
          | filtered_list -> (
              Hashtbl.replace spans key filtered_list ;
              match Hashtbl.find_opt finished_spans key with
              | None ->
                  Hashtbl.add finished_spans key [span]
              | Some span_list ->
                  if List.length span_list < 1000 then
                    Hashtbl.replace finished_spans key (span :: span_list)
            )
        )
    )

  let assert_finished x =
    match x with
    | None ->
        false
    | Some (span : Span.t) -> (
      match Hashtbl.find_opt finished_spans span.context.trace_id with
      | None ->
          false
      | Some span_list ->
          List.exists (fun x -> x = span) span_list
    )

  let since () =
    Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
        let copy = Hashtbl.copy finished_spans in
        Hashtbl.clear finished_spans ;
        copy
    )

  module GC = struct
    let lock = Mutex.create ()

    let span_timeout = ref 86400.

    let span_timeout_thread = ref None

    let gc_inactive_spans () =
      Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
          Hashtbl.iter
            (fun _ (spanlist : Span.t list) ->
              List.iter
                (fun (span : Span.t) ->
                  let elapsed = Unix.gettimeofday () -. span.begin_time in
                  if elapsed > !span_timeout *. 1000000. then
                    debug "Tracing: Span %s timed out, forcibly finishing now"
                      span.context.span_id ;
                  let span =
                    Span.finish ~span
                      ~tags:
                        [("gc_inactive_span_timeout", string_of_float elapsed)]
                      ()
                  in
                  mark_finished ~span
                )
                spanlist
            )
            spans
      )

    let initialise_thread ~timeout =
      span_timeout := timeout ;
      span_timeout_thread :=
        Some
          (Thread.create
             (fun () ->
               while true do
                 debug "Tracing: Span garbage collector" ;
                 Thread.delay !span_timeout ;
                 gc_inactive_spans ()
               done
             )
             ()
          )
  end
end

module Tracer = struct
  type t = {name: string; provider: provider_config_t ref}

  let create ~name ~provider = {name; provider}

  let no_op =
    let provider =
      ref
        {
          name_label= ""
        ; tags= []
        ; endpoints= []
        ; filters= []
        ; processors= []
        ; enabled= false
        }
    in
    {name= ""; provider}

  let span_of_span_context context name : Span.t =
    {
      context
    ; status= {status_code= Status.Unset; description= None}
    ; name
    ; parent= None
    ; span_kind= SpanKind.Client (* This will be the span of the client call*)
    ; begin_time= Unix.gettimeofday ()
    ; end_time= None
    ; tags= []
    }

  let start ?(span_kind = SpanKind.Internal) ~tracer:t ~name ~parent () :
      (Span.t option, exn) result =
    let provider = !(t.provider) in
    (* Do not start span if the TracerProvider is diabled*)
    if not provider.enabled then
      Ok None
    else
      let tags = provider.tags in
      let span = Span.start ~tags ~name ~parent ~span_kind () in
      Spans.add_to_spans ~span ; Ok (Some span)

  let finish ?error (span : Span.t option) : (unit, exn) result =
    let _ =
      Option.map
        (fun span ->
          let span =
            match error with
            | Some exn ->
                Span.set_error span exn
            | None ->
                Span.set_ok span
          in
          let span = Span.finish ~span () in
          Spans.mark_finished ~span
        )
        span
    in
    Ok ()

  let assert_finished x = Spans.assert_finished x
end

module TracerProvider = struct
  type t = {tracers: Tracer.t list; config: provider_config_t}

  let get_tracer ~provider:t ~name =
    match
      List.filter (fun (tracer : Tracer.t) -> tracer.name = name) t.tracers
    with
    | [tracer] ->
        tracer
    | _ ->
        Tracer.no_op

  let endpoints_of t = t.config.endpoints
end

let lock = Mutex.create ()

let tracer_providers = Hashtbl.create 100

let set_default ~tags ~endpoints ~processors ~filters =
  let endpoints = List.map endpoint_of_string endpoints in
  let default : TracerProvider.t =
    {
      tracers= []
    ; config=
        {
          name_label= "default"
        ; tags= ("provider", "default") :: tags
        ; endpoints
        ; filters
        ; processors
        ; enabled= true
        }
    }
  in
  Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
      Hashtbl.replace tracer_providers "default" default
  )

let get_tracer_providers () =
  Hashtbl.fold (fun _ provider acc -> provider :: acc) tracer_providers []

let get_default () =
  try
    Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
        let provider = Hashtbl.find tracer_providers "default" in
        Ok provider
    )
  with e -> Error e

let get_tracer ~name =
  let providers =
    Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
        Hashtbl.fold (fun _k v acc -> v :: acc) tracer_providers []
    )
  in
  match providers with
  | provider :: _ ->
      Tracer.create ~name ~provider:(ref provider.TracerProvider.config)
  | [] ->
      warn "No provider found" ; Tracer.no_op

let enable_span_garbage_collector ?(timeout = 86400.) () =
  Spans.GC.initialise_thread ~timeout

module Export = struct
  module Content = struct
    module Json = struct
      module Zipkinv2 = struct
        module ZipkinSpan = struct
          type localEndpoint = {serviceName: string} [@@deriving rpcty]

          type t = {
              id: string
            ; traceId: string
            ; parentId: string option
            ; name: string
            ; timestamp: int
            ; duration: int
            ; kind: string option
            ; localEndpoint: localEndpoint
            ; tags: (string * string) list
          }
          [@@deriving rpcty]

          type t_list = t list [@@deriving rpcty]

          let kind_to_zipkin_kind = function
            | SpanKind.Internal ->
                None
            | k ->
                Some k

          let json_of_t_list s =
            Rpcmarshal.marshal t_list.Rpc.Types.ty s |> Jsonrpc.to_string
        end

        let zipkin_span_of_span : Span.t -> ZipkinSpan.t =
         fun s ->
          {
            id= s.context.span_id
          ; traceId= s.context.trace_id
          ; parentId= Option.map (fun x -> x.Span.context.span_id) s.parent
          ; name= s.name
          ; timestamp= int_of_float (s.begin_time *. 1000000.)
          ; duration=
              Option.value s.end_time ~default:(Unix.gettimeofday () *. 1000000.)
              -. s.begin_time
              |> ( *. ) 1000000.
              |> int_of_float
          ; kind=
              Option.map SpanKind.to_string
                (ZipkinSpan.kind_to_zipkin_kind s.span_kind)
          ; localEndpoint= {serviceName= "xapi"}
          ; tags= s.tags
          }

        let content_of (spans : Span.t list) =
          List.map zipkin_span_of_span spans |> ZipkinSpan.json_of_t_list
      end
    end
  end

  module Destination = struct
    module File = struct
      let trace_log_dir = "/var/log/dt/zipkinv2/json"

      let export ~trace_id ~span_json ~path : (string, exn) result =
        try
          (* TODO: *)
          let host_id = "" in
          let timestamp = Unix.gettimeofday () in
          let timestamp_ms =
            timestamp *. 1000000. |> int_of_float |> string_of_int
          in
          let microsec =
            String.sub timestamp_ms 6 (String.length timestamp_ms - 6)
          in
          let unix_time = timestamp |> Unix.localtime in
          let date =
            Printf.sprintf "%d%02d%d-%d%d%d-%s"
              (Int.rem unix_time.tm_year 100)
              (unix_time.tm_mon + 1) unix_time.tm_mday unix_time.tm_hour
              unix_time.tm_min unix_time.tm_sec microsec
          in
          let file =
            String.concat "/" [path; trace_id; "xapi"; host_id; date] ^ ".json"
          in
          Xapi_stdext_unix.Unixext.mkdir_rec (Filename.dirname file) 0o700 ;
          Xapi_stdext_unix.Unixext.write_string_to_file file span_json ;
          Ok ""
        with e -> Error e
    end

    module Http = struct
      module Request = Cohttp.Request.Make (Cohttp_posix_io.Buffered_IO)
      module Response = Cohttp.Response.Make (Cohttp_posix_io.Buffered_IO)

      let export ~span_json ~url : (string, exn) result =
        try
          let body = span_json in
          let uri = Uri.of_string url in
          let headers =
            Cohttp.Header.of_list
              [
                ("accepts", "application/json")
              ; ("content-type", "application/json")
              ; ("content-length", string_of_int (String.length body))
              ]
          in
          Open_uri.with_open_uri uri (fun fd ->
              let request =
                Cohttp.Request.make ~meth:`POST ~version:`HTTP_1_1 ~headers uri
              in
              let ic = Unix.in_channel_of_descr fd in
              let oc = Unix.out_channel_of_descr fd in
              Request.write
                (fun writer -> Request.write_body writer body)
                request oc ;
              Unix.shutdown fd Unix.SHUTDOWN_SEND ;
              match try Response.read ic with _ -> `Eof with
              | `Eof ->
                  Ok ""
              | `Invalid x ->
                  Error (Failure ("invalid read: " ^ x))
              | `Ok response ->
                  let body = Buffer.create 128 in
                  let reader = Response.make_body_reader response ic in
                  let rec loop () =
                    match Response.read_body_chunk reader with
                    | Cohttp.Transfer.Chunk x ->
                        Buffer.add_string body x ; loop ()
                    | Cohttp.Transfer.Final_chunk x ->
                        Buffer.add_string body x
                    | Cohttp.Transfer.Done ->
                        ()
                  in
                  loop () ;
                  Ok (Buffer.contents body)
          )
        with e -> Error e
    end

    let export_to_endpoint endpoint =
      try
        debug "Tracing: About to export" ;
        let span_list = Spans.since () in
        Hashtbl.iter
          (fun trace_id span_list ->
            let zipkin_spans = Content.Json.Zipkinv2.content_of span_list in
            match
              match endpoint with
              | Url url ->
                  Http.export ~span_json:zipkin_spans ~url
              | Bugtool ->
                  File.export ~trace_id ~span_json:zipkin_spans
                    ~path:File.trace_log_dir
            with
            | Ok _ ->
                ()
            | Error e ->
                raise e
          )
          span_list
      with e -> debug "Tracing: ERROR %s" (Printexc.to_string e)

    let _ =
      enable_span_garbage_collector () ;
      Thread.create
        (fun () ->
          while true do
            debug "Tracing: Waiting 30s before exporting spans" ;
            Thread.delay 30. ;
            get_tracer_providers ()
            |> List.iter (fun x ->
                   TracerProvider.endpoints_of x |> List.iter export_to_endpoint
               )
          done
        )
        ()
  end
end
