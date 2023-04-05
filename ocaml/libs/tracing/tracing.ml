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

let url_file = "/etc/xapi-tracing-url"

type endpoint = Bugtool | Url of string [@@deriving rpcty]

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

module SpanContext = struct
  type t = {trace_id: string; span_id: string} [@@deriving rpcty]
end

module Span = struct
  type t = {
      context: SpanContext.t
    ; parent: t option
    ; name: string
    ; begin_time: float
    ; end_time: float option
    ; tags: (string * string) list
  }
  [@@deriving rpcty]

  let generate_id n = String.init n (fun _ -> "0123456789abcdef".[Random.int 16])

  let start ?(tags = []) ~name ~parent () =
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
    {context; parent; name; begin_time; end_time; tags}

  let finish ?(tags = []) ~span () =
    {span with end_time= Some (Unix.gettimeofday ()); tags= span.tags @ tags}

  let to_string s = Rpcmarshal.marshal t.Rpc.Types.ty s |> Jsonrpc.to_string

  let of_string s =
    Jsonrpc.of_string s
    |> Rpcmarshal.unmarshal t.Rpc.Types.ty
    |> Result.to_option
end

module Spans = struct
  let lock = Mutex.create ()

  let spans = Hashtbl.create 100

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

  let since () =
    Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
        let copy = Hashtbl.copy spans in
        Hashtbl.clear spans ; copy
    )
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

  let start ~tracer:t ~name ~parent : (Span.t option, exn) result =
    let provider = !(t.provider) in
    (* Do not start span if the TracerProvider is diabled*)
    if not provider.enabled then
      Ok None
    else
      let tags = provider.tags in
      let span = Span.start ~tags ~name ~parent () in
      Spans.add_to_spans ~span ; Ok (Some span)

  let finish (span : Span.t option) : (unit, exn) result =
    let _span = Option.map (fun span -> Span.finish ~span ()) span in
    Ok ()
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
            ; kind: string
            ; localEndpoint: localEndpoint
            ; tags: (string * string) list
          }
          [@@deriving rpcty]

          type t_list = t list [@@deriving rpcty]

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
          ; kind= "SERVER"
          ; localEndpoint= {serviceName= "xapi"}
          ; tags= s.tags
          }

        let content_of (spans : Span.t list) =
          List.map zipkin_span_of_span spans |> ZipkinSpan.json_of_t_list
      end
    end
  end

  module Destination = struct
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

    let export_to_http_server () =
      debug "Tracing: About to export to http server" ;
      let url =
        Xapi_stdext_unix.Unixext.string_of_file url_file |> String.trim
      in
      let span_list = Spans.since () in
      try
        Hashtbl.iter
          (fun _ span_list ->
            let zipkin_spans = Content.Json.Zipkinv2.content_of span_list in
            match Http.export ~span_json:zipkin_spans ~url with
            | Ok _ ->
                ()
            | Error e ->
                raise e
          )
          span_list ;
        Ok ()
      with e -> Error e

    let _ =
      Thread.create
        (fun () ->
          while true do
            debug "Tracing: Waiting 30s before exporting spans" ;
            Thread.delay 30. ;
            match export_to_http_server () with
            | Ok () ->
                ()
            | Error e ->
                debug "Tracing: Export ERROR %s" (Printexc.to_string e)
          done
        )
        ()
  end
end
