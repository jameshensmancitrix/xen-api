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

module SpanKind : sig
  type t = Server | Consumer | Client | Producer | Internal

  val to_string : t -> string
end

module Span : sig
  type t

  val of_string : string -> t option

  val to_string : t -> string

  val set_span_kind : t -> SpanKind.t -> unit
end

module Tracer : sig
  type t

  val start :
       ?span_kind:SpanKind.t
    -> tracer:t
    -> name:string
    -> parent:Span.t option
    -> unit
    -> (Span.t option, exn) result

  val finish : Span.t option -> (unit, exn) result

  val assert_finished : Span.t option -> bool
end

module TracerProvider : sig
  type t

  val get_tracer : provider:t -> name:string -> Tracer.t
end

val set_default :
     tags:(string * string) list
  -> endpoints:string list
  -> processors:string list
  -> filters:string list
  -> unit

val get_default : unit -> (TracerProvider.t, exn) result

val get_tracer : name:string -> Tracer.t
