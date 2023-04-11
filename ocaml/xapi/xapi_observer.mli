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

val observed_hosts_of :
  __context:Context.t -> API.ref_host list -> API.ref_host list

val create :
     __context:Context.t
  -> name_label:string
  -> name_description:string
  -> hosts:API.ref_host list
  -> attributes:(string * string) list
  -> endpoints:string list
  -> components:string list
  -> enabled:bool
  -> API.ref_Observer

val create' :
     ?reg_fn:(unit -> unit)
  -> __context:Context.t
  -> name_label:string
  -> name_description:string
  -> hosts:API.ref_host list
  -> attributes:(string * string) list
  -> endpoints:string list
  -> components:string list
  -> enabled:bool
  -> unit
  -> API.ref_Observer

val register :
  __context:Context.t -> self:API.ref_Observer -> host:API.ref_host -> unit

val unregister :
  __context:Context.t -> self:API.ref_Observer -> host:API.ref_host -> unit

val destroy : __context:Context.t -> self:API.ref_Observer -> unit

val destroy' :
     ?dest_fn:(unit -> unit)
  -> __context:Context.t
  -> self:API.ref_Observer
  -> unit
  -> unit

val set_hosts :
     __context:Context.t
  -> self:API.ref_Observer
  -> value:API.ref_host list
  -> unit

val set_enabled :
  __context:Context.t -> self:API.ref_Observer -> value:bool -> unit

val set_attributes :
     __context:Context.t
  -> self:API.ref_Observer
  -> value:(string * string) list
  -> unit

val set_endpoints :
  __context:Context.t -> self:API.ref_Observer -> value:string list -> unit

val set_components :
  __context:Context.t -> self:API.ref_Observer -> value:string list -> unit
