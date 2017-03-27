(*{{{ Copyright (C) 2016, Yann Hamdaoui <yann.hamdaoui@centraliens.net>
  Permission to use, copy, modify, and/or distribute this software for
  any purpose with or without fee is hereby granted, provided that the
  above copyright notice and this permission notice appear in all
  copies.
  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
  WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
  CONSEQUENTIAL
  DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
  DATA
  OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
  OTHER
  TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE
  USE OR
  PERFORMANCE OF THIS SOFTWARE.
  }}}*)

(** Cookies management

    Cookiejar is the module that stores and manages cookies. A Cookiejar stores the
    list of all cookies created by the [Set-Cookie] headers sent by the server.
    It is able to automatically extract cookies from server headers (see
    {!add_from_headers}), or to select and add relevant
    coookies to the [Cookie] client header before a request (see
    {!add_to_headers}).

    It is mainly used by the {!module:Agent} module internally, but can be manipulated if
    one need to programmatically the cookies sent by the agent. *)


(** Representation of a cookie *)
module Cookie : sig
  type t
  type expiration = [
    | `Session
    | `Max_age of int64
  ]

  val name : t -> string
  val value : t -> string
  val expiration : t -> expiration
  val domain : t -> string
  val path : t -> string
  val secure : t -> bool

  (** Given an uri and a cookie, return true if the
     cookie's domain and path match the uri's one.
   *)
  val match_uri : Uri.t -> t -> bool

  (** Create a cookie *)
  val make :
    ?expiration:expiration
    -> ?path:string
    -> ?secure:bool
    -> domain:string
    -> string
    -> string -> t
end

type t

(** The emtpy cookie jar*)
val empty : t

(** Add a cookie to the jar *)
val add : Cookie.t -> t -> t

(** Remove a cookie from the jar *)
val remove : Cookie.t -> t -> t

(** Given a header received from a server, update the jar according to
    possible [Set-Cookie] HTTP header
 *)
val add_from_headers : Uri.t -> Cohttp.Header.t -> t -> t

(** Given an URI to request, update the client HTTP headers with relevant
   cookies before sending it to the server *)
val add_to_headers : Uri.t -> Cohttp.Header.t -> t -> Cohttp.Header.t

val map : (Cookie.t -> Cookie.t) -> t -> t
val iter : (Cookie.t -> unit) -> t -> unit
val fold : (Cookie.t -> 'a -> 'a) -> t -> 'a -> 'a

val is_empty : t -> bool
