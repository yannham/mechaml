(*{{{ Copyright (C) 2016, Yann Hamdaoui <yann.hamdaoui@centraliens.net>
  Permission to use, copy, modify, and/or distribute this software for
  any purpose with or without fee is hereby granted, provided that the
  above copyright notice and this permission notice appear in all
  copies.
  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
  WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
  AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
  CONSEQUENTIAL
  DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
  DATA
  OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
  OTHER
  TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE
  USE OR
  PERFORMANCE OF THIS SOFTWARE.
  }}}*)

type t

type config = {

val init : unit -> t

val get : t -> Uri.t -> Response.t * t
val post : t -> Uri.t -> string -> Response.t * t
val submit : t -> Form.t -> Response.t * t

val page : t -> Page.t
val previous : t -> t
val next : t -> t
val history : _

val cookies : t -> CookieJar.t
val add_default_cookie : t -> CookieJar.Cookie.t -> t
val remove_default_cookie : t -> CookieJar.Cookie.t -> t

val set_default_header : t -> string -> string -> t
val set_default_headers : t -> Cohttp.Header.t -> t
val remove_default_header : t -> string -> t
val reset_default_headers : t -> t
