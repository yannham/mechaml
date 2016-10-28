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
type http_status_code = Cohttp.Code.status_code
type http_headers = Cohttp.Header.t

val init : unit -> t

val get : Uri.t -> t -> t Lwt.t
val click : Page.Link.t -> t -> t Lwt.t
val post : Uri.t -> string -> t -> t Lwt.t
val submit : Page.Form.t -> t -> t Lwt.t
val load : Page.Image.t -> t -> t Lwt.t 

val page : t -> Page.t option
val content : t -> string
val server_headers : t -> http_headers 
val status_code : t -> http_status_code
val code_of_status : http_status_code -> int

val set_proxy : ?user:string
  -> ?password:string
  -> host:string
  -> port:int
  -> t -> t

val disable_proxy : t -> t

val cookie_jar : t -> Cookiejar.t
val set_cookie_jar : Cookiejar.t -> t -> t
val add_cookie : Cookiejar.Cookie.t -> t -> t
val remove_cookie : Cookiejar.Cookie.t -> t -> t

val client_headers : t -> Cohttp.Header.t
val set_client_headers : Cohttp.Header.t -> t -> t
val add_client_header : string -> string -> t -> t
val remove_client_header : string -> t -> t
