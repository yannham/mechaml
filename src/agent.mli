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

(** Scraping agent

    Mechaml is a web agent that allows to :

      - Fetch and parse HTML pages
      - Analyze, fill and submit HTML forms
      - Manages cookies, headers and redirections
      - Use a proxy (soon to be implemented)

    It is build on top of Cohttp, Lwt and Lambdasoup.
*)


type t
type http_status_code = Cohttp.Code.status_code
type http_headers = Cohttp.Header.t
type http_meth = Cohttp.Code.meth

(** {2 Main operations } *)

(** Create a new empty agent. [~max_redirect] indicates how many times the agent
   will automatically and consecutively follow the [Location] header in case of
   HTTP 302 or 303 response codes to avoid a redirect loop. Set
   to [0] to disable any automatic redirection.
 *)
val init : ?max_redirect:int -> unit -> t

(** Perform a get request to the specified URI *)

val get : string -> t -> t Lwt.t
val get_uri : Uri.t -> t -> t Lwt.t

(** Same as get, but work directly with links instead of URIs *)
val click : Page.Link.t -> t -> t Lwt.t

(** Send a raw post requet to the specified URI *)

val post : string -> string -> t -> t Lwt.t
val post_uri : Uri.t -> string -> t -> t Lwt.t

(** Submit a filled form *)
val submit : Page.Form.t -> t -> t Lwt.t

(** Send a get request to retrieve an image, but discard the content *)
val load : Page.Image.t -> t -> t Lwt.t

(** {3 Response} *)

(** Return the last URI requested, or an empty one if none  *)
val uri : t -> Uri.t

(** Return the method used to retrieve the content, or [`Other "None"] if none
  *)
val meth : t -> http_meth

(** Return the last page, or None if none or any error ocurred during HTML
   parsing *)
val page : t -> Page.t option

(** Return the raw content of the last response as a string, or an empty string
   if none *)
val content : t -> string

(** Return the headers sent by the last response, or empty headers if none *)
val server_headers : t -> http_headers

(** Return the HTTP code of the last reponse, or [`Code (-1)] if none *)
val status_code : t -> http_status_code

(** Convert a code to the corresponding int code *)
val code_of_status : http_status_code -> int

(** {4 Proxy} *)

(** Proxy are currently NOT SUPPORTED YET *)

val set_proxy : ?user:string
  -> ?password:string
  -> host:string
  -> port:int
  -> t -> t

val disable_proxy : t -> t

(** {5 Cookies} (see {!module:Cookiejar}) *)

(** Return the current Cookiejar *)
val cookie_jar : t -> Cookiejar.t

(** Change the current Cookiejar *)
val set_cookie_jar : Cookiejar.t -> t -> t

(** Add a single cookie to the current Cookiejar *)
val add_cookie : Cookiejar.Cookie.t -> t -> t

(** Remove a single cookie from the Cookiejar *)
val remove_cookie : Cookiejar.Cookie.t -> t -> t

(** {6 Headers} *)

(** Return the default headers sent when performing HTTP requests *)
val client_headers : t -> Cohttp.Header.t

(** Use the specified headers as new default headers *)
val set_client_headers : Cohttp.Header.t -> t -> t

(** Add a single pair key/value to the default headers *)
val add_client_header : string -> string -> t -> t

(** Remove a single pair key/value from the default headers *)
val remove_client_header : string -> t -> t

(** {7 Redirection} *)

(** Max redirection to avoid infinite loops (use 0 to disable automatic
   redirection) *)
val set_max_redirect : int -> t -> t

(** The default maximum consecutive redirections. Used to avoid redirect loops *)
val default_max_redirect : int
