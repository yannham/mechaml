(*{{{ Copyright (C) 2016, Yann Hamdaoui <yann.hamdaoui@centraliens.net>
  Permission to use, copy, modify, and/or distribute this software for any
  purpose with or without fee is hereby granted, provided that the above
  copyright notice and this permission notice appear in all copies.

  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
  REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
  AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
  INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
  OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
  TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
  THIS SOFTWARE.
  }}}*)

(** Scraping agent

    Mechaml is a web agent that allows to :

      - Fetch and parse HTML pages
      - Analyze, fill and submit HTML forms
      - Manages cookies, headers and redirections

    It is build on top of Cohttp, Lwt and Lambdasoup.
*)


type t
type http_status_code = Cohttp.Code.status_code
type http_headers = Cohttp.Header.t

(** {2 Operations on HTTP responses } *)

(** The HttpResponse module defines a type and operations to extract content and
   metadata from server response *)
module HttpResponse : sig
  type t

  val status : t -> http_status_code
  val status_code : t -> int
  val headers : t -> http_headers
  val content : t -> string
  val page : t -> Page.t
  val location : t -> Uri.t

  val cohttp_response : t -> Cohttp.Response.t
end

type result = t * HttpResponse.t

(** {3 Main operations } *)

(** Create a new empty agent. [~max_redirect] indicates how many times the agent
   will automatically and consecutively follow the [Location] header in case of
   HTTP 302 or 303 response codes to avoid a redirect loop. Set
   to [0] to disable any automatic redirection.
 *)
val init : ?max_redirect:int -> unit -> t

(** Perform a get request to the specified URI. [get "http://www.site/some/url"
   agent] sends a HTTP GET request and return the updated state of the agent
   together with the server's response *)

val get : string -> t -> result Lwt.t
val get_uri : Uri.t -> t -> result Lwt.t

(** Same as get, but work directly with links instead of URIs *)
val click : Page.Link.t -> t -> result Lwt.t

(** Send a raw post request to the specified URI *)

val post : string -> string -> t -> result Lwt.t
val post_uri : Uri.t -> string -> t -> result Lwt.t

(** Submit a filled form *)
val submit : Page.Form.t -> t -> result Lwt.t

(** Save some downloaded content in a file *)

(** [save_image "/path/to/myfile.jpg" image agent] loads the image using [get], open
   [myfile.jpg] and write the content in asynchronously, and return the result *)
val save_image : string -> Page.Image.t -> t -> result Lwt.t

(** [save_content "/path/to/myfile.html" content] write the specified content in a file
    using Lwt's asynchronous IO *)
val save_content : string -> string -> unit Lwt.t

(** {4 Cookies} (see {!module:Cookiejar}) *)

(** Return the current Cookiejar *)
val cookie_jar : t -> Cookiejar.t

(** Set the current Cookiejar *)
val set_cookie_jar : Cookiejar.t -> t -> t

(** Add a single cookie to the current Cookiejar *)
val add_cookie : Cookiejar.Cookie.t -> t -> t

(** Remove a single cookie from the Cookiejar *)
val remove_cookie : Cookiejar.Cookie.t -> t -> t

(** {5 Headers} *)

(** Return the default headers sent when performing HTTP requests *)
val client_headers : t -> Cohttp.Header.t

(** Use the specified headers as new default headers *)
val set_client_headers : Cohttp.Header.t -> t -> t

(** Add a single pair key/value to the default headers *)
val add_client_header : string -> string -> t -> t

(** Remove a single pair key/value from the default headers *)
val remove_client_header : string -> t -> t

(** {6 Redirection} *)

(** Max redirection to avoid infinite loops (use 0 to disable automatic
   redirection) *)
val set_max_redirect : int -> t -> t

(** The default maximum consecutive redirections *)
val default_max_redirect : int

(** {7 The Agent Monad}
    This module defines a monad that implicitely manages the state corresponding to the agent
    inside the Lwt monad. This is basically the state monad (for {! Agent.t}) and the Lwt one stacked *)

module Monad : sig
  type 'a m = t -> (t * 'a) Lwt.t

  val bind : 'a m -> ('a -> 'b m) -> 'b m
  val return : 'a -> 'a m
  val return_from_lwt : 'a Lwt.t -> 'a m
  val map : ('a -> 'b) -> 'a m -> 'b m

  val run : t -> 'a m -> (t * 'a)

  (** Wrappers of Lwt's fail functions *)
  val fail : exn -> 'a m
  val fail_with : string -> 'a m

  (** Wrappers of [Lwt.catch] and [Lwt.try_bind] inside this monad *)

  val catch : (unit -> 'a m) -> (exn -> 'a m) -> 'a m
  val try_bind :
    (unit -> 'a m) ->
    ('a -> 'b m) -> (exn -> 'b m) -> 'b m

  (** The Infix module defines operators for common bind operations *)
  module Infix : sig
    val (>>=) : 'a m -> ('a -> 'b m) -> 'b m
    val (=<<) : ('a -> 'b m) -> 'a m -> 'b m
    val (>>) : 'a m -> 'b m -> 'b m
    val (<<) : 'b m -> 'a m -> 'b m
    val (>|=) : 'a m -> ('a -> 'b) -> 'b m
    val (=|<) : ('a -> 'b) -> 'a m -> 'b m
  end

  (** The List module mainly wrap the Lwt_list one in the Agent monad. Functions
     suffixed with
     _s chains the actions sequentially, passing around the updated agent to the
     next one. The _m ones do everything in parallel, sending a copy of the
     initial state to every threads and returning this same unupdated state.
     The latter can be useful to retrive a bunch of ressources in batch where
     the updated state is not of interest (e.g images) *)
  module List : sig
    val iter_s : ('a -> unit m) -> 'a list -> unit m
    val iter_p : ('a -> unit m) -> 'a list -> unit m

    val iteri_s : (int -> 'a -> unit m) -> 'a list -> unit m
    val iteri_p : (int -> 'a -> unit m) -> 'a list -> unit m

    val map_s : ('a -> 'b m) -> 'a list -> 'b list m
    val map_p : ('a -> 'b m) -> 'a list -> 'b list m

    val mapi_s : (int -> 'a -> 'b m) -> 'a list -> 'b list m
    val mapi_p : (int -> 'a -> 'b m) -> 'a list -> 'b list m

    val fold_left_s : ('a -> 'b -> 'a m) -> 'a -> 'b list -> 'a m
    val fold_right_s : ('a -> 'b -> 'b m) -> 'a list -> 'b -> 'b m
  end

  (** get the current state of the agent, or set a new one *)

  val get : t m
  val set : t -> unit m

  (** Wrap the type of functions operating on the agent such as {!
      Agent.cookie_jar} or {!
      Agent.set_cookie_jar} to usable inside the monad.
      For example, the first one go from [Agent.t -> Cookiejar.t] to
      [Agent.t -> (Agent.t * Cookiejar.t) Lwt.t] by just returning the agent
      unmodified together with the current cookie jar and wrap the result in
      [Lwt.return]

      Note that the redefined functions have the same name as their counterpart,
      and thus will shadow or can be shadowed by them.
  *)

  val save_content : string -> string -> unit m

  val cookie_jar : Cookiejar.t m
  val set_cookie_jar : Cookiejar.t -> unit m
  val add_cookie : Cookiejar.Cookie.t -> unit m
  val remove_cookie : Cookiejar.Cookie.t -> unit m

  val client_headers : Cohttp.Header.t m
  val set_client_headers : Cohttp.Header.t -> unit m
  val add_client_header : string -> string -> unit m
  val remove_client_header : string -> unit m

  val set_max_redirect : int -> unit m
end
