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

    It is built on top of Cohttp, Lwt and Lambdasoup.
*)


type t
type http_status_code = Cohttp.Code.status_code
type http_headers = Cohttp.Header.t

(** {2 Operations on HTTP responses } *)

(** The HttpResponse module defines a type and operations to extract content and
   metadata from the server response *)
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
   an HTTP 302 or 303 response code, to avoid a redirect loop. Set
   to [0] to disable automatic redirection.  *)
val init : ?max_redirect:int -> unit -> t

(** The following functions perform a get request to the specified URI.
   [get "http://www.site/some/url" agent] sends a HTTP GET request and return
   the updated state of the agent together with the server response *)

val get : string -> t -> result Lwt.t
val get_uri : Uri.t -> t -> result Lwt.t

(** Same as get, but work directly with links instead of URIs *)
val click : Page.Link.t -> t -> result Lwt.t

(** The following functions send a raw post request to the specified URI *)

val post : string -> string -> t -> result Lwt.t
val post_uri : Uri.t -> string -> t -> result Lwt.t

(** Submit a filled form *)
val submit : Page.Form.t -> t -> result Lwt.t

(** Save some downloaded content in a file *)

(** [save_image "/path/to/myfile.jpg" image agent] loads the image using [get],
   opens [myfile.jpg], write the content in asynchronously and then returns the
   result *)
val save_image : string -> Page.Image.t -> t -> result Lwt.t

(** [save_content "/path/to/myfile.html" content] writes the specified content in a file
    using Lwt asynchronous I/O *)
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

(** Add a single key/value pair to the default headers *)
val add_client_header : string -> string -> t -> t

(** Remove a single key/value pair from the default headers *)
val remove_client_header : string -> t -> t

(** {6 Redirection} *)

(** Set the maximum consecutive redirections (to avoid infinite
   loops). Use [0] to disable automatic redirection) *)
val set_max_redirect : int -> t -> t

(** The default maximum consecutive redirections *)
val default_max_redirect : int

(** {7 The Agent Monad}
    This module defines a monad that implicitly manages the state corresponding
    to the agent while being inside the Lwt monad. This is basically the state
    monad (for {! Agent.t}) and the Lwt one stacked *)

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

  (** The Syntax module implements the monadic operators provided by OCaml 4.08 *)
  module Syntax: sig
    val (let*) : 'a m -> ('a -> 'b m) -> 'b m

    (** The semantic of [( and* )] is the same as in {!  Agent.Monad.List.iter_p},
        that is the two elements are fed with the same initial agent and
        evaluated in parallel. As there is no reason to choose one state over the
        other as a result, we restore the state to the same initial value.
        *)
    val (and*) : 'a m -> 'b m -> ('a * 'b) m

    val (let+) : 'a m -> ('a -> 'b) -> 'b m
    val (and+) : 'a m -> 'b m -> ('a * 'b) m
  end

  (** The List module wraps the functions of the Lwt_list module inside
     the Agent monad. Functions suffixed with _s chain the actions sequentially,
     passing around the updated agent from an action to the next one. The
     functions suffixed with _m do everything in parallel, using a copy of the
     initial state in every thread and returning this same original state.  The
     latter can be useful to retrieve a bunch of resources in batch where the
     updated state is not of interest (e.g download images) *)
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

  (** Get the current state of the agent, or set a new one *)

  val get : t m
  val set : t -> unit m

  (** Wrap the type of functions operating on the agent such as {!  Agent.cookie_jar}
    or {!  Agent.set_cookie_jar} to be usable inside the monad. For example, the
    first one go from [Agent.t -> Cookiejar.t] to
    [Agent.t -> (Agent.t * Cookiejar.t) Lwt.t] by just returning the agent
    unmodified together with the current cookie jar and wrap the result through
    [Lwt.return]

    Note that the redefined functions have the same name as their counterpart,
    and thus will shadow or can be shadowed by them. *)

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
