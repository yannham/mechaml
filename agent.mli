type t

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
