(* Internal representation of a cookie *)
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
  val path : t -> string option
  val secure : t -> bool

  (* Given an uri and a cookie, return true if the
   * cookie's domain and path matches the uri's one.
   *)
  val match_uri : Uri.t -> t -> bool

  val make :
    ?expiration:expiration ->
    ?path:string ->
    ?secure:bool -> ~domain:string
    ~name:string -> ~value:string -> t
end

type t

(* The emtpy cookie jar*)
val empty : t

(* Add a plain cookie to the jar *)
val add : Cookie.t -> t -> t
(* Remove a plain cookie from the jar *)
val remove : Cookie.t -> t -> t

(* Given a header received from a server,
 * update the jar according
 * to set-cookie HTTP header
 *)
val add_from_headers : Uri.t -> Header.t -> t -> t

(* Given an URI, update the HTTP headers parameter
 * with corresponding cookies before sending it
 * to the server *)
val add_to_headers : Uri.t -> Header.t -> t -> Header.t

val map : (Cookie.t -> Cookie.t) -> t -> t
val iter : (Cookie.t -> unit) -> t -> unit
val fold : (Cookie.t -> 'a -> 'a) -> t -> 'a -> 'a
