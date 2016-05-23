type t

(* Create a new empty cookie jar *)
val init : unit -> t

(* Given a header received from a server,
 * update the list of cookies according
 * to set-cookie HTTP header
 *)
val update_cookies : Header.t -> t -> unit

(* Given an URL, update the header parameters
 * with corresponding cookies before sending it
 * to the server *)
val include_cookies : string -> Header.t -> t -> Header.t
