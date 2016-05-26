module Option : sig
  val (>>>) : 'a option * 'a option-> ('a -> 'b) -> 'b option
  val (>|=) : 'a option -> ('a -> 'b) -> 'b option
  val (>>) : 'a option -> (('a -> 'b) option) -> 'b option
  val (|?) : 'a option -> 'a -> 'a
end
