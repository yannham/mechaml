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

(** Helpers for the option Monad *)

(** Monadic operations *)

val return : 'a -> 'a option
val bind : 'a option -> ('a -> 'b option) -> 'b option
val join : ('a option) option -> 'a option
val map : ('a -> 'b) -> 'a option -> 'b option

(** Apply a two arguments function to a pair of optionals : if one of the
     component is [None], the result is [None], otherwise [map2 (Some x, Some y)
     f] is [Some (f x y)] *)
val map_pair : 'a option * 'b option -> ('a -> 'b -> 'c) -> 'c option

(** Return the content of an optional, or the given default value if the first
     argument is [None]. *)
val default : 'a option -> 'a -> 'a

module Infix : sig
  (** bind *)
  val (>>=) : 'a option -> ('a -> 'b option) -> 'b option

  (** Map a function and apply it to the given argument *)
  val (>|=) : 'a option -> ('a -> 'b) -> 'b option

  (** map_pair *)
  val (>>>) : 'a option * 'b option-> ('a -> 'b -> 'c) -> 'c option

  (** default *)
  val (|?) : 'a option -> 'a -> 'a
end
