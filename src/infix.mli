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

(** Some infix operators for the option Monad *)
module Option : sig
  (** The monadic bind operator *)
  val (>>=) : 'a option -> ('a -> 'b option) -> 'b option

  (** The usual [map] + application : [None >|= f] is [None] and [Some x >|= f] is
     [Some (f x)] *)
  val (>|=) : 'a option -> ('a -> 'b) -> 'b option

  (** Apply a two argument function to a pair of optionals : if one of the
     component is [None], then return [None], otherwise [(Some x, Some y) >>> f] is
     [Some (f x y)] *)
  val (>>>) : 'a option * 'b option-> ('a -> 'b -> 'c) -> 'c option

  (** Return the content of an optional, or the given default value if the first
     argument is [None]. Id est [Some x |? def] is [x] and [None |? def] is
     [def]. *) 
  val (|?) : 'a option -> 'a -> 'a
end
