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

(** Format

    This module contains type definitions and helpers to deal with some of the
    HTML5 formatted input types, such as dates, colors, range, etc. Each module
    corresponds to a form input type and implements a [to_string] function that
    generates a well-formed string from an internal representation. The
    generated string may then be put in a form using [Page.Form.Field.set], for
    example. Each module provides a [make] function to create and manipulate
    this representation.
*)

(** The common interface of all formatting modules *)
module type S = sig
  type t

  val to_string : t -> string
end

(* Module for the color input type *)
module Color : sig
  include S

  (** Create a color representation from its red, green and blue components.
    Arguments must lie between 0 and 255 included, otherwise None is returned *)
  val make : red:int -> green:int -> blue:int -> t option
end

(* Module for the date input type *)
module Date : sig
  include S

  (** Create a date representation from a day, a month and a year. If the date
    is not a valid date string, [None] is returned *)
  val make : day:int -> month:int -> year:int -> t option
end

(* Module for the time input type *)
module Time : sig
  include S

  (** Create a time representation from hours, minutes and seconds. If an
    invalid time is given (e.g. hours < 0), then [None] is returned. *)
  val make : hour:int -> minute:int -> second:int -> t option
end
