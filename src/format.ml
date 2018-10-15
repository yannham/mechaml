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

module type S = sig
  type t

  val to_string : t -> string
end

module Color = struct
  type t = {red : int; green : int; blue : int}

  let check color =
    let check_bound x = x >= 0 && x <= 255 in
    if check_bound color.red && check_bound color.green && check_bound color.blue
    then Some color
    else None

  let make ~red ~green ~blue = check {red;green;blue}

  let to_string color =
    Printf.sprintf "#%02X%02X%02X" color.red color.green
      color.blue
end

module Date = struct
  type t = {day : int; month : int; year : int}

  let check date =
    let leap_year =
      date.year mod 400 = 0
      || (date.year mod 4 = 0 && date.year mod 100 <> 0) in
    let maxday = match date.month with
      | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
      | 4 | 6 | 9 | 11 -> 30
      | 2 when leap_year -> 29
      | 2 -> 28
      | _ -> -1 in
    if date.month >= 1 && date.month <= 12
      && date.year > 0
      && date.day >= 1 && date.day <= maxday
    then Some date
    else None

  let make ~day ~month ~year = check {day; month; year}

  let to_string date =
    Printf.sprintf "%d-%02d-%02d" date.year date.month date.day
end

module Time = struct
  type t = {hour : int; minute : int; second : int}

  let check time =
    if time.hour >= 0 && time.hour <= 23
      && time.minute >= 0 && time.minute <= 59
      && time.second >= 0 && time.second <= 59
    then Some time
    else None

  let make ~hour ~minute ~second = check {hour; minute; second}

  let to_string time =
    Printf.sprintf "%02d:%02d:%02d" time.hour time.minute time.second
end
