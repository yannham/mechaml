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

type t

(* field types *)
type checkbox
type radio_button
type select_list
type field
type file_upload

type _ input
type _ inputs

val to_node : _ input -> Soup.element Soup.node
val to_nodes : _ inputs -> Soup.element Soup.nodes

val to_list : 'a inputs -> 'a input list
val iter : 'a inputs -> t
val fold : ('a -> 'b input -> 'a) -> 'a -> 'b inputs -> 'a
val filter : 'a inputs -> ('a input -> bool) -> 'a inputs

val raw_set : t -> string -> string list -> t
val raw_get : t -> string -> string list
val raw_unset : t -> string -> t -> t
val raw_values : t -> string * (string list) list

let name : _ input -> string option
let value : _ input -> string option

val checkbox_with : t -> string -> checkbox inputs option
val checkboxes : t -> checkbox inputs
val checkboxes_with : t -> string -> checkbox inputs

val radio_button_with : t -> string -> radio_button input option
val radio_buttons : t -> radio_button inputs
val radio_buttons_with : t -> string -> radio_button inputs

val select_list_with : t -> string -> select_list input option
val select_lists : t -> select_list inputs
val select_lists_with : t -> string -> select_list inputs

val field_with : t -> string -> field input option
val fields : t -> field inputs
val fields_with : t -> string -> field inputs

val text_with : t -> string -> field inputs option
val texts : t -> field inputs
val texts_with : t -> string -> field inputs

val password_with : t -> string -> field inputs option
val passwords : t -> field inputs
val passwords_with : t -> string -> field inputs

val hidden_with : t -> string -> field inputs option
val hiddens : t -> field inputs
val hiddens_with : t -> string -> field inputs

val ints : t -> string -> field inputs option
val ints : t -> field inputs
val ints : t -> string -> field inputs

let textarea_with : t -> string -> field inputs option
let textareas : t -> field inputs
let textareas_with : t -> string -> field inputs

let keygen_with : t -> string -> field inputs option
let keygens : t -> field inputs
let keygens_with : string -> t -> field inputs

val file_upload_with : t -> string -> file_upload input option
val file_uploads : t -> file_upload inputs
val file_uploads_with : t -> string -> file_upload inputs

val reset : t -> t

module Checkbox = sig
  val check : t -> checkbox input -> t
  val uncheck : t -> checkbox input -> t
  val is_checked : t -> checkbox input -> bool
end

module RadioButton = sig
  val value : radio_button -> string
  val values : t -> radio_button input -> radio_button list 
  val select : t -> radio_button input -> t
  val is_selected : t -> radio_button -> bool
end

module SelectList = sig
  type item

  val items : select_list input -> item list

  val select : t -> select_list input -> item -> t
  val unselect : t -> select_list input -> item -> t

  val is_selected : t -> select_list input -> item -> bool

  val to_string : item -> string
end

module Menu = sig
  type item

  val items : menu input -> item list

  val select : t -> menu input -> item -> t
  val unselect : t -> menu input -> item -> t

  val is_selected : t -> select_list input -> item -> bool

  val to_string : item -> string
end

module Field = sig
  val set : t -> field input -> string -> t
  val get : t -> field input -> string option
end

module FileUpload = sig
  val select : t -> file_upload input -> string -> t
  val which_selected : file_upload input -> string option
end

(* TODO : need the definition of an Agent module *)
