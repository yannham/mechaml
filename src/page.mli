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

module Form : sig
  type t

  (* field types *)
  type checkbox
  type radio_button
  type select_list
  type menu
  type field
  type file_upload

  type _ input
  type _ inputs

  val to_node : _ input -> Soup.element Soup.node
  val to_nodes : _ inputs -> Soup.element Soup.nodes

  val to_list : 'a inputs -> 'a input list
  val iter : ('a input -> unit) -> 'a inputs -> unit
  val fold : ('a -> 'b input -> 'a) -> 'a -> 'b inputs -> 'a
  val filter : ('a input -> bool) -> 'a inputs -> 'a inputs

  val raw_set : string -> string list -> t -> t
  val raw_get : string -> t ->string list
  val raw_unset : string -> t -> t
  val raw_values : t -> (string * string list) list

  val name : (_ input) -> string option
  val value : (_ input) -> string option

  val checkbox_with : string -> t -> checkbox input option
  val checkboxes : t -> checkbox inputs
  val checkboxes_with : string -> t -> checkbox inputs

  val radio_button_with : string -> t -> radio_button input option
  val radio_buttons : t -> radio_button inputs
  val radio_buttons_with : string -> t -> radio_button inputs

  val select_list_with : string -> t -> select_list input option
  val select_lists : t -> select_list inputs
  val select_lists_with : string -> t -> select_list inputs

  val field_with : string -> t -> field input option
  val fields : t -> field inputs
  val fields_with : string -> t -> field inputs

  val text_with : string -> t -> field input option
  val texts : t -> field inputs
  val texts_with : string -> t -> field inputs

  val password_with : string -> t -> field input option
  val passwords : t -> field inputs
  val passwords_with : string -> t -> field inputs

  val hidden_with : string -> t -> field input option
  val hiddens : t -> field inputs
  val hiddens_with : string -> t -> field inputs

  val int_with : string -> t -> field input option
  val ints : t -> field inputs
  val ints_with : string -> t -> field inputs

  val textarea_with : string -> t -> field input option
  val textareas : t -> field inputs
  val textareas_with : string -> t -> field inputs

  val keygen_with : string -> t -> field input option
  val keygens : t -> field inputs
  val keygens_with : string -> t -> field inputs

  val file_upload_with : string -> t -> file_upload input option
  val file_uploads : t -> file_upload inputs
  val file_uploads_with : string -> t -> file_upload inputs

  val reset : t -> t

  module Checkbox : sig
    val check : t -> checkbox input -> t
    val uncheck : t -> checkbox input -> t
    val is_checked : t -> checkbox input -> bool
  end

  module RadioButton : sig
    val value : radio_button input -> string
    val values : t -> radio_button input -> radio_button input list 
    val select : t -> radio_button input -> t
    val is_selected : t -> radio_button input -> bool
  end

  module SelectList : sig
    type item

    val items : select_list input -> item list
    val select : t -> select_list input -> item -> t
    val unselect : t -> select_list input -> item -> t
    val is_selected : t -> select_list input -> item -> bool
    val to_string : item -> string
  end

  module Menu : sig
    type item

    val items : menu input -> item list
    val select : t -> menu input -> item -> t
    val unselect : t -> menu input -> item -> t
    val is_selected : t -> select_list input -> item -> bool
    val to_string : item -> string
  end

  module Field : sig
    val set : t -> field input -> string -> t
    val get : t -> field input -> string option
  end

  module FileUpload : sig
    val select : t -> file_upload input -> string -> t
    val which_selected : t -> file_upload input -> string option
  end
end

module Link : sig
  type t

  val href : t -> string
  val text : t -> string option
  val uri : t -> Uri.t

  val make : ?text:string -> href:string -> t

  val to_node : t -> Soup.element Soup.node
end

module Image : sig
  type t

  val source : t -> string
  val uri : t -> Uri.t

  val make : source:string -> t

  val to_node : t -> Soup.element Soup.node
end

module Frame : sig
  type t

  val source : t -> string
  val uri : t -> Uri.t
  val name : t -> string option
  val text : t -> string option

  val make : ?name:string -> ?text:string -> source:string -> t

  val to_node : t -> Soup.element Soup.node
end

val form_with : string -> t -> Form.t option
val forms_with : string -> t -> Form.t list
val forms : t -> Form.t list

val link_with : string -> t -> Image.t option
val links_with : string -> t -> Image.t list
val links : t -> Image.t list

val image_with : string -> t -> Image.t option
val images_with : string -> t -> Image.t list
val images : t -> Image.t list

val frame_with : string -> t -> Image.t option
val frames_with : string -> t -> Image.t list
val frames : t -> Image.t list

val to_soup : t -> Soup.soup Soup.node
val from_soup : Soup.soup Soup.node -> t
