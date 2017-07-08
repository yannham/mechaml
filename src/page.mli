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

(** Page

    This module contains all the functions used to analyze a page, select specific
    elements, and manage forms.

*)

(** The type of an html page *)
type t

val from_soup : ?location:Uri.t -> Soup.soup Soup.node -> t
val from_string : ?location:Uri.t -> string -> t

val base_uri : t -> Uri.t
val resolver : t -> Uri.t -> Uri.t

(** Convert to Lambdasoup *)
val soup : t -> Soup.soup Soup.node

(** {2 Form} *)

(** Operations on forms and inputs *)
module Form : sig
  type t

  (** Phantom types for inputs *)
  type checkbox
  type radio_button
  type select_list
  type menu
  type field
  type file_upload

  (** A form input *)
  type _ input
  (** A (possibly lazy) list of form inputs *)
  type _ inputs

  (** Return the name of the form *)
  val name : t -> string option

  (** Return the action attribute of the form *)
  val action : t -> Uri.t

  (** Return the absolute (resolved) uri corresponding to the action attribute *)
  val uri : t -> Uri.t

  (** Return the method attribute of the form *)
  val meth : t -> [`POST | `GET]

  (** Convert a form to a Soup node *)
  val to_node : t -> Soup.element Soup.node

  (** Convert an input to a Soup node *)
  val input_to_node : _ input -> Soup.element Soup.node

  (** Convert an input list to a Soup nodes list *)
  val input_to_nodes : _ inputs -> Soup.element Soup.nodes

  (** Convert the lazy input list to a native OCaml list *)
  val to_list : 'a inputs -> 'a input list

  (** Operations on a lazy input list *)
  val iter : ('a input -> unit) -> 'a inputs -> unit
  val fold : ('a -> 'b input -> 'a) -> 'a -> 'b inputs -> 'a
  val filter : ('a input -> bool) -> 'a inputs -> 'a inputs

  (** Set directly the values of a field *)
  val set : string -> string list -> t -> t

  (** Get the value of a field *)
  val get : string -> t -> string list

  (** Delete the value of a field *)
  val unset : string -> t -> t

  (** Return all set values as a list *)
  val values : t -> (string * string list) list

  (** Return the name of an input *)
  val iname : (_ input) -> string option

  (** All the following function are built using the same pattern.

      - xxxs (eg {!checkboxes}) return all the input of a certain type.
      For example, [checkboxes myform] will return all the checkboxes of the form
      - xxx_with take a CSS selector as parameter, and return the first input that
      matches the selector, or [None] if there isn't any. Eg, [fields_with myform
      "\[name$=text2\]"] will try to find any text field which name ends with {v text2
      v}
      - xxxs_with proceed as the previous form, but return all inputs matching the
      selector.

  *)

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

  (** Operation on Checkboxes *)
  module Checkbox : sig
    (** Return the value (the label) of a checkbox *)
    val value : checkbox input -> string

    (** Return the values of all the checkboxes with the same name that the
       given one *)
    val values : t -> checkbox input -> string list

    (** Return all the checkboxes with the same name that the given one *)
    val choices : t -> checkbox input -> checkbox inputs

    (** Return the list of all checked checkboxes with the same name that the
       given one *)
    val checked : t -> checkbox input -> string list

    val check : t -> checkbox input -> t
    val uncheck : t -> checkbox input -> t
    val is_checked : t -> checkbox input -> bool
  end

  (** Operations on Radio Buttons *)
  module RadioButton : sig

    (** Similar to checkboxes, except that selecting one radio button in group
       automatically unselect the others *)

    val value : radio_button input -> string
    val values : t -> radio_button input -> string list
    val choices : t -> radio_button input -> radio_button inputs

    (** Return the possibly selected radio button *)
    val selected : t -> checkbox input -> string option

    val select : t -> radio_button input -> t
    val is_selected : t -> radio_button input -> bool
  end

  (** Operations on Menu (select lists) *)
  module SelectList : sig
    (** Represent an item of the list *)
    type item

    (** Return a list of all items of a given list *)
    val items : select_list input -> item list

    val selected : t -> select_list input -> string option
    val select : t -> select_list input -> item -> t
    val unselect : t -> select_list input -> item -> t
    val is_selected : t -> select_list input -> item -> bool

    (** Label of an item *)
    val to_string : item -> string
  end

  (** Operations on text fields : textarea, text/password type input, etc. *)
  module Field : sig
    val set : t -> field input -> string -> t
    val get : t -> field input -> string option
  end

  (** Operation on file upload fields *)
  module FileUpload : sig
    val select : t -> file_upload input -> string -> t
    val which_selected : t -> file_upload input -> string option
  end
end

(** Operations on hypertext links *)
module Link : sig
  type t

  val href : t -> string
  val text : t -> string option
  val uri : t -> Uri.t

  val make : ?resolver:(Uri.t -> Uri.t) -> ?text:string -> href:string -> t

  val to_node : t -> Soup.element Soup.node
end

(** Operations on images *)
module Image : sig
  type t

  val source : t -> string
  val uri : t -> Uri.t

  val make : ?resolver:(Uri.t -> Uri.t) -> source:string -> t

  val to_node : t -> Soup.element Soup.node
end

(* module Frame : sig *)
(*   type t *)
(*  *)
(*   val source : t -> string *)
(*   val uri : t -> Uri.t *)
(*   val name : t -> string option *)
(*   val text : t -> string option *)
(*  *)
(*   val make : ?name:string -> ?text:string -> source:string -> t *)
(*  *)
(*   val to_node : t -> Soup.element Soup.node *)
(* end *)

(** All the following function are built using the same pattern.

    - xxxs (eg {!forms}) return all the element of a certain type.
    For example, [forms mypage] will return all the form in the page 
    - xxx_with take a CSS selector as parameter, and return the first element that
    matches the selector, or [None] if there isn't any. Eg, [link_with
    "\[href$=.jpg\]" mypage] will try to find a link that point to a JPEG image
    - xxxs_with proceed as the previous one, but return all elements matching the
    selector.

*)

val form_with : string -> t -> Form.t option
val forms_with : string -> t -> Form.t list
val forms : t -> Form.t list

val link_with : string -> t -> Link.t option
val links_with : string -> t -> Link.t list
val links : t -> Link.t list

val image_with : string -> t -> Image.t option
val images_with : string -> t -> Image.t list
val images : t -> Image.t list

(* val frame_with : string -> t -> Frame.t option *)
(* val frames_with : string -> t -> Frame.t list *)
(* val frames : t -> Frame.t list *)
