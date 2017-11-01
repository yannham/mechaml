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

(** Page

    This module contains all the functions used to analyze a page, select specific
    elements, and manage forms.

*)

(** The type of an html page *)
type t

(** Make a new page from a base URI and a Lambdasoup document *)
val from_soup : ?location:Uri.t -> Soup.soup Soup.node -> t

(** Make a new page from a base URI and a HTML string *)
val from_string : ?location:Uri.t -> string -> t

(** Return the location of a page (or [Uri.empty] if not specified) *)
val base_uri : t -> Uri.t

(** Return the resolver of page, that take relative URIs to absolute ones using
   the page base URI *)
val resolver : t -> Uri.t -> Uri.t

(** Convert to Lambdasoup *)
val soup : t -> Soup.soup Soup.node

(** {2 Lazy sequences}
    Lambdasoup provides lazy sequences to traverse only needed part of an HTML
    document when used in combination with [with_stop]. We provide a wrapper
    that is compatible with Mechaml types such as forms, images, inputs, etc. *)

(** Lazy sequences of HTML elements. See [Soup.nodes] type *)
type +'a seq

(** [Soup.stop] type *)
type 'a stop = 'a Soup.stop = { throw : 'b. 'a -> 'b }

(** Operations on lazy sequences *)

val iter : ('a -> unit) -> 'a seq -> unit
val fold : ('a -> 'b -> 'a) -> 'a -> 'b seq -> 'a
val filter : ('a -> bool) -> 'a seq -> 'a seq

val first : 'a seq -> 'a option
val nth : int -> 'a seq -> 'a option
val find_first : ('a -> bool) -> 'a seq -> 'a option

val to_list : 'a seq -> 'a list

(** see Lambdasoup's [Soup.with_stop] *)
val with_stop : ('a stop -> 'a) -> 'a

(** {3 Form} *)

(** Operations on forms and inputs *)
module Form : sig
  type t

  (** Phantom types for inputs *)

  type checkbox
  type radio_button
  type select_list
  type menu
  type field

  (** A form input *)
  type 'a input

  (** Return the name of the form *)
  val name : t -> string option

  (** Return the action attribute of the form *)
  val action : t -> Uri.t

  (** Return the absolute (resolved) uri corresponding to the action attribute *)
  val uri : t -> Uri.t

  (** Return the method attribute of the form or [`GET] if none *)
  val meth : t -> [`POST | `GET]

  (** Convert a form to a Soup node *)
  val to_node : t -> Soup.element Soup.node

  (** Convert an input to a Soup node *)
  val input_to_node : _ input -> Soup.element Soup.node

  (** Set directly the value(s) of a field *)

  val set : string -> string -> t -> t
  val set_multi : string -> string list -> t -> t

  (** Get the value(s) of a field *)

  val get : string -> t -> string option
  val get_multi : string -> t -> string list

  (** Remove the value of a field *)
  val clear : string -> t -> t

  (** Return all set values as a list *)
  val values : t -> (string * string list) list

  (** Return the name of an input *)
  val iname : (_ input) -> string option

  (** All the following function are built using the same pattern.

      - xxxs (eg {!checkboxes}) return all the inputs of a certain type as a lazy
      sequence.
      For example, [checkboxes myform] will return all the checkboxes of the form
      - xxx_with take a CSS selector as parameter, and return the first input that
      matches the selector, or [None] if there isn't any. Eg, [fields_with myform
      "\[name$=text2\]"] will try to find any text field which name ends with
      [text2]
      - xxxs_with proceed as the previous one, but return a lazy sequence of all inputs matching the
      selector.

  *)

  val checkbox_with : string -> t -> checkbox input option
  val checkboxes : t -> checkbox input seq
  val checkboxes_with : string -> t -> checkbox input seq

  val radio_button_with : string -> t -> radio_button input option
  val radio_buttons : t -> radio_button input seq
  val radio_buttons_with : string -> t -> radio_button input seq

  val select_list_with : string -> t -> select_list input option
  val select_lists : t -> select_list input seq
  val select_lists_with : string -> t -> select_list input seq

  (** Select textual fields, either text, password, search, or textarea *)
  val field_with : string -> t -> field input option
  val fields : t -> field input seq
  val fields_with : string -> t -> field input seq

  (** Select numeric fields, either number or range *)
  val numeric_with : string -> t -> field input option
  val numerics : t -> field input seq
  val numerics_with : string -> t -> field input seq

  val text_with : string -> t -> field input option
  val texts : t -> field input seq
  val texts_with : string -> t -> field input seq

  val password_with : string -> t -> field input option
  val passwords : t -> field input seq
  val passwords_with : string -> t -> field input seq

  val hidden_with : string -> t -> field input option
  val hiddens : t -> field input seq
  val hiddens_with : string -> t -> field input seq

  val textarea_with : string -> t -> field input option
  val textareas : t -> field input seq
  val textareas_with : string -> t -> field input seq

  val color_with : string -> t -> field input option
  val colors : t -> field input seq
  val colors_with : string -> t -> field input seq

  val date_with : string -> t -> field input option
  val dates : t -> field input seq
  val dates_with : string -> t -> field input seq

  val email_with : string -> t -> field input option
  val emails : t -> field input seq
  val emails_with : string -> t -> field input seq

  val month_with : string -> t -> field input option
  val months : t -> field input seq
  val months_with : string -> t -> field input seq

  val number_with : string -> t -> field input option
  val numbers : t -> field input seq
  val numbers_with : string -> t -> field input seq

  val tel_with : string -> t -> field input option
  val tels : t -> field input seq
  val tels_with : string -> t -> field input seq

  val search_with : string -> t -> field input option
  val searchs : t -> field input seq
  val searchs_with : string -> t -> field input seq

  val time_with : string -> t -> field input option
  val times : t -> field input seq
  val times_with : string -> t -> field input seq

  val url_with : string -> t -> field input option
  val urls : t -> field input seq
  val urls_with : string -> t -> field input seq

  (** Reset or clear all the fields *)

  val reset_all : t -> t
  val clear_all : t -> t

  (** Operation on Checkboxes *)
  module Checkbox : sig
    (** Return the value (the label) of a checkbox *)
    val value : checkbox input -> string

    (** Return the values of all the checkboxes with the same name as the
       given one *)
    val values : t -> checkbox input -> string list

    (** Return all the checkboxes with the same name as the given one *)
    val choices : t -> checkbox input -> checkbox input seq

    (** Return the list of all checked checkboxes with the same name as the
       given one *)
    val checked : t -> checkbox input -> string list

    (** [check form cb] return [form] where cb is checked *)
    val check : t -> checkbox input -> t

    (** [uncheck form cb] return [form] where cb is unchecked *)
    val uncheck : t -> checkbox input -> t

    (** Check if the specified checkbox is checked *)
    val is_checked : t -> checkbox input -> bool

    (** Values with the [checked] attribute set *)
    val checked_default : t -> checkbox input -> string list

    (** Reset to its default value, meaning that only the checkboxes with the
        [checked] attribute will be checked  *)
    val reset : t -> checkbox input -> t
  end

  (** Operations on Radio Buttons *)
  module RadioButton : sig
    (** Similar to checkboxes, except that selecting one radio button in group
       automatically unselect the others *)

    (** Return the value (the label) of a radio button*)
    val value : radio_button input -> string

    (** Return the values of all the radio buttons with the same name as the
       given one *)
    val values : t -> radio_button input -> string list

    (** Return all the radio buttons with the same name as the given one *)
    val choices : t -> radio_button input -> radio_button input seq

    (** Return the possibly selected radio button *)
    val selected : t -> checkbox input -> string option

    (** [select form rb] return [form] where [rb] is selected *)
    val select : t -> radio_button input -> t

    (** Check if the specified radio button is selected *)
    val is_selected : t -> radio_button input -> bool

    (** Values with [checked] attribute set *)
    val selected_default : t -> radio_button input -> string option

    (** Reset to its default value, meaning that only the radio buttons with the
        [checked] attribute will be selected *)
    val reset : t -> checkbox input -> t
  end

  (** Operations on Menu (select lists) *)
  module SelectList : sig
    (** Represent an item of the list *)
    type item

    (** Return a list of all items of a given list *)
    val items : select_list input -> item list

    (** Check if the select list supports multiple selection *)
    val is_multiple : select_list input -> bool

    (** Return a list of selected items as strings *)
    val selected : t -> select_list input -> string list

    (** Select a specific item. If multiple selection is not enabled,
        this unselect any previously selected item *)
    val select : t -> select_list input -> item -> t

    (** Unselect a specific item *)
    val unselect : t -> select_list input -> item -> t

    (** Check if the specified item is selected *)
    val is_selected : t -> select_list input -> item -> bool

    (** Label of an item *)
    val text : item -> string

    (** Value (as sent in form data) of an item *)
    val value : item -> string

    (** Items with the attribute [selected] set *)
    val selected_default : t -> select_list input -> string list

    (** Reset the select list to its default value,
        meaning that only the items with the
        [selected] attribute will be selected *)
    val reset : t -> checkbox input -> t
  end

  (** Operations on general fields : textarea, text, password, color, date, etc. *)
  module Field : sig
    (** Set the value of a field *)
    val set : t -> field input -> string -> t

    (** Return the content of the field, if any *)
    val get : t -> field input -> string option

    (** Return the default value of a field, specified via the attribute
       [value] *)
    val default_value : t -> field input -> string option

    (** Set the value back to the default value, or just clear the field if
        there is no default value *)
    val reset : t -> checkbox input -> t
  end
end

(** {4 Images and links} *)

(** Operations on hypertext links *)
module Link : sig
  type t

  val href : t -> string
  val text : t -> string option
  val uri : t -> Uri.t

  val to_node : t -> Soup.element Soup.node
end

(** Operations on images *)
module Image : sig
  type t

  val source : t -> string
  val uri : t -> Uri.t

  val to_node : t -> Soup.element Soup.node
end

(** {5 Nodes selection} *)

(** All the following function are built using the same pattern.

    - xxxs (eg {!forms}) return all the elements of a certain type as a lazy
    sequence.
    For example, [forms mypage] will return all the forms in the page
    - xxx_with take a CSS selector as parameter, and return the first element that
    matches the selector, or [None] if there isn't any. Eg, [link_with
    "\[href$=.jpg\]" mypage] will try to find a link that point to a JPEG image
    - xxxs_with proceed as the previous one, but return a lazy sequence of all elements matching the
    selector.
*)

val form_with : string -> t -> Form.t option
val forms_with : string -> t -> Form.t seq
val forms : t -> Form.t seq

val link_with : string -> t -> Link.t option
val links_with : string -> t -> Link.t seq
val links : t -> Link.t seq

val image_with : string -> t -> Image.t option
val images_with : string -> t -> Image.t seq
val images : t -> Image.t seq
