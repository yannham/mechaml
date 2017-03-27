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

type t = Soup.soup Soup.node
type elt = Soup.element Soup.node

open Infix.Option

let is_identifier_char c =
  let code = c |> Char.lowercase_ascii |> Char.code in
  (code >= (Char.code 'a') && code <= (Char.code 'z')) ||
  (code >= (Char.code '0') && code <= (Char.code '9')) ||
  (c == '-') || (c == '_')

let tag_filter tag node =
  Soup.name node = tag

let input_filter input_type node =
  Soup.name node = "input"
  && (Soup.attribute "type" node >|= fun t ->
    t=input_type) |? false

let field_filter node =
    match Soup.name node with
      | "textarea" -> true
      | "input" ->
        Soup.attribute "type" node >|= (fun t ->
          t="text" || t="password") |? false
      | _ -> false

let tag_selector tag = function
  | "" -> tag
  | s when s.[0]='*' -> s
  | s when is_identifier_char s.[0] -> s
  | s -> tag^s

let hd_opt = function
  | [] -> None
  | x::xs -> Some x

module Form = struct
  module StringMap = Map.Make(String)

  type t = {form : elt; data : (string list) StringMap.t}

  type checkbox
  type radio_button
  type select_list
  type menu
  type field
  type file_upload
  type _ input = elt
  type _ inputs = Soup.element Soup.nodes

  let name f = Soup.attribute "name" f.form
  let action f =
    f.form |> Soup.attribute "action"
    >|= Uri.of_string
    |> Soup.require
  let meth f =
    let m = f.form |> Soup.attribute "method"
      >|= String.lowercase_ascii
      >|= String.trim in
    match m with
      | Some "post" -> `POST
      | _ -> `GET

  let to_node f = f.form
  let input_to_node i = i
  let input_to_nodes is = is

  let to_list = Soup.to_list
  let iter = Soup.iter
  let fold = Soup.fold
  let filter = Soup.filter

  let set key value f =
    {f with data = f.data |> StringMap.add key value}
  let get key f = StringMap.find key f.data
  let unset key f =
    {f with data = f.data |> StringMap.remove key}
  let values f = StringMap.fold (fun id value l -> (id,value)::l) f.data []

  let checkboxes_with selector f =
    f.form |> Soup.select (tag_selector "input[type=checkbox]" selector)
    |> Soup.filter (input_filter "checkbox")

  let checkbox_with selector f =
    f |> checkboxes_with selector |> Soup.first

  let checkboxes = checkboxes_with ""

  let radio_buttons_with selector f =
    f.form |> Soup.select (tag_selector "input[type=radio]" selector)
    |> Soup.filter (input_filter "radio")

  let radio_button_with selector f =
    f |> radio_buttons_with selector |> Soup.first

  let radio_buttons = radio_buttons_with ""

  let select_lists_with selector f =
    f.form |> Soup.select (tag_selector "select" selector)
    |> Soup.filter (tag_filter "select")

  let select_list_with selector f =
    f |> select_lists_with selector |> Soup.first

  let select_lists = select_lists_with ""

  let fields_with selector f =
    f.form |> Soup.select (tag_selector "*" selector)
    |> Soup.filter field_filter

  let field_with selector f =
    f |> fields_with selector |> Soup.first

  let fields = fields_with ""

  let texts_with selector f =
    f.form |> Soup.select (tag_selector "input[type=text]" selector)
    |> Soup.filter (input_filter "text")

  let text_with selector f =
    f |> texts_with selector |> Soup.first

  let texts = texts_with ""

  let passwords_with selector f =
    f.form |> Soup.select (tag_selector "input[type=password]" selector)
    |> Soup.filter (input_filter "password")

  let password_with selector f =
    f |> passwords_with selector |> Soup.first

  let passwords = passwords_with ""

  let hiddens_with selector f =
    f.form |> Soup.select (tag_selector "input[type=hidden]" selector)
    |> Soup.filter (input_filter "hidden")

  let hidden_with selector f =
    f |> hiddens_with selector |> Soup.first

  let hiddens = hiddens_with ""

  let ints_with selector f =
    f.form |> Soup.select (tag_selector "input[type=int]" selector)
    |> Soup.filter (input_filter "int")

  let int_with selector f =
    f |> ints_with selector |> Soup.first

  let ints = ints_with ""

  let textareas_with selector f =
    f.form |> Soup.select (tag_selector "textarea" selector)
    |> Soup.filter (fun node -> Soup.name node = "textarea")

  let textarea_with selector f =
    f |> textareas_with selector |> Soup.first

  let textareas = textareas_with ""

  let keygens_with selector f =
    f.form |> Soup.select (tag_selector "input[type=keygen]" selector)
    |> Soup.filter (input_filter "keygen")

  let keygen_with selector f =
    f |> keygens_with selector |> Soup.first

  let keygens = keygens_with ""

  let file_uploads_with selector f =
    f.form |> Soup.select (tag_selector "input[type=file_upload]" selector)
    |> Soup.filter (input_filter "file_upload")

  let file_upload_with selector f =
    f |> file_uploads_with selector |> Soup.first

  let file_uploads = file_uploads_with ""

  let reset f = {f with data = StringMap.empty}

  let iname input = Soup.attribute "name" input
  let ivalue input = Soup.attribute "value" input

  open Infix.Option

  let singleton x = [x]
  let cons x l = x::l
  let uncurry f (x,y) = f x y
  let radd m k v = StringMap.add k v m
  let rrem m k = StringMap.remove k m
  let rmem m k = StringMap.mem k m
  let rfind m k = StringMap.find k m

  let update_form f newdata = {f with data = newdata |? f.data}

  let has_value m k v =
    try
      rfind m k
      |> List.mem v
    with Not_found -> false

  let current_values m k =
    try
      rfind m k
    with Not_found -> []

  let add_value m k v =
    v::(current_values m k)
    |> radd m k

  let rem_value m k v =
    current_values m k
    |> List.filter ((<>) v)
    |> radd m k

  let current_value m k =
    match rfind m k with
      | exception Not_found -> None
      | [x] -> Some x
      | _ -> None

  module Checkbox = struct
    let cb_selector name = Printf.sprintf "[type=checkbox][name=%s]" name

    let value cb = ivalue cb |> Soup.require

    let choices f cb =
      iname cb >|= cb_selector >|= (fun s ->
        Soup.select s f.form) |> Soup.require

    let values f cb =
      choices f cb |> fold (fun l cb ->
        match ivalue cb with
          | Some v -> v::l
          | None -> l) []

    let checked f cb =
      iname cb >|= current_values f.data |? []

    let check f cb =
      (iname cb, ivalue cb) >>> add_value f.data
      |> update_form f

    let uncheck f cb =
      (iname cb, ivalue cb) >>> rem_value f.data
      |> update_form f

    let is_checked f cb =
      (iname cb, ivalue cb) >>> has_value f.data |? false
  end

  module RadioButton = struct
    let rb_selector name = Printf.sprintf "[type=radio][name=%s]" name

    let value rb = ivalue rb |> Soup.require

    let choices f rb =
      iname rb
      >|= rb_selector
      >|= (fun s ->
        Soup.select s f.form) |> Soup.require

    let values f rb =
      choices f rb |> fold (fun l cb ->
        match ivalue cb with
          | Some v -> v::l
          | None -> l) []

    let selected f rb =
      iname rb >>= current_value f.data

    let select f rb =
      (iname rb, ivalue rb >|= singleton)
      >>> radd f.data
      |> update_form f

    let is_selected f rb =
      (iname rb, ivalue rb) >>> has_value f.data |? false

    let to_string item = item >|= ivalue |> Soup.require
  end

  module SelectList = struct
    type item = elt

    let items sl = Soup.select "option" sl |> to_list

    let selected f sl =
      iname sl >>= current_value f.data

    let select f sl item =
      (iname sl, ivalue item >|= singleton)
      >>> radd f.data
      |> update_form f

    let unselect f sl item =
      iname sl >|= rrem f.data |> update_form f

    let is_selected f sl item =
      (iname sl, ivalue item) >>> has_value f.data |? false

    let to_string item = item |> ivalue |> Soup.require
  end

  module Field = struct
    let set f fd v =
      iname fd
      >|= (fun name ->
        radd f.data name [v])
      |> update_form f

    let get f fd =
      iname fd >>= current_value f.data
  end

  module FileUpload = struct
    let select f fu path =
      iname fu
      >|= (fun name -> radd f.data name [path])
      |> update_form f

    let which_selected f fu =
      iname fu >>= current_value f.data
  end
end

module Link = struct
  type t = elt

  let href link = link |> Soup.attribute "href" |> Soup.require
  let text = Soup.leaf_text
  let uri link = link |> href |> Uri.of_string

  let make ?text:(text="") ~href =
    let attributes = [("href",href)] in
    Soup.create_element ~attributes ~inner_text:text "a"

  let to_node link = link
end

module Image = struct
  type t = elt

  let source image = image |> Soup.attribute "src" |> Soup.require
  let uri image = image |> source |> Uri.of_string

  let make ~source =
    let attributes = [("src",source)] in
    Soup.create_element ~attributes "img"

  let to_node image = image
end

(* module Frame = struct *)
(*   type t = elt *)
(*  *)
(*   let source frame = frame |> Soup.attribute "src" |> Soup.require *)
(*   let uri frame = frame |> source |> Uri.of_string *)
(*   let name frame = frame |> Soup.attribute "name" *)
(*   let text = Soup.leaf_text *)
(*  *)
(*   let make ?name ?text ~source = failwith "Not implemented" *)
(*  *)
(*   let to_node frame = frame *)
(* end *)

let forms_with selector p =
  p |> Soup.select (tag_selector "form" selector)
  |> Soup.filter (tag_filter "form")
  |> Soup.to_list
  |> List.map (fun node -> {Form.form = node; data = Form.StringMap.empty})

let forms = forms_with ""

let form_with selector p =
  p |> forms_with selector |> hd_opt

let links_with selector p =
  p |> Soup.select (tag_selector "a" selector)
  |> Soup.filter (tag_filter "a") |> Soup.to_list

let links = links_with ""

let link_with selector p =
  p |> links_with selector |> hd_opt

let images_with selector p =
  p |> Soup.select (tag_selector "img" selector)
  |> Soup.filter (tag_filter "img") |> Soup.to_list

let images = images_with ""

let image_with selector p =
  p |> images_with selector |> hd_opt

(* let frames_with selector p = *)
(*   p |> Soup.select (tag_selector "frame" selector) *)
(*   |> Soup.filter (tag_filter "frame") |> Soup.to_list *)
(*  *)
(* let frames = frames_with "" *)
(*  *)
(* let frame_with selector p = *)
(*   p |> frames_with selector |> hd_opt *)

let to_soup p = p
let from_soup p = p
