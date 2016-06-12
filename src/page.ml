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

let tag_selector tag = function
  | "" -> tag
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

  let to_node n = n
  let to_nodes n = n

  let to_list = Soup.to_list
  let iter = Soup.iter
  let fold = Soup.fold
  let filter = Soup.filter

  let raw_set key value f = 
    {f with data = f.data |> StringMap.add key value}
  let raw_get key f = StringMap.find key f.data
  let raw_unset key f = 
    {f with data = f.data |> StringMap.remove key}
  let raw_values f = StringMap.fold (fun id value l -> (id,value)::l) f.data []

  let checkboxes_with selector f =
    f.form |> Soup.select (tag_selector "input[type=checkbox]" selector)
    |> Soup.filter (input_filter "checkbox")

  let checkbox_with selector f =
    f |> checkboxes_with selector |> Soup.first

  let checkboxes = checkboxes_with ""

  let radio_buttons_with selector f =
    f.form |> Soup.select (tag_selector "input[type=radio_button]" selector)
    |> Soup.filter (input_filter "radio_button")

  let radio_button_with selector f =
    f |> radio_buttons_with selector |> Soup.first

  let radio_buttons = radio_buttons_with ""

  let select_lists_with selector f =
    f.form |> Soup.select (tag_selector "input[type=select_list]" selector)
    |> Soup.filter (input_filter "select_list")

  let select_list_with selector f =
    f |> select_lists_with selector |> Soup.first

  let select_lists = select_lists_with ""

  let fields_with selector f =
    f.form |> Soup.select (tag_selector "input[type=fields_list]" selector)
    |> Soup.filter (input_filter "field")

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
    |> Soup.filter (input_filter "textarea")

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

  let name input = Soup.attribute "name" input
  let value input = Soup.attribute "value" input

  open Infix.Option

  let singleton x = [x]
  let ladd x l = x::l
  let radd m k v = StringMap.add k v m
  let rrem m k = StringMap.remove k m
  let rmem m k = StringMap.mem k m
  let rfind m k = StringMap.find k m

  let update_form f data = {f with data = data |? f.data}

  let has_value m k v =
    try
      List.mem v (StringMap.find k m)
    with Not_found -> false

  module Checkbox = struct
    let check f cb =
      (name cb, value cb >|= singleton) >>> radd f.data
      |> update_form f

    let uncheck f cb =
      name cb >|= rrem f.data |> update_form f

    let is_checked f cb =
      name cb >|= rmem f.data |? false
  end

  module RadioButton = struct
    let rb_selector id = Printf.sprintf "[type=radio]#%s" id

    let _value = value

    let value rb = _value rb |> Soup.require

    let values f rb =
      name rb >|= rb_selector >|=
      (fun s -> Soup.select s f.form
        |> to_list) |? []

    let select f rb =
      (name rb, _value rb >|= singleton) >>> radd f.data
      |> update_form f

    let is_selected f rb =
      _value rb >|=
      (fun v -> values f rb
        |> List.exists (fun x -> (_value x |? "") = v))
      |? false

    let to_string item = item >|= _value |> Soup.require
  end

  module SelectList = struct
    type item = elt

    let items sl = Soup.select "[type=option]" sl |> to_list

    let select f sl item =
      (Soup.id sl, value item >|= singleton) >>> radd f.data
      |> update_form f

    let unselect f sl item =
      Soup.id sl >|= rrem f.data |> update_form f

    let is_selected f sl item =
      (Soup.id sl, value sl) >>>
      has_value f.data |? false

    let to_string item = item |> value |> Soup.require
  end

  module Menu = struct
    type item = elt

    let current_selection f id =
      try
        StringMap.find id f.data
      with Not_found -> []

    let items menu = Soup.select "[type=option]" menu |> to_list

    let is_selected f menu item =
      (name menu, value item) >>>
      (fun id value -> current_selection f id |> List.mem value)
      |? false

    let select f menu item =
      match is_selected f menu item with
      | false ->
        (name menu, value item) >>>
        (fun name value ->
          current_selection f name |> ladd value |> radd f.data name)
        |> update_form f
      | true -> f

    let unselect f menu item =
      (name menu, value item) >>>
      (fun id value ->
        current_selection f id |> List.filter ((!=) value)
        |> radd f.data id)
      |> update_form f

    let to_string item = item |> value |> Soup.require
  end

  module Field = struct
    let set f fd v =
      Soup.id fd >|= (fun id -> v |> singleton |> radd f.data id)
      |> update_form f

    let get f fd =
      try
        Soup.id fd >|= rfind f.data >|= List.hd
      with _ -> None
  end

  module FileUpload = struct
    let select f fu path =
      name fu >|= (fun id -> radd f.data id [path]) |> update_form f

    let which_selected f fu =
      try
        name fu >|= rfind f.data >|= List.hd
      with _ -> None
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

module Frame = struct
  type t = elt

  let source frame = frame |> Soup.attribute "src" |> Soup.require
  let uri frame = frame |> source |> Uri.of_string
  let name frame = frame |> Soup.attribute "name"
  let text = Soup.leaf_text

  let make ?name ?text ~source = failwith "Not implemented"

  let to_node frame = frame
end

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

let frames_with selector p =
  p |> Soup.select (tag_selector "frame" selector)
  |> Soup.filter (tag_filter "frame") |> Soup.to_list

let frames = frames_with ""

let frame_with selector p =
  p |> frames_with selector |> hd_opt

let to_soup p = p
let from_soup p = p
