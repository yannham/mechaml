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

module StringMap = Map.Make(String)

type elt = Soup.element Soup.node
type t = {form : elt; data : (string list) StringMap.t}

type _ input = elt
type _ inputs = Soup.element Soup.nodes

let to_node n = n
let to_nodes n = n

let to_list = Soup.to_list
let iter = Soup.iter
let fold = Soup.fold
let filter = Soup.filter

let raw_set f key value = StringMap.add key value f.data
let raw_get f key = StringMap.find key f.data
let raw_unset f key = StringMap.remove key f.data
let raw_values f = StringMap.fold (fun id value l -> (id,value)::l) f.data []

let checkbox_with f selector = Soup.select_one ("input[type=checkbox]"^selector)
f.form
let checkboxes_with f selector = Soup.select ("input[type=checkbox]"^selector)
f.form
let checkboxes f = checkboxes_with f ""

let radio_button_with f selector = Soup.select_one
("input[type=radio_button]"^selector) f.form
let radio_buttons_with f selector = Soup.select
("input[type=radio_button]"^selector) f.form
let radio_buttons f = radio_buttons_with f ""

let select_list_with f selector = Soup.select_one ("input[type=select_list]"^selector) f.form
let select_lists_with f selector = Soup.select ("input[type=select_list]"^selector) f.form
let select_lists f = select_lists_with f ""

let field_with f selector = Soup.select_one ("input[type=fields_list]"^selector) f.form
let fields_with f selector = Soup.select ("input[type=fields_list]"^selector) f.form
let fields f = fields_with f ""

let text_with f selector = Soup.select_one ("input[type=text]"^selector) f.form
let texts_with f selector = Soup.select ("input[type=text]"^selector) f.form
let texts f = texts_with f ""

let password_with f selector = Soup.select_one ("input[type=password]"^selector) f.form
let passwords_with f selector = Soup.select ("input[type=password]"^selector) f.form
let passwords f = passwords_with f ""

let hidden_with f selector = Soup.select_one ("input[type=hidden]"^selector) f.form
let hiddens_with f selector = Soup.select ("input[type=hidden]"^selector) f.form
let hiddens f = hiddens_with f ""

let int_with f selector = Soup.select_one ("input[type=int]"^selector) f.form
let ints_with f selector = Soup.select ("input[type=int]"^selector) f.form
let ints f = ints_with f ""

let keygen_with f selector = Soup.select_one ("input[type=keygen]"^selector) f.form
let keygens_with f selector = Soup.select ("input[type=keygen]"^selector) f.form
let keygens f = keygens_with f ""

let file_upload_with f selector = Soup.select_one ("input[type=file_upload]"^selector) f.form
let file_uploads_with f selector = Soup.select ("input[type=file_upload]"^selector) f.form
let file_uploads f = file_uploads_with f ""

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
  type value = elt

  let rb_selector id = Printf.sprintf "[type=radio]#%s" id

  let values f rb =
    name rb >|= rb_selector >|=
    (fun s -> Soup.select s f.form
      |> to_list) |? []

  let select f rb =
    (name rb, value rb >|= singleton) >>> radd f.data
    |> update_form f

  let is_selected f rb =
    value rb >|=
    (fun v -> values f rb
      |> List.exists (fun x -> (value x |? "") = v))
    |? false

  let to_string item = item >|= value |> Soup.require
end

module SelectList = struct
  type item = elt

  let items sl = Soup.select "[type=option]" sl |> to_list

  let select f sl item =
    (Soup.id sl, Soup.attribute "value" item >|= singleton) >>> radd f.data
    |> update_form f

  let unselect f sl item =
    Soup.id sl >|= rrem f.data |> update_form f

  let is_selected f sl item =
    (Soup.id sl, Soup.attribute "value" sl) >>>
    has_value f.data |? false

  let to_string item = item >|= value |> Soup.require
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

  let to_string item = item >|= value |> Soup.require
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
