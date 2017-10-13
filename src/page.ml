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

type t = { base_uri : Uri.t; soup : Soup.soup Soup.node }

type elt = Soup.element Soup.node

open Infix.Option

let id x = x

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

let is_input_field = function
  | "text"
  | "password"
  | "hidden" -> true
  | _ -> false

let field_filter node =
    match Soup.name node with
      | "textarea" -> true
      | "input" ->
        Soup.attribute "type" node >|= is_input_field |? false
      | _ -> false

let tag_selector tag = function
  | "" -> tag
  | s when s.[0]='*' -> s
  | s when is_identifier_char s.[0] -> s
  | s -> tag^s

let from_soup ?(location=Uri.empty) soup =
  let base_uri =
    Soup.select_one "base[href]" soup
    >>= Soup.attribute "href"
    >|= Uri.of_string |? location in
  {base_uri; soup}

let from_string ?(location=Uri.empty) s =
  s
  |> Soup.parse
  |> from_soup ~location

let base_uri p = p.base_uri

let resolver p = Uri.resolve "" p.base_uri

let soup p = p.soup

type +'a seq = { eliminate : 'b. ('b -> 'a -> 'b) -> 'b -> 'b }
type 'a stop = 'a Soup.stop = { throw : 'b. 'a -> 'b }

let with_stop = Soup.with_stop

let seq_from_nodes gen nodes =
  { eliminate = fun f init ->
      let f' acc x = f acc (gen x) in
      Soup.fold f' init nodes }

let iter it seq =
  seq.eliminate (fun _ -> it) ()

let fold f init seq = seq.eliminate f init

let filter pred seq =
  { eliminate = fun f init ->
      let f' acc x = if pred x then f acc x else acc in
      seq.eliminate f' init }

let nth n seq =
  with_stop (fun stop ->
    seq.eliminate (fun i x ->
      if i = n then Some x |> stop.throw
      else i+1) 1
    |> ignore;
    None)

let first seq = nth 1 seq

let find_first pred seq =
  seq
  |> filter pred
  |> first

let to_list seq =
  seq.eliminate (fun l x -> x::l) []
  |> List.rev

module Form = struct
  module StringMap = Map.Make(String)

  type t = {resolver : Uri.t -> Uri.t; elt : elt; data : (string list) StringMap.t}

  type checkbox
  type radio_button
  type select_list
  type menu
  type field
  (* type file_upload *)
  type _ input = elt
  type _ inputs = Soup.element Soup.nodes

  let name f = Soup.attribute "name" f.elt

  let action f =
    f.elt
    |> Soup.attribute "action"
    >|= Uri.of_string
    |> Soup.require

  let uri f =
    f
    |> action
    |> f.resolver

  let meth f =
    let m = f.elt
      |> Soup.attribute "method"
      >|= String.lowercase_ascii
      >|= String.trim in
    match m with
      | Some "post" -> `POST
      | _ -> `GET

  let to_node f = f.elt
  let input_to_node i = i

  let set_multi key values f =
    {f with data = f.data |> StringMap.add key values}
  let set key value f = set_multi key [value] f

  let get_multi key f = StringMap.find key f.data
  let get key f =
    get_multi key f
    |> (function
      | [] -> None
      | x::xs -> Some x)

  let clear key f =
    {f with data = f.data |> StringMap.remove key}
  let reset = clear

  let clear_all f =
    {f with data = StringMap.empty }

  let values f = StringMap.fold (fun id value l -> (id,value)::l) f.data []

  let checkboxes_with selector f =
    f.elt
    |> Soup.select (tag_selector "input[type=checkbox]" selector)
    |> Soup.filter (input_filter "checkbox")
    |> seq_from_nodes id

  let checkbox_with selector f =
    f |> checkboxes_with selector |> first

  let checkboxes = checkboxes_with ""

  let radio_buttons_with selector f =
    f.elt
    |> Soup.select (tag_selector "input[type=radio]" selector)
    |> Soup.filter (input_filter "radio")
    |> seq_from_nodes id

  let radio_button_with selector f =
    f
    |> radio_buttons_with selector
    |> first

  let radio_buttons = radio_buttons_with ""

  let select_lists_with selector f =
    f.elt
    |> Soup.select (tag_selector "select" selector)
    |> Soup.filter (tag_filter "select")
    |> seq_from_nodes id

  let select_list_with selector f =
    f
    |> select_lists_with selector
    |> first

  let select_lists = select_lists_with ""

  let fields_with selector f =
    f.elt
    |> Soup.select (tag_selector "*" selector)
    |> Soup.filter field_filter
    |> seq_from_nodes id

  let field_with selector f =
    f
    |> fields_with selector
    |> first

  let fields = fields_with ""

  let texts_with selector f =
    f.elt
    |> Soup.select (tag_selector "input[type=text]" selector)
    |> Soup.filter (input_filter "text")
    |> seq_from_nodes id

  let text_with selector f =
    f
    |> texts_with selector
    |> first

  let texts = texts_with ""

  let passwords_with selector f =
    f.elt
    |> Soup.select (tag_selector "input[type=password]" selector)
    |> Soup.filter (input_filter "password")
    |> seq_from_nodes id

  let password_with selector f =
    f
    |> passwords_with selector
    |> first

  let passwords = passwords_with ""

  let hiddens_with selector f =
    f.elt
    |> Soup.select (tag_selector "input[type=hidden]" selector)
    |> Soup.filter (input_filter "hidden")
    |> seq_from_nodes id

  let hidden_with selector f =
    f
    |> hiddens_with selector
    |> first

  let hiddens = hiddens_with ""

  let textareas_with selector f =
    f.elt
    |> Soup.select (tag_selector "textarea" selector)
    |> Soup.filter (fun node -> Soup.name node = "textarea")
    |> seq_from_nodes id

  let textarea_with selector f =
    f |> textareas_with selector |> first

  let textareas = textareas_with ""

  (* let file_uploads_with selector f = *)
  (*   f.elt *)
  (*   |> Soup.select (tag_selector "input[type=file_upload]" selector) *)
  (*   |> Soup.filter (input_filter "file_upload") *)
  (*   |> seq_from_nodes id *)
  (*  *)
  (* let file_upload_with selector f = *)
  (*   f *)
  (*   |> file_uploads_with selector *)
  (*   |> first *)
  (*  *)
  (* let file_uploads = file_uploads_with "" *)

  let iname input = Soup.attribute "name" input
  let ivalue input = Soup.attribute "value" input

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

  let reset_single f input get_default =
    match iname input, get_default f input with
      | None, _ -> f
      | Some name, None -> clear name f
      | Some name, Some value -> set name value f

  let reset_multi f input get_default =
    match iname input, get_default f input with
      | None, _ -> f
      | Some name, [] -> clear name f
      | Some name, values -> set_multi name values f

  open Infix.Option

  module Checkbox = struct
    let cb_selector name = Printf.sprintf "[type=checkbox][name=%s]" name

    let value cb = ivalue cb |> Soup.require

    let choices f cb =
      iname cb
      >|= cb_selector
      >|= (fun s ->
        Soup.select s f.elt)
      |> Soup.require
      |> seq_from_nodes id

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

    let checked_default f cb =
      choices f cb
      |> fold (fun values cb ->
        if Soup.has_attribute "checked" cb then
          (ivalue cb |? "on")::values
        else
          values) []
      |> List.rev

    let reset f cb = reset_multi f cb checked_default
  end

  module RadioButton = struct
    let rb_selector name = Printf.sprintf "[type=radio][name=%s]" name

    let value rb = ivalue rb |> Soup.require

    let choices f rb =
      iname rb
      >|= rb_selector
      >|= (fun s ->
        Soup.select s f.elt)
      |> Soup.require
      |> seq_from_nodes id

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

    let selected_default f rb =
      with_stop (fun stop ->
        choices f rb
        |> iter (fun rb' ->
          if Soup.has_attribute "checked" rb' then
            Some (ivalue rb' |? "on")
            |> stop.throw);
        None)

    let reset f rb = reset_single f rb selected_default
  end

  module SelectList = struct
    type item = elt

    let items sl = Soup.select "option" sl |> Soup.to_list

    let is_multiple = Soup.has_attribute "multiple"

    let selected f sl =
      iname sl
      >|= current_values f.data |? []

    let unselect f sl item =
      iname sl
      >|= rrem f.data
      |> update_form f

    let is_selected f sl item =
      (iname sl, ivalue item) >>> has_value f.data |? false

    let text item =
      item |> Soup.leaf_text |? ""

    let value item =
      item
      |> ivalue |? text item

    let select f sl item =
      iname sl
      >|= (function name ->
        let v = value item in
        if is_multiple sl then
          let vs = current_values f.data name in
          (if has_value f.data name v then vs else v::vs)
          |> radd f.data name
        else
          radd f.data name [v])
      |> update_form f

    let selected_default f sl =
      items sl
      |> List.fold_left (fun values item ->
        if Soup.has_attribute "selected" item then
          (value item)::values
        else
          values) []
      |> List.rev

    let reset f sl = reset_multi f sl selected_default
  end

  module Field = struct
    let fset = set

    let set f fd v =
      iname fd
      >|= (fun name ->
        radd f.data name [v])
      |> update_form f

    let get f fd =
      iname fd >>= current_value f.data

    let default_value _ fd =
      match Soup.name fd with
        | "textarea" -> Soup.leaf_text fd
        | _ -> ivalue fd

    let reset f fd = reset_single f fd default_value
  end

  (* module FileUpload = struct *)
  (*   let select f fu path = *)
  (*     iname fu *)
  (*     >|= (fun name -> radd f.data name [path]) *)
  (*     |> update_form f *)
  (*  *)
  (*   let which_selected f fu = *)
  (*     iname fu >>= current_value f.data *)
  (* end *)

  let reset_all f =
    checkboxes f
    |> fold Checkbox.reset f
    |> fun newf ->
    radio_buttons newf
    |> fold RadioButton.reset newf
    |> fun newf ->
    select_lists newf
    |> fold SelectList.reset newf
    |> fun newf ->
    fields newf
    |> fold Field.reset newf
end

module Link = struct
  type t = { resolver : Uri.t -> Uri.t ; elt : elt }

  let href link = link.elt |> Soup.attribute "href" |> Soup.require
  let text link = Soup.leaf_text link.elt
  let uri link = link |> href
    |> Uri.of_string
    |> link.resolver

  let to_node link = link.elt
end

module Image = struct
  type t = {resolver : Uri.t -> Uri.t ; elt : elt}

  let source image = image.elt |> Soup.attribute "src" |> Soup.require
  let uri image = image |> source
    |> Uri.of_string
    |> image.resolver

  let to_node image = image.elt
end

let forms_with selector p =
  p.soup
  |> Soup.select (tag_selector "form" selector)
  |> Soup.filter (tag_filter "form")
  |> seq_from_nodes (fun elt -> Form.( {resolver = resolver p; elt;
    data = StringMap.empty} |> reset_all ))

let forms = forms_with ""

let form_with selector p =
  p
  |> forms_with selector
  |> first

let links_with selector p =
  p.soup
  |> Soup.select (tag_selector "a" selector)
  |> Soup.filter (tag_filter "a")
  |> seq_from_nodes (fun elt -> Link.( {resolver = resolver p; elt} ))

let links = links_with ""

let link_with selector p =
  p
  |> links_with selector
  |> first

let images_with selector p =
  p.soup
  |> Soup.select (tag_selector "img" selector)
  |> Soup.filter (tag_filter "img")
  |> seq_from_nodes (fun elt -> Image.( {resolver = resolver p; elt} ))

let images = images_with ""

let image_with selector p =
  p
  |> images_with selector
  |> first
