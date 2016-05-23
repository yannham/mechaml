type e = Soup.element Soup.node
type t = {form : e; data : (string,string list) Hashtbl.t}

type _ input = element node
type _ inputs = element nodes

let to_node n = n
let to_nodes n = n

let to_list = Soup.to_list
let iter = iter
let fold = fold
let filter = filter

let raw_set f key value = Hashtbl.add f.data key value
let raw_unset f key = Hashtbl.remove f.data key
let raw_values f = Hashtbl.fold (fun x y l -> (x,y)::l) f.data []

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

let submit f agent = ()

let reset f = Hashtbl.clear f.data

let (>>>) (x,y) f = match x,y with Some x, Some y -> Some (f x y) | _ -> None
let (>|=) x f = match x with Some x -> Some (f x) | _ -> None
let (>>) x f = match x,f with Some x, Some f -> Some (f x) | _ -> None
let (|?) x default = match x with Some x -> x | None -> default
let singleton x = [x]
let ladd x l = x::l

module Checkbox = struct
  let check f cb = 
    (Soup.id cb, Soup.attribute "value" cb >|= singleton) >>> Hashtbl.add f.data
    |> ignore

  let uncheck f cb =
    Soup.id cb >|= Hashtbl.remove f.data |> ignore

  let is_checked f cb =
    Soup.id cb >|= Hashtbl.mem f.data |? false
end

module RadioButton = struct
  type value = string

  let rb_selector id = Printf.sprintf "[type=radio]#%s" id

  let values f rb =
    Soup.id rb >|= rb_selector >|= (fun s ->
      Soup.select s f.form |> to_list) |? []
 
  let select f rb =
    (Soup.id rb, Soup.attribute "value" rb >|= singleton) >>> Hashtbl.add f.data |> ignore

  let is_selected f rb =
    Soup.attribute "value" rb >|= Hashtbl.mem f.data |? false
end

module SelectList = struct
  type item = element node 

  let items sl = Soup.select "[type=option]" sl |> to_list

  let select f sl item =
    (Soup.id sl, Soup.attribute "value" item >|= singleton) >>> Hashtbl.replace f.data
    |> ignore

  let unselect f sl item = 
    Soup.id sl >|= Hashtbl.remove f.data |> ignore

  let is_selected f sl item = 
    Soup.attribute "value" sl >|= Hashtbl.mem f.data |? false

  let to_string item = item >|= Soup.attribute "value" |> require
end

module Menu = struct
  type item = element node 

  let current_selection f id = 
    try
      Some (Hashtbl.find f.data id)
    with Not_found -> None

  let items menu = Soup.select "[type=option]" menu |> to_list

  let is_selected f menu item = 
    (Soup.id menu, Soup.attribute "value" item) >>>
      (fun id value ->
        current_selection f id >|= List.mem value |? false) |? false

  let select f menu item =
    if not (is_selected f menu item) then
      (Soup.id menu, Soup.attribute "value" item) >>>
        (fun id value ->
          current_selection f id |? [] |> ladd value
          |> Hashtbl.replace f.data id)
      |> ignore

  let unselect f menu item = 
    (Soup.id menu, Soup.attribute "value" item) >>>
      (fun id value ->
        current_selection f id >|= List.filter ((!=) value) >|=
          Hashtbl.replace f.data id)

  let to_string item = item >|= Soup.attribute "value" |> require
end

module Field = struct
  let set f fd value =
    Soup.id fd >|= (fun id -> value |> singleton |> Hashtbl.replace f.data id)

  let get f fd = 
    try
      Soup.id fd >|= Hashtbl.find f.data >|= List.hd
    with _ -> None
end

module FileUpload = struct
  let select f fu path =
    Soup.id fu >|= (fun id -> Hashtbl.replace f.data id [path])

  let which_selected f fu =
    try
      Soup.id fu >|= Hashtbl.find f.data >|= List.hd
    with _ -> None
end
