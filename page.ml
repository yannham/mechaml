type t = Soup.soup Soup.node
type elt = Soup.element Soup.node

open Infix.Option

module Link = struct
  type t = elt

  let href link = link |> Soup.attribute "href" |> Soup.require
  let text = Soup.leaf_text
  let uri link = link |> href |> Uri.of_string

  let make ?text:(t="") ~href =
    failwith "Not Implemented"
end

module Image = struct
  type t = elt

  let source img = img |> Soup.attribute "src" |> Soup.require
  let uri img = img |> source |> Uri.of_string

  let make ?mime_type ?caption ~source = failwith "Not Implemented"
end

module Frame = struct
  type t = elt

  let source frame = frame |> Soup.attribute "src" |> Soup.require
  let uri frame = frame |> source |> Uri.of_string
  let name frame = frame |> Soup.attribute "name"
  let text = Soup.leaf_text

  let make ?name ?text ?source = failwith "Not Implemented"
end

let form_with selector p = p |> Soup.select_one ("form"^selector)
let forms_with selector p = p |> Soup.select ("form"^selector)
let forms p = p |> forms_with ""

let link_with selector p = p |> Soup.select_one ("a"^selector)
let links_with selector p = p |> Soup.select ("a"^selector)
let links p = p |> links_with ""

let image_with selector p = p |> Soup.select_one ("img"^selector)
let images_with selector p = p |> Soup.select ("img"^selector)
let images p = p |> images_with ""

let frame_with selector p = p |> Soup.select_one ("frame"^selector)
let frames_with selector p = p |> Soup.select ("frame"^selector)
let frames p = p |> frames_with ""

let to_soup p = p
