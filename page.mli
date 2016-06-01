type t

module Link = sig
  type t

  val href : t -> string
  val text : t -> string option
  val uri : t -> Uri.t

  val make : ?text:string -> ~href:string -> t
end

module Image = sig
  type t

  val source : t -> string
  val uri : t -> Uri.t
  val caption : t -> string

  val make : ?mime_type:string -> ?caption:string
    -> ~source:string -> t
end

module Frame = sig
  type t

  val source : t -> string
  val uri : t -> Uri.t
  val name : t -> string option
  val text : t -> string option

  val make : ?name:string -> ?text:string -> ~source:string -> t
end

val form_with : string -> t -> Form.t
val forms_with : string -> t -> Form.t list
val forms : t -> Form.t list

val link_with : string -> t -> Image.t
val links_with : string -> t -> Image.t list
val links : t -> Image.t list

val image_with : string -> t -> Image.t
val images_with : string -> t -> Image.t list
val images : t -> Image.t list

val frame_with : string -> t -> Image.t
val frame_with : string -> t -> Image.t list
val frames : t -> Image.t list

val to_soup : t -> Soup.soup Soup.node
