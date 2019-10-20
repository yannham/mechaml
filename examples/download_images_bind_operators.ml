(* This file is in the public domain *)

(** Connect to https://ocaml.org/index.fr.html and download all the png images
   of the page in /tmp *)

(** This is the same program as download_images, but where we used the new OCaml
  binding operators introduced in 4.08 and supported in Mechaml through the
  module Mechaml.Agent.Syntax *)

open Mechaml
module M = Agent.Monad
open M.Syntax

type state = Ok of string | Error of string * exn

let image_filename src =
  let last_slash = match String.rindex src '/' with
    | exception Not_found -> 0
    | i -> i+1 in
  String.sub src last_slash (String.length src - last_slash)

let save_images images =
  images
  |> M.List.map_p (fun img ->
    let path = Page.Image.source img
      |> image_filename
      |> (^) "/tmp/" in
    let save _ =
      let* _ = Agent.save_image path img in
      M.return (Ok path) in
    let handler e =
      Error (path,e)
      |> M.return in
    M.catch save handler)

let action_download =
  let* response = Agent.get "https://ocaml.org/index.fr.html" in
  let images =
    response
    |> Agent.HttpResponse.page
    |> Page.images_with "[src$=.png]"
    |> Page.to_list
  in save_images images

let _ =
  action_download
  |> M.run (Agent.init ())
  |> snd
  |> List.iter (function
    | Ok file ->
      Printf.printf "Image %s successfully downloaded\n" file
    | Error (file,e) ->
      e
      |> Printexc.to_string
      |> Printf.printf "Image %s : error (%s)\n" file)
