open Mechaml
module M = Agent.Monad
open M.Infix

type state = Ok of string | Error of string * exn

let image_filename src =
  let last_slash = match String.rindex src '/' with
    | exception Not_found -> 0
    | i -> i+1 in
  String.sub src last_slash (String.length src - last_slash)

let save_images images agent =
  images
  |> List.map (fun img ->
    let file = Page.Image.source img
      |> image_filename
      |> (^) "/tmp/" in
    (file,Agent.save_image file img agent))
  |> Lwt_list.map_p (fun (file,thread) ->
    let f _ =
      thread
      |> Lwt.map (fun _ -> Ok file) in
    let c e =
      Error (file,e)
      |> Lwt.return in
    Lwt.catch f c)
  |> Lwt.map (fun result -> (agent,result))

let action =
  Agent.get "https://ocaml.org/index.fr.html"
  >|= Agent.HttpResponse.page
  >|= Page.images_with "[src$=.png]"
  >>= save_images

let _ =
  action
  |> M.run (Agent.init ())
  |> snd
  |> List.iter (function
    | Ok file ->
      Printf.printf "Image %s successfully downloaded\n" file
    | Error (file,e) ->
      Printf.printf "Image %s : error (%s)\n" file (Printexc.to_string e))
