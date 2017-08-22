(* This file is in the public domain *)

(** Connect to reddit then fill and submit the login form then download the
   resulting page *)

open Mechaml
module M = Agent.Monad
open M.Infix

let require msg = function
  | Some a -> a
  | None -> failwith msg

let action_login =
  Agent.get "https://www.reddit.com"
  >|= (fun response ->
    response
    |> Agent.HttpResponse.page
    |> Page.form_with "[id=login_login-main]"
    |> require "Can't find the login form !"
    |> Page.Form.set "user" "mynick"
    |> Page.Form.set "passwd" "@xlz43")
  >>= Agent.submit
  >>= (fun response ->
    response
    |> Agent.HttpResponse.content
    |> M.save_content "reddit-login.html")

let _ =
  M.run (Agent.init ()) action_login
