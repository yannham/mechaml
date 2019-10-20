(* This file is in the public domain *)

(** Connect to reddit then fill and submit the login form then download the
   resulting page *)

(** This is the same program as reddit_login, but where we used the new OCaml
  binding operators introduced in 4.08 and supported in Mechaml through the
  module Mechaml.Agent.Syntax *)

open Mechaml
module M = Agent.Monad
open M.Syntax

let require msg = function
  | Some a -> a
  | None -> failwith msg

let action_login =
  let* response = Agent.get "https://www.reddit.com" in

  let form =
    response
    |> Agent.HttpResponse.page
    |> Page.form_with "[id=login_login-main]"
    |> require "Can't find the login form !"
    |> Page.Form.set "user" "mynick"
    |> Page.Form.set "passwd" "@xlz43" in

  let* response = Agent.submit form in
    response
    |> Agent.HttpResponse.content
    |> M.save_content "reddit-login.html"

let _ =
  M.run (Agent.init ()) action_login
