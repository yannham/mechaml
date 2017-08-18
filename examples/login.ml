open Mechaml
module M = Agent.Monad
open M.Infix

let require msg = function
  | Some a -> a
  | None -> failwith msg

let action_login =
  Agent.get "http://www.somewebsite.com"
  >|= Agent.HttpResponse.page
  >|= (function page ->
    page
    |> Page.form_with "[name=login]"
    |> require "Can't find the login form !"
    |> Page.Form.set "username" ["mynick"]
    |> Page.Form.set "password" ["@xlz43"])
  >>= Agent.submit

let _ =
  M.run (Agent.init ()) action_login

