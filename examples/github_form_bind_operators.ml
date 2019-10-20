(* This file is in the public domain *)

(** Connect to the mechaml github page, fill the search form, submit the form and
   download the results page in a file *)

(** This is the same program as github_form, but where we used the new OCaml
  binding operators introduced in 4.08 and supported in Mechaml through the
  module Mechaml.Agent.Syntax *)

open Mechaml
module M = Agent.Monad
open M.Syntax

let require msg = function
  | Some a -> a
  | None -> failwith msg

let action_search =
  let* response = Agent.get "http://github.com/yannham/mechaml/tree/master/src"
  in

  let form = response
      |> Agent.HttpResponse.page
      |> Page.form_with "[action$=search]"
      |> require "search form not found" in
  let field = form
      |> Page.Form.field_with "[name=q]"
      |> require "q field not found" in

  let form = Page.Form.Field.set form field "module Form" in

  let* response = Agent.submit form in
  response
  |> Agent.HttpResponse.content
  |> M.save_content "github-research-result.html"

let _ =
  M.run (Agent.init ()) action_search
