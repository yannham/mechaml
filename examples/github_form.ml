(* This file is in the public domain *)

(** Connect to the mechaml github page, fill the search form, submit the form and
   download the results page in a file *)

open Mechaml
module M = Agent.Monad
open M.Infix

let require msg = function
  | Some a -> a
  | None -> failwith msg

let action_search =
  Agent.get "http://github.com/yannham/mechaml/tree/master/src"
  >|= (fun result ->
    let form = result
      |> Agent.HttpResponse.page
      |> Page.form_with "[action$=search]"
      |> require "search form not found" in
    let field = form
      |> Page.Form.field_with "[name=q]"
      |> require "q field not found" in
    Page.Form.Field.set form field "module Form")
  >>= Agent.submit
  >>= (fun response ->
    response
    |> Agent.HttpResponse.content
    |> M.save_content "github-research-result.html")

let _ =
  M.run (Agent.init ()) action_search
