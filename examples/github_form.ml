open Mechaml
module M = Agent.Monad
open M.Infix

let require msg = function
  | Some a -> a
  | None -> failwith msg

let action =
  Agent.get "http://github.com/yannham/mechaml/tree/master/src"
  >|= Agent.HttpResponse.page
  >|= (function page ->
    let form = page
      |> Page.form_with "[action$=search]"
      |> require "search form not found" in
    let field = form
      |> Page.Form.field_with "[name=q]"
      |> require "q field not found" in
    Page.Form.Field.set form field "module Form")
  >>= Agent.submit

let _ =
  action
  |> M.run (Agent.init ())
  |> snd
  |> Agent.HttpResponse.content
  |> Soup.write_file "github-research-result.html"
