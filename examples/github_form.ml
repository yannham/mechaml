open Mechaml
module M = Agent.Monad
open M.Infix

let requireM msg = function
  | Some a -> M.return a
  | None ->
    (Printf.sprintf "Require failed : %s" msg
    |> M.fail_with)

let action =
  Agent.get "http://github.com/yannham/mechaml/tree/master/src"
  >|= Agent.HttpResponse.page
  >>= (function page ->
    page
    |> Page.form_with "[action$=search]"
    |> requireM "form")
  >>= (function form ->
    form
    |> Page.Form.field_with "[name=q]"
    |> requireM "field"
    >|= (function field ->
    Page.Form.Field.set form field "module Form"))
  >>= Agent.submit

let _ =
  action
  |> M.run (Agent.init ())
  |> snd
  |> Agent.HttpResponse.content
  |> Soup.write_file "github-research-result.html"
