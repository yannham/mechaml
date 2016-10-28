open Lwt
open Cohttp
open Cohttp_lwt_unix

let body =
  Client.get (Uri.of_string "http://www.reddit.com/") >|= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  Response.headers resp
  (*body |> Cohttp_lwt_body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body*)

let parse_cookie () = 
  let headers = Lwt_main.run body in
  CookieJar.update headers

let form =
  Client.post_form test_form (Uri.of_string "http://httpbin.org/post")
  >>= fun (resp,body) ->
    let code = resp |> Response.status |> Code.code_of_status in
    Printf.printf "Response code: %d\n" code;
    Printf.printf "Headers: %s\n" (resp |> Response.headers |>
    Header.to_string);
    body |> Cohttp_lwt_body.to_string >|= fun body ->
    Printf.printf "Body of length: %d\n" (String.length body);
    body

(*let () =
  let body = Lwt_main.run form in
  print_endline ("Received body" ^ body)*)

let test_form = 
  [("champ1","val1");("champ2","val2")]
  |> List.map (function (x,y) -> (x,[y]))
