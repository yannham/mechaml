open OUnit2
open Mechaml
open Cohttp

let jar_head jar =
  let f cookie = function
    | None -> Some cookie
    | c -> c
  in Cookiejar.fold f jar None

let test_cookie =
  Cookiejar.Cookie.make ~domain:"abc.com" ~name:"test" ~value:"foo"

let second_cookie =
  Cookiejar.Cookie.make ~domain:"azerty.io.fr" ~name:"second" ~value:"bar"

let suites = [
  "cookiejar" >::: [
    ("add" >:: fun _ ->
      Cookiejar.empty |> Cookiejar.add test_cookie |> jar_head
      |> assert_equal test_cookie
    );

    ("remove" >:: fun _ ->
      Cookiejar.empty |> Cookiejar.add test_cookie
      |> Cookiejar.remove test_cookie |> jar_empty
      |> assert_bool "expected empty jar");

    ("add_from_headers" >:: fun _ -> ()
      let headers_simpl = Headers.init_with "Set-Cookie"
        "test = foo ; domain = abc.com" in
      let headers_comma = Headers.init_with "Set-Cookie"
        "test = foo ; domain = abc.com, second = bar ; domain = azerty.io.fr"
      in
      let headers_mult = Headers.init ()
      |> Headers.add "Set-Cookie" "test = foo ; domain = abc.com"
      |> Headers.add "Set-Cookie" "second = bar ; domain = azerty.io.fr"
      in ()
      (**)
    );

    ("add_to_headers" >:: fun _ -> ()
      (**)
    );
