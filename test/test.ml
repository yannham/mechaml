(*{{{ Copyright (C) 2016, Yann Hamdaoui <yann.hamdaoui@centraliens.net>
  Permission to use, copy, modify, and/or distribute this software for any
  purpose with or without fee is hereby granted, provided that the above
  copyright notice and this permission notice appear in all copies.

  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
  REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
  AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
  INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
  OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
  TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
  THIS SOFTWARE.
  }}}*)

open Mechaml
open Cohttp

let _ = Random.self_init ()

(** Helper functions for cookiejar testing **)

let random_char () =
  Random.int(26) + (Char.code 'a')
  |> Char.chr

let random_string n =
  String.init n (fun _ -> random_char ())

let between min max =
  Random.int(max-min) + min

let random_uri () =
  let host = random_string (between 5 10) in
  let ext = random_string 2 in
  let path = random_string (between 5 10) in
  Printf.sprintf "http://%s.%s/%s" host ext path
  |> Uri.of_string

let random_cookie domain =
  let name = random_string (between 5 10) in
  let value = random_string (between 5 10) in
  (* let domain = (random_string (between 5 10))^"."^(random_string 2) in *)
  Cookiejar.Cookie.make ~domain name value

let rec random_cookies domain = function
  | 0 -> []
  | n -> (random_cookie domain)::(random_cookies domain (n-1))

let jar_from =
  List.fold_left (fun j c -> Cookiejar.add c j) Cookiejar.empty

let uri = random_uri ()
let cookies = random_cookies (Option.value (Uri.host uri) ~default:"dunno.com") 10
let jar = jar_from cookies

let jar_mem c jar =
  let f x c =
    if x=c then
      raise Exit
    else
      c
  in match Cookiejar.fold f jar c with
    | exception Exit -> true
    | _ -> false

let rec jar_eq cookies jar =
  match cookies with
    | [] -> Cookiejar.is_empty jar
    | c::cs ->
      (match jar_mem c jar with
        | true -> jar_eq cs (Cookiejar.remove c jar)
        | false -> false)

let to_set_cookie cookie =
  let name = Cookiejar.Cookie.name cookie in
  let value = Cookiejar.Cookie.value cookie in
  let domain = Cookiejar.Cookie.domain cookie in
  Printf.sprintf "%s=%s; Domain=%s" name value domain

let to_set_cookies cookies =
  let rec f s = function | [] -> s
  | [c] ->
    s^(to_set_cookie c)
  | c::cs ->
    let s = Printf.sprintf "%s%s, " s (to_set_cookie c) in
    f s cs in
  f "" cookies

(** Helper functions for page testing **)

let soup_index =
  Soup.read_file "page/index.html"
  |> Soup.parse

module type PageElement = sig
  type t
  val to_node : t -> Soup.element Soup.node
end

let check_bool msg = Alcotest.(check bool) msg
let check_true msg = check_bool msg true
let check_string = Alcotest.(check string)
let check_int = Alcotest.(check int)
let fail s = Alcotest.fail s

let test_selector (type s) page f (module M : PageElement with type t = s) node prefix selector expected_count =
  let nodes = f selector page in
  let msg c = Printf.sprintf "%s [%d/%d]" (prefix^selector) c expected_count in
  nodes
  |> Page.fold (fun c _ -> succ c) 0
  |> (fun c -> check_int (msg c) expected_count c);
  nodes
  |> Page.iter (fun x ->
    x
    |> M.to_node
    |> Soup.name
    |> check_string (prefix^":bad node type") node)

let test_selector_inputs form f input prefix selector expected_count =
  let inputs = f selector form in
  let msg c = Printf.sprintf "%s:%s [%d/%d]" prefix selector c expected_count in
  let node_name = match input with
    | "textarea" | "select" -> input
    | _ -> "input" in
  inputs
  |> Page.fold (fun c _ -> succ c) 0
  |> (fun c -> check_int (msg c) expected_count c);
  inputs
  |> Page.iter (fun x ->
    x
    |> Page.Form.input_to_node
    |> Soup.name
    |> check_string (prefix^":bad node type") node_name;
    match input with
      | "textarea" | "select" -> ()
      | _ ->
        x
        |> Page.Form.input_to_node
        |> Soup.attribute "type"
        |> (function
          | Some t ->
            check_string (prefix^":bad input type") t input
          | None ->
            fail (prefix^":input node without type attribute")
        ))

let tests_cookiejar = [
  "add", `Quick, (fun _ ->
    jar
    |> jar_eq cookies
    |> check_true "mismatch between generated jar and original cookie list");

  "remove", `Quick, (fun _ ->
    List.fold_left (fun jar cookie -> Cookiejar.remove cookie jar) jar cookies
    |> Cookiejar.is_empty
    |> check_true "expected empty jar after removing cookies");

  "add_from_headers", `Quick, (fun _ ->
    let headers_single =
      Header.init_with "Set-Cookie" (to_set_cookies cookies) in
    Cookiejar.empty
    |> Cookiejar.add_from_headers uri headers_single
    |> jar_eq [(List.hd cookies)]
    |> check_true "Mismatch between jar generated from one \
      \"Set-Cookie\" header containing all cookies as a list and original cookie\
      list";

    let headers_mult =
      List.fold_left
        (fun h c -> Header.add h "Set-Cookie" (to_set_cookie c))
        (Header.init ()) cookies in
    Cookiejar.empty
    |> Cookiejar.add_from_headers uri headers_mult
    |> jar_eq cookies
    |> check_true "Mismatch between a jar generated from multiple \
      \"Set-Cookie\" headers and the first cookie of the original list";

    let cookie = random_cookie (Option.value (Uri.host uri) ~default:"dunno.com") in
    let name = Cookiejar.Cookie.name cookie in
    let value = Cookiejar.Cookie.value cookie in
    let domain = Cookiejar.Cookie.domain cookie in
    let cookie_uri = Printf.sprintf "http://%s/a.php" domain
    |> Uri.of_string in
    let header_nodomain =
      Header.init_with "Set-Cookie" (Printf.sprintf "%s=%s" name value) in
    Cookiejar.empty
    |> Cookiejar.add_from_headers cookie_uri header_nodomain
    |> jar_eq [cookie]
    |> check_true "Mismatch between jar generated from a domain-less \
      \"Set-Cookie\" header and the original cookie")

  (*"add_to_headers", `Quick, (fun _ ->
    let _ = jar
      |> Cookiejar.add_to_headers uri (Header.init ())
      |> Header.iter (fun s ls ->
        Printf.printf("%s: %s") s @@ List.fold_left (^) "" ls
      ) in
    true |> check_true "Mismatch between the original jar and the jar generated \
      using headers")*)
]

let tests_page = [
  "forms", `Quick, (fun _ ->
    let page = soup_index |> Page.from_soup in
    let forms_with =
      test_selector page Page.forms_with (module Page.Form)
        "form" "forms_with" in

    forms_with "[id=form-one]" 1;
    forms_with "[id=form-two]" 1;
    forms_with "[id=form-none]" 0;

    forms_with "form[id=form-one]" 1;
    forms_with "form[id=form-none]" 0;
    forms_with "li" 0;
    (* forms_with "li, form" 0; *)
    forms_with "li[id=form-one]" 0;

    forms_with ".noneclass" 0;

    forms_with "" 2;
    forms_with "*" 2;
    forms_with "form" 2;
    forms_with ".formclass" 2;
    forms_with "div > form" 1);
    (* forms_with "li + form" 1); *)

  "forms_input", `Quick, (fun _ ->
    let module F = Page.Form in
    let form = ref (soup_index
      |> Page.from_soup
      |> Page.form_with "[id=form-one]"
      |> Soup.require) in
    let checkboxes_with =
      test_selector_inputs !form F.checkboxes_with "checkbox" "checkboxes_with"
    in

    checkboxes_with "[name=check1]" 3;
    checkboxes_with "[name=check1][value=choice1]" 1;
    checkboxes_with "[name=nothere]" 0;
    checkboxes_with "" 6;

    let check1_choice1 =
      !form |> F.checkbox_with "[name=check1][value=choice1]" in
    let check1_choice2 =
      !form |> F.checkbox_with "[name=check1][value=choice2]" in

    (match check1_choice1,check1_choice2 with
      | _,None | None,_ ->
        fail "checkbox_with [name=check1][value=choice1/2] found none"
      | Some c1,Some c2 ->
        form := c1 |> F.Checkbox.check !form;
        form := c2 |> F.Checkbox.check !form;
        c1
        |> F.Checkbox.is_checked !form
        |> check_true "checkbox_choice1 (checked?)";
        c2
        |> F.Checkbox.is_checked !form
        |> check_true "checkbox_choice2 (checked?)";
        form := c2 |> F.Checkbox.uncheck !form;
        c2
        |> F.Checkbox.is_checked !form
        |> not |> check_true "checkbox_choice2 (unchecked?)");

    let radio_buttons_with =
      test_selector_inputs !form F.radio_buttons_with "radio"
        "radiobuttons_with"
    in

    radio_buttons_with "[name=radio1]" 3;
    radio_buttons_with "[name=radio1][value=choice1]" 1;
    radio_buttons_with "[name=nothere]" 0;
    radio_buttons_with "" 6;

    let radio1_choice1 =
      !form |> F.radio_button_with "[name=radio1][value=choice1]" in
    let radio1_choice2 =
      !form |> F.radio_button_with "[name=radio1][value=choice2]" in

    (match radio1_choice1,radio1_choice2 with
      | _,None | None,_ ->
        fail "radio_button_with [name=radio1][value=choice1/2] found none"
      | Some c1,Some c2 ->
        form := c1 |> F.RadioButton.select !form;
        form := c2 |> F.RadioButton.select !form;
        c2
        |> F.RadioButton.is_selected !form
        |> check_true "radio_button_choice2 (selected?)";
        c1
        |> F.RadioButton.is_selected !form
        |> not |> check_true "radio_button_choice1 (unselected?)");

    let selects_with =
      test_selector_inputs !form F.select_lists_with "select" "select_lists_with"
    in

    selects_with "[name=select1]" 1;
    selects_with "[name=select2]" 1;
    selects_with "[name=nothere]" 0;
    selects_with "" 2;

    let test_selection12 name results = (
      let sl =
        !form
        |> F.select_list_with (Printf.sprintf "[name=%s]" name)
        |> Soup.require in
      let items = sl |> F.SelectList.items in
      match items,results with
        | [x;y;z], [res_x;res_y] ->
          form := x |> F.SelectList.select !form sl;
          form := y |> F.SelectList.select !form sl;
          let msg item =
            Printf.sprintf "%s : %s (selected ?)"
              (F.iname sl |> Soup.require)
              (F.SelectList.value item) in
          x
          |> F.SelectList.is_selected !form sl
          |> check_bool (msg x) res_x;
          y
          |> F.SelectList.is_selected !form sl
          |> check_bool (msg y) res_y;
          z
          |> F.SelectList.is_selected !form sl
          |> check_bool (msg z) false
        | _ ->
          items
          |> List.length
          |> Printf.sprintf "%s : has %d items, expected 3" (F.iname sl |>
            Soup.require)
          |> fail) in

    let check_multiple name result =
      !form
      |> F.select_list_with (Printf.sprintf "[name=%s]" name)
      |> Soup.require
      |> F.SelectList.is_multiple
      |> check_bool (Printf.sprintf "%s : is_multiple?" name) result in

    check_multiple "select1" false;
    check_multiple "select2" true;
    test_selection12 "select1" [false;true];
    test_selection12 "select2" [true;true];

    let fields_with t =
      test_selector_inputs !form F.fields_with t "select_field"
    and texts_with =
      test_selector_inputs !form F.texts_with "text" "select_text"
    and passwords_with =
      test_selector_inputs !form F.passwords_with "password"
        "select_password"
    and textareas_with =
      test_selector_inputs !form F.textareas_with "textarea" "select_textarea"
    in

    texts_with "[name=text1]" 1;
    fields_with "text" "[name=text1]" 1;
    texts_with "[name=text2]" 1;
    fields_with "text" "[name=text2]" 1;
    texts_with "[name=text-none]" 0;
    fields_with "text" "[name=text-none]" 0;
    texts_with "" 2;

    passwords_with "[name=password1]" 1;
    fields_with "password" "[name=password1]" 1;
    passwords_with "[name=password2]" 1;
    fields_with "password" "[name=password2]" 1;
    passwords_with "[name=password-none]" 0;
    fields_with "password" "[name=password-none]" 0;
    passwords_with "" 2;

    textareas_with "[name=area1]" 1;
    fields_with "textarea" "[name=area1]" 1;
    textareas_with "[name=area2]" 1;
    fields_with "textarea" "[name=area2]" 1;
    textareas_with "[name=area-none]" 0;
    fields_with "textarea" "[name=area-none]" 0;
    textareas_with "" 2;

    let check_content selector =
      let field = !form |> F.field_with selector |> Soup.require
      and content = random_string 20 in
      form := F.Field.set !form field content;
      F.Field.get !form field
      |> Soup.require
      |> (=) content
      |> check_true ("consistency of field's content"^selector)
    in

    check_content "[name=text1]";
    check_content "[name=password1]";
    check_content "[name=area1]");

  "form_default_values", `Quick, (fun _ ->
    let module F = Page.Form in
    let form = soup_index
      |> Page.from_soup
      |> Page.form_with "[id=form-two]"
      |> Soup.require in

    let check name select checker value is_checked =
      let selector = Printf.sprintf "[name=%s][value=%s]" name value in
      form
      |> select selector
      |> Soup.require
      |> checker form
      |> check_bool (Printf.sprintf "%s default value" name) is_checked in

    let check_cb = check "check3" F.checkbox_with F.Checkbox.is_checked in
    check_cb "choice1" false;
    check_cb "choice2" true;
    check_cb "choice3" true;

    let check_rb = check "radio3" F.radio_button_with F.RadioButton.is_selected
    in
    check_rb "choice1" true;
    check_rb "choice2" false;

    form
    |> F.select_list_with "[name=select3]"
    |> Soup.require
    |> (fun sl ->
      sl
      |> F.SelectList.items
      |> List.iter (fun item ->
        let is_selected = match F.SelectList.value item with
          | "choice1" | "choice2" -> true
          | _ -> false in
        item
        |> F.SelectList.is_selected form sl
        |> check_bool "select3 default selection" is_selected));

    let check_field name value =
      form
      |> F.field_with (Printf.sprintf "[name=%s]" name)
      |> Soup.require
      |> F.Field.get form
      |> Soup.require
      |> check_string (Printf.sprintf "%s default value" name) value in

    check_field "hidden" "hidden";
    check_field "text3" "default";
    check_field "area3" "default");

  "forms_format", `Quick, (fun _ ->
    let color_sample = [
      ((0,0,0), Some "#000000");
      ((255,255,255), Some "#FFFFFF");
      ((128,128,128), Some "#808080");
      ((30,75,145), Some "#1E4B91");
      ((-10,128,128), None);
      ((300,128,128), None);
      ((128,-10,128), None);
      ((128,300,128), None);
      ((128,128,-10), None);
      ((128,128,300), None)] in let date_sample = [
      ((31,1,2016), Some "2016-01-31");
      ((31,3,2016), Some "2016-03-31");
      ((31,5,2016), Some "2016-05-31");
      ((31,7,2016), Some "2016-07-31");
      ((31,8,2016), Some "2016-08-31");
      ((31,12,2016), Some "2016-12-31");

      ((31,2,2016), None);
      ((31,4,2016), None);
      ((31,6,2016), None);
      ((31,9,2016), None);
      ((31,11,2016), None);

      ((29,2,1824), Some "1824-02-29");
      ((29,2,800), Some "800-02-29");
      ((29,2,1823), None);
      ((29,2,801), None);
      ((30,2,2524), None);
      ((30,2,1825), None);

      ((20, 5,-5), None);
      ((20, -1,2000), None);
      ((20, 15,2000), None);
      ((-10, 5,2000), None);
      ((40, 5,2000), None)] in
    (* let email_sample = [ *)
    (*   ("email@example.com", Some "email@example.com"); *)
    (*   ("firstname.lastname@example.com", Some "firstname.lastname@example.com"); *)
    (*   ("email@subdomain.example.com", Some "email@subdomain.example.com"); *)
    (*   ("firstname+lastname@example.com", Some "firstname+lastname@example.com"); *)
    (*   ("email@123.123.123.123", Some "email@123.123.123.123"); *)
    (*   ("email@[123.123.123.123]", Some "email@[123.123.123.123]"); *)
    (*   ("“email”@example.com", Some "“email”@example.com"); *)
    (*   ("1234567890@example.com", Some "1234567890@example.com"); *)
    (*   ("email@example-one.com", Some "email@example-one.com"); *)
    (*   ("_______@example.com", Some "_______@example.com"); *)
    (*   ("email@example.name", Some "email@example.name"); *)
    (*   ("email@example.museum", Some "email@example.museum"); *)
    (*   ("email@example.co.jp", Some "email@example.co.jp"); *)
    (*   ("firstname-lastname@example.com", Some "firstname-lastname@example.com"); *)
    (*   ("much.“more\\ unusual”@example.com", Some "much.“more\\ unusual”@example.com"); *)
    (*   ("very.unusual.“@”.unusual.com@example.com", Some "very.unusual.“@”.unusual.com@example.com"); *)
    (*   ("very.“(),:;<>[]”.VERY.“very@\\\\ \"very”.unusual@strange.example.com", Some "very.“(),:;<>[]”.VERY.“very@\\\\ \"very”.unusual@strange.example.com"); *)
    (*  *)
      (* ("plainaddress", None); *)
      (* ("#@%^%#$@#$@#.com", None); *)
      (* ("@example.com", None); *)
      (* ("Joe Smith <email@example.com>", None); *)
      (* ("email.example.com", None); *)
      (* ("email@example@example.com", None); *)
      (* (".email@example.com", None); *)
      (* ("email.@example.com", None); *)
      (* ("email..email@example.com", None); *)
      (* ("あいうえお@example.com", None); *)
      (* ("email@example.com (Joe Smith)", None); *)
      (* ("email@example", None); *)
      (* ("email@-example.com", None); *)
      (* ("email@example.web", None); *)
      (* ("email@111.222.333.44444", None); *)
      (* ("email@example..com", None); *)
      (* ("Abc..123@example.com", None); *)
      (* ("“(),:;<>[\\]@example.com", None); *)
      (* ("just\"not\"right@example.com", None); *)
      (* ("this\\ is\"really\"not\\allowed@example.com", None)] in *)
    let time_sample = [
      ((0,0,0), Some "00:00:00");
      ((10,20,30), Some "10:20:30");
      ((14,8,48), Some "14:08:48");
      ((23,55,3), Some "23:55:03");

      ((-1,30,30), None);
      ((35,30,30), None);
      ((12,-1,30), None);
      ((12,60,30), None);
      ((12,30,-1), None);
      ((12,30,60), None)] in

    let check3 f_name (type s) (module M : Format.S with type t = s) f ((x,y,z),expected) =
      let result = match f x y z with
        | Some x -> Some (M.to_string x)
        | None -> None in
      let expected_string = match expected with
        | Some s -> s
        | None -> "None" in
      let msg = Printf.sprintf "%s on (%d,%d,%d) : expected %s" f_name x y z
        expected_string in
      check_true msg (result = expected) in

    (* let check_email (email,expected) = *)
    (*   let result = match Format.Email.make email with *)
    (*     | Some x -> Some (Format.Email.to_string x) *)
    (*     | None -> None in *)
    (*   let expected_string = match expected with *)
    (*     | Some s -> s *)
    (*     | None -> "None" in *)
    (*   let msg = Printf.sprintf "email on %s : expected %s" email expected_string in *)
    (*   check_true msg (result = expected) in *)

    let color_unlabeled x y z = Format.Color.make ~red:x ~green:y ~blue:z in
    let date_unabeled x y z = Format.Date.make ~day:x ~month:y ~year:z in
    let time_unlabeled x y z = Format.Time.make ~hour:x ~minute:y ~second:z
      in
    List.iter (check3 "color" (module Format.Color) color_unlabeled) color_sample;
    List.iter (check3 "date" (module Format.Date) date_unabeled) date_sample;
    List.iter (check3 "time" (module Format.Time) time_unlabeled)
      time_sample);
    (* List.iter check_email email_sample); *)

  "links", `Quick, (fun _ ->
    let page = soup_index |> Page.from_soup in
    let links_with =
      test_selector page Page.links_with (module Page.Link) "a" "links_with" in

    links_with "[id=a-one]" 1;
    links_with "[id=a-two]" 1;
    links_with "[id=a-none]" 0;

    links_with "a[id=a-one]" 1;
    links_with "a[id=a-none]" 0;
    links_with "ul" 0;
    (* links_with "ul, a" 0; *)
    links_with "ul[id=a-one]" 0;

    links_with ".noneclass" 0;

    links_with "" 3;
    links_with "*" 3;
    links_with "[href^=https]" 1;
    links_with "[href$=.html]" 1;
    links_with "[href*=http]" 3;
    links_with "a" 3;
    links_with ".aclass" 2;
    links_with "div > a" 1);
    (* links_with "ul + a" 1); *)

  "images", `Quick, (fun _ ->
    let page = soup_index |> Page.from_soup in
    let images_with =
      test_selector page Page.images_with (module Page.Image) "img" "images_with" in

    images_with "[id=img1]" 1;
    images_with "[id=img2]" 1;
    images_with "[id=imgnone]" 0;

    images_with "img[id=img1]" 1;
    images_with "img[id=imgnone]" 0;
    images_with "div" 0;
    (* images_with "div, img" 0; *)
    images_with "div[id=img1]" 0;

    images_with ".noneclass" 0;

    images_with "" 3;
    images_with "*" 3;
    images_with "[src^=https]" 1;
    images_with "[src$=.jpg]" 1;
    images_with "[src*=http]" 3;
    images_with "img" 3;
    images_with ".imgclass" 2;
    (* images_with "table > img" 1; *)
    images_with "div + img" 1)
]

let test_suite = [
  "Cookiejar", tests_cookiejar;
  "Page", tests_page;
]

let _ =
  Alcotest.run "Mechaml" test_suite
