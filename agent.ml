(*{{{ Copyright (C) 2016, Yann Hamdaoui <yann.hamdaoui@centraliens.net>
  Permission to use, copy, modify, and/or distribute this software for
  any purpose with or without fee is hereby granted, provided that the
  above copyright notice and this permission notice appear in all
  copies.
  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
  WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
  AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
  CONSEQUENTIAL
  DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
  DATA
  OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
  OTHER
  TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE
  USE OR
  PERFORMANCE OF THIS SOFTWARE.
  }}}*)

open Lwt
open Cohttp
open Cohttp_lwt_unix

type http_status_code = Cohttp.Code.status_code
type http_header = Cohttp.Header.t

type proxy = {
  user : string option;
  password : string option;
  host : string;
  port : string}

type t = {
  last_page : Page.t option;
  last_headers : Header.t;
  last_status_code : http_status_code;
  last_body : Cohttp_lwt_body.t;
  proxy : proxy option;
  cookie_jar : Cookiejar.t;
  client_headers : Header.t}

let init _ =
  { last_page = None;
    last_headers = Cohttp.Header.init ();
    last_status_code = `Code (-1);
    last_body = Cohttp_lwt_body.of_string "";
    proxy = None;
    cookie_jar = Cookiejar.empty;
    client_headers = Header.init ()}

let update_agent uri agent (response,body) =
  let code = response |> Response.status in
  let headers = response |> Response.headers in
  let page = Page.from_body body in
  {agent with
    last_page = Some page;
    last_headers = headers;
    last_status_code = code;
    last_body = body;
    cookie_jar = Cookiejar.add_from_headers uri headers agent.cookie_jar}

let get uri agent =
  Client.get ~headers:agent.client_headers uri
  >|= update_agent uri agent |> Lwt_main.run

let load image agent = 
  let page = agent.last_page in
  let agent = Client.get ~headers:agent.client_headrs uri
  >|= update_agent uri agent |> Lwt_main.run in
  {agent with last_page = page}

let click link = link |> Page.Link.uri |> get

let post uri agent content =
  Client.post ~headers:agent.client_headers
    ~body:(Cohttp_lwt_body.of_string content) uri
  >|= update_agent uri agent |> Lwt_main.run

let submit form agent =
  let uri = raise (Failure "not implemented") in
  let params = Form.raw_values form in
  Client.post_form ~headers:agent.client_headers ~params:params uri
  >|= update_agent uri agent |> Lwt_main.run

let page agent = agent.last_page
let content agent = agent.last_body |> Cohttp_lwt_body.to_string
let server_headers agent = agent.last_headers
let status_code agent = agent.last_status_code
let code_of_status = Code.code_of_status

let set_proxy ?user ?password ~host ~port agent =
  {agent with proxy = Some ({user = user; password = password;
    host = host; port = port})}

let disable_proxy agent = {agent with proxy = None}

let cookie_jar agent = agent.cookie_jar
let set_cookie_jar cookie_jar agent = {agent with cookie_jar = cookie_jar}
let add_cookie cookie agent =
  {agent with cookie_jar = Cookiejar.add cookie agent.cookie_jar}
let remove_cookie cookie agent =
  {agent with cookie_jar = Cookiejar.remove cookie agent.cookie_jar}

let client_headers agent = agent.client_headers
let set_client_headers headers agent = {agent with client_headers = headers}
let add_client_header header value agent =
  {agent with client_headers = Header.add agent.client_headers header value}
let remove_client_header header agent =
  {agent with client_headers = Header.remove agent.client_headers header}
