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

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

open Cohttp
open Cohttp_lwt_unix

type http_status_code = Code.status_code
type http_headers = Header.t

module HttpResponse = struct
  type t = { 
    location : Uri.t;
    cohttp_response : Response.t;
    content : string
  }

  let location r = r.location

  let status r =
    Response.status r.cohttp_response

  let status_code r =
    r.cohttp_response
    |> Response.status
    |> Code.code_of_status

  let headers r =
    Response.headers r.cohttp_response

  let content r = r.content

  let page r =
    content r
    |> Page.from_string ~location:r.location

  let cohttp_response r = r.cohttp_response

  let make ~location ~cohttp_response ~content = {location; cohttp_response;
    content}
end

type proxy = {
  user : string option;
  password : string option;
  host : string;
  port : int
}

type t = {
  proxy : proxy option;
  cookie_jar : Cookiejar.t;
  client_headers : Header.t;
  max_redirect : int;
  redirect : int
}

type result = t * HttpResponse.t

let default_max_redirect = 5

let init ?(max_redirect = default_max_redirect) _ =
  { proxy = None;
    cookie_jar = Cookiejar.empty;
    client_headers = Header.init ();
    max_redirect;
    redirect = 0}

let rec redirect (agent,r) =
  match r |> HttpResponse.cohttp_response |> Response.status with
    | `Moved_permanently
    | `Found ->
      (match Header.get (HttpResponse.headers r) "Location" with
        | Some loc ->
          { agent with redirect = succ agent.redirect}
          |> get loc
        | None -> Lwt.return ({ agent with redirect = 0 },r) )
    | _ -> Lwt.return ({ agent with redirect = 0 },r)

and update_agent location agent (cohttp_response,body) =
  let headers = Response.headers cohttp_response in
  let agent =
    {agent with cookie_jar =
      Cookiejar.add_from_headers location headers agent.cookie_jar}
  in
  body
  |> Cohttp_lwt_body.to_string
  >>= (function content ->
    if agent.redirect < agent.max_redirect then
      redirect (agent, HttpResponse.make ~location ~cohttp_response ~content)
    else
      Lwt.return ({ agent with redirect=0 }, HttpResponse.make ~location
        ~cohttp_response ~content))

and get_uri uri agent =
  let headers = agent.cookie_jar
    |> Cookiejar.add_to_headers uri agent.client_headers in
  Client.get ~headers uri
  >>= update_agent uri agent

and get uri_string agent =
  get_uri (Uri.of_string uri_string) agent

let click link = link |> Page.Link.uri |> get_uri

let post_uri uri content agent =
  let headers = agent.cookie_jar
    |> Cookiejar.add_to_headers uri agent.client_headers in
  Client.post ~headers:headers ~body:(Cohttp_lwt_body.of_string content) uri
  >>= update_agent uri agent

let post uri_string content agent =
  post_uri (Uri.of_string uri_string) content agent

let submit form agent =
  let uri = Page.Form.uri form in
  let params = Page.Form.values form in
  let headers = agent.cookie_jar
    |> Cookiejar.add_to_headers uri agent.client_headers in
  Client.post_form ~headers:headers ~params:params uri
  >>= update_agent uri agent

let save_content file data =
  Lwt_io.open_file Lwt_io.output file
  >>= (fun out ->
    Lwt_io.write out data
    |> ignore;
    Lwt_io.close out)

let save_image image file agent =
  let uri = Page.Image.uri image in
  agent
  |> get_uri uri
  >>= (function (agent,response) ->
    save_content file (HttpResponse.content response) 
    >|= fun _ -> (agent,response))

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

let set_max_redirect max_redirect agent = {agent with max_redirect }

module Monad = struct
  type 'a m = t -> (t * 'a) Lwt.t

  let bind (x : 'a m) (f : 'a -> 'b m) =
    fun agent ->
      Lwt.bind (x agent) (fun (agent,result) ->
        f result agent)

  let return (x : 'a) =
    fun agent -> Lwt.return (agent,x)

  let map (f : 'a -> 'b) (x : 'a m) =
    bind x (function y ->
      f y
      |> return)

  let return_from_lwt (x : 'a Lwt.t) =
    fun agent ->
      Lwt.bind x (fun y ->
        Lwt.return (agent,y))

  let run (agent : t) (x : 'a m) =
    Lwt_main.run (x agent)

  module Infix = struct
    let (>>=) = bind

    let (<<=) x f = f >>= x

    let (>>) x y = x >>= (fun _ -> y)

    let (<<) y x = x >> y

    let (>|=) (x : 'a m) (f : 'a -> 'b) = x |> map f

    let (<|=) f x = x |> map f
  end

  let save_content data file =
    save_content data file
    |> return_from_lwt

  let monadic_get g =
    fun agent ->
      Lwt.return (agent, g agent)

  let monadic_set s =
    fun agent ->
      Lwt.return (s agent, ())

  (* let set_proxy ?user ?password ~host ~port = *)
  (*   set_proxy ~user ~password ~host ~port *)
  (*   |> monadic_set *)

  let cookie_jar = monadic_get cookie_jar

  let set_cookie_jar jar =
    set_cookie_jar jar
    |> monadic_set

  let add_cookie cookie =
    add_cookie cookie
    |> monadic_set

  let remove_cookie cookie =
    remove_cookie cookie
    |> monadic_set

  let client_headers = monadic_get client_headers

  let set_client_headers headers =
    set_client_headers headers
    |> monadic_set

  let add_client_header key value =
    add_client_header key value
    |> monadic_set

  let remove_client_header key =
    remove_client_header key
    |> monadic_set

  let set_max_redirect n =
    set_max_redirect n
    |> monadic_set
end
