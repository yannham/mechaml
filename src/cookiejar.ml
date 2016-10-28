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

let (|?) = Infix.Option.(|?)

module C = Cohttp.Cookie.Set_cookie_hdr

let domain_from_uri uri =
  Uri.host uri |? "zboub.com"

module Cookie = struct
  type expiration = [
    | `Session
    | `Max_age of int64
  ]
  type t = { name : string;
    value : string;
    expiration : expiration;
    domain : string;
    path : string;
    secure : bool }

  let name c = c.name
  let value c = c.value
  let expiration c = c.expiration
  let domain c = c.domain
  let path c = c.path
  let secure c = c.secure

  (* let domain_match host dom = *)
  (*   let host_length,dom_length = String.length host, String.length dom in *)
  (*   let delta = host_length - dom_length in *)
  (*   host=dom *)
  (*   || (dom_length > 0 && dom.[0]='.' && delta >= 0 *)
  (*     && String.sub host delta (host_length-delta) = dom *)
  (*     && (String.sub host 0 (delta-1) |> String.contains) '.' |> not) *)

  let domain_match host dom =
    let host_length,dom_length = String.length host, String.length dom in
    let delta = host_length - dom_length in
    host=dom
    || (delta > 0
      && String.sub host delta (host_length-delta) = dom
      && host.[delta-1] = '.')

  let path_match uri_path cookie_path =
    let u_length, c_length = String.length uri_path,
      String.length cookie_path in
    u_length >= c_length && String.sub uri_path 0 c_length = cookie_path

  let match_uri uri cookie =
    match Uri.host uri, domain cookie with
    | Some host, dom ->
      domain_match host dom
      && path_match (Uri.path uri) (path cookie)
    | _ -> false


  let make ?(expiration = `Session) ?(path = "")  ?(secure = false) ~domain name value =
    { name = name;
      value = value;
      expiration = expiration;
      domain = domain;
      path = path;
      secure = secure}

  let from_hdr uri c =
    { name = C.cookie c |> fst;
      value = C.cookie c |> snd;
      expiration = C.expiration c;
      domain = C.domain c |? domain_from_uri uri;
      path = C.path c |? "";
      secure = C.secure c }
end

module Key = struct
  type t = {name: string; domain : string; path : string}

  let key c =
    {name = Cookie.name c;
    domain = Cookie.domain c;
    path = Cookie.path c}

  let to_string k = k.name ^ k.domain ^ k.path

  let compare k k' = String.compare (to_string k) (to_string k')
end

module JarMap = Map.Make(Key)

type t = Cookie.t JarMap.t

let map = JarMap.map
let iter f = JarMap.iter (fun x -> f)
let fold f = JarMap.fold (fun x -> f)
let is_empty = JarMap.is_empty

let empty = JarMap.empty

let add c jar =
  match Cookie.expiration c with
  | `Max_age 0L -> JarMap.remove (Key.key c) jar
  | _ -> JarMap.add (Key.key c) c jar

let remove c jar = JarMap.remove (Key.key c) jar

let add_from_headers uri headers jar =
  let add_to_jar jar c = add (Cookie.from_hdr uri c) jar in
  C.extract headers
  |> List.map snd
  |> List.fold_left add_to_jar jar

let add_to_headers uri headers jar =
  let buffer = Buffer.create 64 in
  let to_header c first =
    match Cookie.match_uri uri c with
    | true ->
      let sep = (match first with
        | true -> ""
        | false -> "; ")
      in
      Printf.bprintf buffer "%s%s=%s" sep (Cookie.name c) (Cookie.value c);
      false
    | false -> first
  in
  fold to_header jar true |> ignore;
  Cohttp.Header.add headers "Cookie" (Buffer.contents buffer)
