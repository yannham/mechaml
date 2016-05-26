let (|?) = Infix.Option.(|?)

module C = Cohttp.Cookie.Set_cookie_hdr

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

  let domain_match host dom =
    let host_length,dom_length = String.length host, String.length dom in
    let delta = host_length - dom_length in
    host=dom
    || (dom_length > 0 && dom.[0]='.' && delta >= 0
      && String.sub host delta (host_length-delta) = dom
      && (String.sub host 0 (delta-1) |> String.contains) '.' |> not)

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


  let make ?expiration ?path ?secure ~domain ~name ~value =
    { name=name;
      value=value;
      expiration=expiration |? `Session;
      domain=domain;
      path=path |? "";
      secure=secure |? false}

  let from_hdr uri c =
    match C.domain c with
    | Some domain ->
      Some { name = C.cookie c |> fst;
        value = C.cookie c |> snd;
        expiration = C.expiration c;
        domain = domain;
        path = C.path c |? "";
        secure = C.secure c }
    | None -> None
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

let map f = JarMap.map
let iter f = JarMap.iter (fun x -> f)
let fold f = JarMap.fold (fun x -> f)

let empty = JarMap.empty

let add c jar =
  match Cookie.expiration c with
  | `Max_age 0L -> JarMap.remove (Key.key c) jar
  | _ -> JarMap.add (Key.key c) c jar

val remove c jar = JarMap.remove (Key.key c) jar

let add_from_headers uri headers jar =
  let add_to_jar jar c =
    match Cookie.from_hdr uri c with
    | Some c' -> add c' jar
    | None -> jar
  in
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
  Cohttp.Header.add headers "cookie" (Buffer.contents buffer)
