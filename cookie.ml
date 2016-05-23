open Re_pcre
open Cohttp
open Cohttp_lwt_unix

let is_suffix s suff =
  let s_length,suff_length = String.length s, String.length suff in
  let delta = s_length-suff_length in
  if delta < 0 then false
  else String.sub s delta (s_length-delta) = suff

let (=~) s r = Re.exec_opt r s

module CookieJar = struct
  let default_jar_size = 20
  let uri_pattern = regexp "(?:^[a-z]+://)?([^/]+)" 

  let cookies = Hashtbl.create default_jar_size 
  
  let update header =
    let setter = function (_,c) ->
      let name,_ = Cookie.Set_cookie_hdr.cookie c in
      Hashtbl.replace cookies name c
    in
    Cookie.Set_cookie_hdr.extract header
    |> List.iter setter

  let domain_match uri cookie = 
    let domain = match uri =~ uri_pattern with
      Some groups -> 
        Printf.printf "matched ! domain=%s\n" (Re.Group.get groups 1);
        Some (Re.Group.get groups 1)
      | None -> Printf.printf "not matched...\n"; None
    in
    match domain,Cookie.Set_cookie_hdr.domain cookie with
      | Some dom,Some c_dom ->
          let c_dom = match String.get c_dom 0 with
            '.' -> c_dom
            | _ -> ("."^c_dom) in
          Printf.printf "cookie domain : %s\n" c_dom;
          Printf.printf "Suffix : %b \n" (is_suffix dom c_dom);
          is_suffix dom c_dom
      | _ -> true

  let generate_headers uri = 
    let adder name cookie headers =
      let k,v = Cookie.Set_cookie_hdr.cookie cookie in
      match domain_match uri cookie with
        true -> 
          Printf.sprintf "%s=%s" k v
          |> Header.add headers "cookie"
        | false -> headers
    in
    Hashtbl.fold adder cookies (Header.init ()) 

end
