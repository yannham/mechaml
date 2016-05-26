module Option = struct
  let (>>>) (x,y) f = match x,y with Some x, Some y -> Some (f x y) | _ -> None
  let (>|=) x f = match x with Some x -> Some (f x) | _ -> None
  let (>>) x f = match x,f with Some x, Some f -> Some (f x) | _ -> None
  let (|?) x default = match x with Some x -> x | None -> default
end
