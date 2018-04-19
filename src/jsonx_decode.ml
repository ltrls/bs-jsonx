module Dict = Map.Make(String)

type ('a, 'b) decoder = 
    Decoder of ((Js.Json.t * 'a) -> 'b)

(* helpers *)

let is_int v =
  let c = classify_float (fst (modf v)) in
  c == FP_zero

let identity x = x

(* Run Decoder *)

let decode_string (Decoder decode) s =
    try 
        let json = Js.Json.parseExn s in
        try
            Js.Result.Ok (decode (json, identity))
        with
            Failure msg -> Js.Result.Error msg
    with
      _ -> Js.Result.Error ( Printf.sprintf "failed to decode json string: %s" s )
      

(* Primitive Decoders *)

let string = Decoder (fun (json, value) -> 
    match Js.Json.classify json with
        | JSONString str -> value str
        | _ -> failwith ( Printf.sprintf "failed to decode json as string: %s" ( Js.Json.stringify json ) )
)    

let bool = Decoder (fun (json, value) -> 
    match Js.Json.classify json with
        | JSONTrue -> value true
        | JSONFalse -> value false
        | _ -> failwith ( Printf.sprintf "failed to decode json as bool: %s" ( Js.Json.stringify json ) )
)    

let int = Decoder (fun (json, value) -> 
    let fail () = failwith ( Printf.sprintf "failed to decode json as int: %s" ( Js.Json.stringify json ) ) in
    match Js.Json.classify json with
        | JSONNumber f -> 
            if is_int f 
            then value (int_of_float f)
            else fail ()
        | _ -> fail ()
)  

let float = Decoder (fun (json, value) -> 
    match Js.Json.classify json with
        | JSONNumber f -> value f
        | _ -> failwith ( Printf.sprintf "failed to decode json as float: %s" ( Js.Json.stringify json ) ) 
)

let null sub_value = Decoder (fun (json, value) -> 
    match Js.Json.classify json with
        | JSONNull -> value sub_value
        | _ -> failwith ( Printf.sprintf "failed to decode json as null: %s" ( Js.Json.stringify json ) )
)

(* Inconsistent Structure *)

let optional (Decoder decode) = Decoder (fun (json, value) -> 
    try decode (json, (fun x -> value (Some x)))
    with _ -> (value None)   
)

let one_of decoders = Decoder (fun (json, value) -> 
    let rec first ds = 
        match ds with
            | (Decoder decode) :: xs ->
                (try decode (json, value)
                with _ -> first xs)
            | [] -> failwith ( Printf.sprintf "failed to decode json using any one of the provided decoders: %s" ( Js.Json.stringify json ) )
    in 
        first decoders
)

(* Mapping *)

let map sub_value (Decoder d1) = Decoder (fun (json, value) -> 
    let v1 = d1 (json, identity) in
    value (sub_value v1)
)

let map2 sub_value (Decoder d1) (Decoder d2) = Decoder (fun (json, value) -> 
    let v1 = d1 (json, identity) in
    let v2 = d2 (json, identity) in
    value (sub_value v1 v2)
)

let map3 sub_value (Decoder d1) (Decoder d2) (Decoder d3) = Decoder (fun (json, value) -> 
    let v1 = d1 (json, identity) in
    let v2 = d2 (json, identity) in
    let v3 = d3 (json, identity) in
    value (sub_value v1 v2 v3)
)

let map4 sub_value (Decoder d1) (Decoder d2) (Decoder d3) (Decoder d4) = Decoder (fun (json, value) -> 
    let v1 = d1 (json, identity) in
    let v2 = d2 (json, identity) in
    let v3 = d3 (json, identity) in
    let v4 = d4 (json, identity) in
    value (sub_value v1 v2 v3 v4)
)

let map5 sub_value (Decoder d1) (Decoder d2) (Decoder d3) (Decoder d4) (Decoder d5) = Decoder (fun (json, value) -> 
    let v1 = d1 (json, identity) in
    let v2 = d2 (json, identity) in
    let v3 = d3 (json, identity) in
    let v4 = d4 (json, identity) in
    let v5 = d5 (json, identity) in
    value (sub_value v1 v2 v3 v4 v5)
)

let map6 sub_value (Decoder d1) (Decoder d2) (Decoder d3) (Decoder d4) (Decoder d5) (Decoder d6) = Decoder (fun (json, value) -> 
    let v1 = d1 (json, identity) in
    let v2 = d2 (json, identity) in
    let v3 = d3 (json, identity) in
    let v4 = d4 (json, identity) in
    let v5 = d5 (json, identity) in
    let v6 = d6 (json, identity) in
    value (sub_value v1 v2 v3 v4 v5 v6)
)

let map7 sub_value (Decoder d1) (Decoder d2) (Decoder d3) (Decoder d4) (Decoder d5) (Decoder d6) (Decoder d7) = Decoder (fun (json, value) -> 
    let v1 = d1 (json, identity) in
    let v2 = d2 (json, identity) in
    let v3 = d3 (json, identity) in
    let v4 = d4 (json, identity) in
    let v5 = d5 (json, identity) in
    let v6 = d6 (json, identity) in
    let v7 = d7 (json, identity) in
    value (sub_value v1 v2 v3 v4 v5 v6 v7)
)

let map8 sub_value (Decoder d1) (Decoder d2) (Decoder d3) (Decoder d4) (Decoder d5) (Decoder d6) (Decoder d7) (Decoder d8) = Decoder (fun (json, value) -> 
    let v1 = d1 (json, identity) in
    let v2 = d2 (json, identity) in
    let v3 = d3 (json, identity) in
    let v4 = d4 (json, identity) in
    let v5 = d5 (json, identity) in
    let v6 = d6 (json, identity) in
    let v7 = d7 (json, identity) in
    let v8 = d8 (json, identity) in
    value (sub_value v1 v2 v3 v4 v5 v6 v7 v8)
)

(* Data Structure Decoders *)

let nullable decoder = Decoder (fun state -> 
    let (Decoder null_decode) = null None in
    let (Decoder value_decode) = map (fun x -> Some x) decoder in
    try null_decode state
    with _ -> value_decode state
)

let list (Decoder decode) = Decoder (fun (json, value) ->
    match Js.Json.classify json with
        | JSONArray jsons -> 
            jsons
                |> Array.to_list
                |> List.map (fun json' -> decode(json', identity))
                |> value 
        | _ -> failwith ( Printf.sprintf "failed to decode json as a list: %s" ( Js.Json.stringify json ) )
)

let array (Decoder decode) = Decoder (fun (json, value) ->
    match Js.Json.classify json with
        | JSONArray jsons -> 
            jsons
                |> Array.map (fun json' -> decode(json', identity))
                |> value
        | _ -> failwith ( Printf.sprintf "failed to decode json as an array: %s" ( Js.Json.stringify json ) )
)

let dict (Decoder decode) = Decoder (fun (json, value) ->
    match Js.Json.classify json with
        | JSONObject json_dict -> 
            json_dict
                |> Js.Dict.entries
                |> Array.fold_left 
                    (fun m (k, json') -> Dict.add k (decode(json', identity)) m) 
                    Dict.empty
                |> value
        | _ -> failwith ( Printf.sprintf "failed to decode json as a dict: %s" ( Js.Json.stringify json ) )
)

let key_value_pairs (Decoder decode) = Decoder (fun (json, value) ->
    match Js.Json.classify json with
        | JSONObject json_dict -> 
            json_dict
                |> Js.Dict.entries
                |> Array.to_list
                |> List.map (fun (k,json') -> (k,(decode(json', identity))))
                |> value
        | _ -> failwith ( Printf.sprintf "failed to decode json as key value pairs: %s" ( Js.Json.stringify json ) )
)

(* Object Primitives *)

let field k (Decoder decode) = Decoder (fun (json, value) ->
    let fail () = failwith ( Printf.sprintf "failed to decode json field '%s': %s" k ( Js.Json.stringify json ) ) in
    match Js.Json.classify json with
        | JSONObject json_dict -> 
            (match Js.Dict.get json_dict k with
                | Some json' -> value(decode(json', identity))
                | None -> fail ()
            )
        | _ -> fail ()
)

let at fields decoder = 
    List.fold_right field fields decoder

let index idx (Decoder decode) = Decoder (fun (json, value) ->
    let fail () = failwith ( Printf.sprintf "failed to decode json at index '%d': %s" idx ( Js.Json.stringify json ) ) in
    match Js.Json.classify json with
        | JSONArray jsons -> 
            let json' = 
                try Array.get jsons idx
                with _ -> fail ()
            in
                value(decode(json', identity))

        | _ -> fail ()
)

(* Fancy Decoding *)

let succeed sub_value = Decoder (fun (_json, value) ->
    value sub_value
)

let fail msg = Decoder (fun (_json, _value) ->
    failwith msg
)

let and_then to_d2 (Decoder d1) = Decoder (fun (json, value) ->
    let v1 = d1 (json, identity) in
    let (Decoder d2) = to_d2 v1 in
    d2 (json, value)
)

let and_map d1 d2 = 
    and_then (fun f -> map f d1) d2

let (|:) = and_map