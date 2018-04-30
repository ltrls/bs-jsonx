module Dict = Map.Make(String)

type 'a decoder = Js.Json.t -> 'a

(* helpers *)

let is_int v =
  let c = classify_float (fst (modf v)) in
  c == FP_zero

(* Run Decoder *)

let decode_string decode json_str =
    try 
        let json = Js.Json.parseExn json_str in
        try
            Js.Result.Ok (decode json)
        with
            Failure msg -> Js.Result.Error msg
    with
      _ -> Js.Result.Error ( Printf.sprintf "failed to decode json string: %s" json_str )
      
(* Primitive Decoders *)

let string json = 
    match Js.Json.classify json with
        | JSONString str -> str
        | _ -> failwith ( Printf.sprintf "failed to decode json as string: %s" ( Js.Json.stringify json ) )

let bool json =
    match Js.Json.classify json with
        | JSONTrue -> true
        | JSONFalse -> false
        | _ -> failwith ( Printf.sprintf "failed to decode json as bool: %s" ( Js.Json.stringify json ) )

let int json = 
    let fail () = failwith ( Printf.sprintf "failed to decode json as int: %s" ( Js.Json.stringify json ) ) in
    match Js.Json.classify json with
        | JSONNumber f -> 
            if is_int f 
            then int_of_float f
            else fail ()
        | _ -> fail ()

let float json = 
    match Js.Json.classify json with
        | JSONNumber f -> f
        | _ -> failwith ( Printf.sprintf "failed to decode json as float: %s" ( Js.Json.stringify json ) ) 

let null value json = 
    match Js.Json.classify json with
        | JSONNull -> value
        | _ -> failwith ( Printf.sprintf "failed to decode json as null: %s" ( Js.Json.stringify json ) )


(* Inconsistent Structure *)

let optional decoder json =
    try Some (decoder json)
    with _ -> None

let one_of decoders json =
    let rec first ds = 
        match ds with
            | decoder :: xs -> (try decoder json with _ -> first xs)
            | [] -> failwith ( Printf.sprintf "failed to decode json using any one of the provided decoders: %s" ( Js.Json.stringify json ) )
    in 
        first decoders

(* Mapping *)

let map mapper decoder json =
    mapper (decoder json)

let map2 mapper d1 d2 json =
    let v1 = d1 json in
    let v2 = d2 json in
    mapper v1 v2

let map3 mapper d1 d2 d3 json =
    let v1 = d1 json in
    let v2 = d2 json in
    let v3 = d3 json in
    mapper v1 v2 v3

let map4 mapper d1 d2 d3 d4 json =
    let v1 = d1 json in
    let v2 = d2 json in
    let v3 = d3 json in
    let v4 = d4 json in
    mapper v1 v2 v3 v4

let map5 mapper d1 d2 d3 d4 d5 json =
    let v1 = d1 json in
    let v2 = d2 json in
    let v3 = d3 json in
    let v4 = d4 json in
    let v5 = d5 json in
    mapper v1 v2 v3 v4 v5

let map6 mapper d1 d2 d3 d4 d5 d6 json =
    let v1 = d1 json in
    let v2 = d2 json in
    let v3 = d3 json in
    let v4 = d4 json in
    let v5 = d5 json in
    let v6 = d6 json in
    mapper v1 v2 v3 v4 v5 v6

let map7 mapper d1 d2 d3 d4 d5 d6 d7 json =
    let v1 = d1 json in
    let v2 = d2 json in
    let v3 = d3 json in
    let v4 = d4 json in
    let v5 = d5 json in
    let v6 = d6 json in
    let v7 = d7 json in
    mapper v1 v2 v3 v4 v5 v6 v7

let map8 mapper d1 d2 d3 d4 d5 d6 d7 d8 json =
    let v1 = d1 json in
    let v2 = d2 json in
    let v3 = d3 json in
    let v4 = d4 json in
    let v5 = d5 json in
    let v6 = d6 json in
    let v7 = d7 json in
    let v8 = d8 json in
    mapper v1 v2 v3 v4 v5 v6 v7 v8

(* Data Structure Decoders *)

let nullable decoder state =
    try null None state
    with _ -> (map (fun x -> Some x) decoder) state

let list decoder json =
    match Js.Json.classify json with
        | JSONArray jsons -> 
            jsons
                |> Array.to_list
                |> List.map decoder
        | _ -> failwith ( Printf.sprintf "failed to decode json as a list: %s" ( Js.Json.stringify json ) )

let array decoder json =
    match Js.Json.classify json with
        | JSONArray jsons -> Array.map decoder jsons
        | _ -> failwith ( Printf.sprintf "failed to decode json as an array: %s" ( Js.Json.stringify json ) )

let dict decoder json =
    match Js.Json.classify json with
        | JSONObject json_dict -> 
            json_dict
                |> Js.Dict.entries
                |> Array.fold_left 
                    (fun m (k, json') -> Dict.add k (decoder json') m) 
                    Dict.empty
        | _ -> failwith ( Printf.sprintf "failed to decode json as a dict: %s" ( Js.Json.stringify json ) )

let key_value_pairs decoder json =
    match Js.Json.classify json with
        | JSONObject json_dict -> 
            json_dict
                |> Js.Dict.entries
                |> Array.to_list
                |> List.map (fun (k, json') -> (k, decoder json'))
        | _ -> failwith ( Printf.sprintf "failed to decode json as key value pairs: %s" ( Js.Json.stringify json ) )

(* Object Primitives *)

let field k decoder json =
    let fail () = failwith ( Printf.sprintf "failed to decode json field '%s': %s" k ( Js.Json.stringify json ) ) in
    match Js.Json.classify json with
        | JSONObject json_dict -> 
            (match Js.Dict.get json_dict k with
                | Some json' -> decoder json'
                | None -> fail ()
            )
        | _ -> fail ()

let at fields decoder = 
    List.fold_right field fields decoder

let index idx decoder json =
    let fail () = failwith ( Printf.sprintf "failed to decode json at index '%d': %s" idx ( Js.Json.stringify json ) ) in
    match Js.Json.classify json with
        | JSONArray jsons -> 
            let json' = 
                try Array.get jsons idx
                with _ -> fail ()
            in
                decoder json'

        | _ -> fail ()

(* Fancy Decoding *)

let succeed value _json =
    value

let fail msg _json =
    failwith msg

let and_then to_decoder decoder json =
    let value = decoder json in
    let decoder2 = to_decoder value in
    decoder2 json

let and_map d1 d2 = 
    and_then (fun f -> map f d1) d2

let (|:) = and_map