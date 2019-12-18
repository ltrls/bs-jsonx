type value =
    Value of Js.Json.t

let encode _space v =
    let (Value json) = v in
    Js.Json.stringify json 

let string v = 
    Value (Js.Json.string v)

let int v =
    Value (Js.Json.number (float_of_int v))

let float v =
    Value (Js.Json.number v)

let bool v =
    Value (Js.Json.boolean v)

let null =
    Value Js.Json.null

let array v =
    let json_t_ar = Array.map (fun (Value t) -> t) v in
    let json_t = Js.Json.array json_t_ar in
    Value json_t

let list v =
    array (Array.of_list v)

let object' v =
    let deconstruct_pair (k, Value v) = (k,v) in
    let to_value x = Value x in
    v
        |> List.map deconstruct_pair
        |> Js.Dict.fromList
        |> Js.Json.object_
        |> to_value
