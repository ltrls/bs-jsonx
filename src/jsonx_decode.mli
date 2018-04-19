module Dict : Map.S with type key = string

type ('a, 'b) decoder

val decode_string : ('a -> 'a, 'b) decoder -> string -> ('b, string) Js.Result.t

(* Primitive Decoders *)

val string : (string -> 'a, 'a) decoder

val bool : (bool -> 'a, 'a) decoder

val int : (int -> 'a, 'a) decoder

val float : (float -> 'a, 'a) decoder

val null : 'a -> ('a -> 'a, 'a) decoder

(* Data Structure Decoders *)

val nullable : ('a -> 'a, 'b) decoder -> ('b option -> 'c, 'c) decoder

val list : ('a -> 'a, 'b) decoder -> ('b list -> 'c, 'c) decoder

val array : ('a -> 'a, 'b) decoder -> ('b array -> 'c, 'c) decoder

val dict : ('a -> 'a, 'b) decoder -> ('b Dict.t -> 'c, 'c) decoder

val key_value_pairs : ('a -> 'a, 'b) decoder -> ((string * 'b) list -> 'c, 'c) decoder

(* Object Primitives *)

val field : string -> ('a -> 'a, 'b) decoder -> ('b -> 'c, 'c) decoder

val at : string list -> ('a -> 'a, 'a) decoder -> ('a -> 'a, 'a) decoder

val index : int -> ('a -> 'a, 'a) decoder -> ('a -> 'a, 'a) decoder

(* Inconsistent Structure *)

val optional : ('a -> 'b, 'b) decoder -> ('a option -> 'b, 'b) decoder

val one_of :  (('a -> 'a, 'a) decoder) list -> ('a -> 'a, 'a) decoder

(* Mapping *)

val map : ('a -> 'b) -> ('c -> 'c, 'a) decoder -> ('b -> 'd, 'd) decoder

val map2 :
    ('a -> 'b -> 'c) ->
    ('d -> 'd, 'a) decoder ->
    ('e -> 'e, 'b) decoder -> ('c -> 'f, 'f) decoder

val map3 :
    ('a -> 'b -> 'c -> 'd) ->
    ('e -> 'e, 'a) decoder ->
    ('f -> 'f, 'b) decoder ->
    ('g -> 'g, 'c) decoder -> ('d -> 'h, 'h) decoder

val map4 :
    ('a -> 'b -> 'c -> 'd -> 'e) ->
    ('f -> 'f, 'a) decoder ->
    ('g -> 'g, 'b) decoder ->
    ('h -> 'h, 'c) decoder ->
    ('i -> 'i, 'd) decoder -> ('e -> 'j, 'j) decoder

val map5 :
    ('a -> 'b -> 'c -> 'd -> 'e -> 'f) ->
    ('g -> 'g, 'a) decoder ->
    ('h -> 'h, 'b) decoder ->
    ('i -> 'i, 'c) decoder ->
    ('j -> 'j, 'd) decoder ->
    ('k -> 'k, 'e) decoder -> ('f -> 'l, 'l) decoder

val map6 :
    ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
    ('h -> 'h, 'a) decoder ->
    ('i -> 'i, 'b) decoder ->
    ('j -> 'j, 'c) decoder ->
    ('k -> 'k, 'd) decoder ->
    ('l -> 'l, 'e) decoder ->
    ('m -> 'm, 'f) decoder -> ('g -> 'n, 'n) decoder

val map7 :
    ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h) ->
    ('i -> 'i, 'a) decoder ->
    ('j -> 'j, 'b) decoder ->
    ('k -> 'k, 'c) decoder ->
    ('l -> 'l, 'd) decoder ->
    ('m -> 'm, 'e) decoder ->
    ('n -> 'n, 'f) decoder ->
    ('o -> 'o, 'g) decoder -> ('h -> 'p, 'p) decoder

val map8 :
    ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i) ->
    ('j -> 'j, 'a) decoder ->
    ('k -> 'k, 'b) decoder ->
    ('l -> 'l, 'c) decoder ->
    ('m -> 'm, 'd) decoder ->
    ('n -> 'n, 'e) decoder ->
    ('o -> 'o, 'f) decoder ->
    ('p -> 'p, 'g) decoder ->
    ('q -> 'q, 'h) decoder -> ('i -> 'r, 'r) decoder

(* Fancy Decoding *)

val succeed : 'a -> ('a -> 'b, 'b) decoder

val fail : string -> ('a, 'b) decoder

val and_then : 
    ('a -> ('b, 'c) decoder) -> 
    ('d -> 'd, 'a) decoder -> 
    ('b, 'c) decoder

val and_map : 
    ('a -> 'a, 'b) decoder -> 
    ('c -> 'c, 'b -> 'd) decoder -> 
    ('d -> 'e, 'e) decoder

val (|:) : 
    ('a -> 'a, 'b) decoder -> 
    ('c -> 'c, 'b -> 'd) decoder -> 
    ('d -> 'e, 'e) decoder