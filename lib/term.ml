type t =
  | Int of int
  | App of t * t
  | Fun of string * t
  | Let of string * t * t
  | Var of string
  | Add of t * t
  | Sub of t * t
[@@deriving show { with_path = false }]

