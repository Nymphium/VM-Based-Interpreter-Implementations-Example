type t =
  | Bool of bool
  | Unit
  | Int of int
  | Fun of string * t
  | LetRecFun of string * string * t * t
  | Var of string
  | App of t * t
  | Let of string * t * t
  | If of t * t * t
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Div of t * t
  | Not of t
  | Eq of t * t
  | Lt of t * t (* t < t *)
  | Ge of t * t (* t >= t *)
[@@deriving show { with_path = false }]

