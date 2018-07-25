open Irepr
open VMUtils

exception VReturn of Irepr.regv

type ep = regv ref array -> regv list -> regv

let cfhash : (cl, ep) Hashtbl.t = Hashtbl.create 100;;
let get c = Hashtbl.find cfhash c

(* Printf.printf "[%d] %s\n" pc (show_i is0); *)


let runwrap ep rv upv =
  try
    let _ = ep rv upv
    in Null (* dummy value *)
  with VReturn v -> v (* actual return value *)

let morecomp0 is cs cls =
  let product is0 pc ial =
    match is0 with
    | Return(i) ->
      fun[@inline] rv upvs ->
        raise_notrace @@ VReturn !(rv.(i))
    | Jump(i) ->
      fun[@inline] rv upvs ->
        ial.(pc + i + 1) rv upvs
    | Test(a, p) ->
      fun[@inline] rv upvs ->
        let b = ?@ !(rv.(a)) in
        if p > 0 && b || p <= 0 && not b then
          ial.(pc + 2) rv upvs
        else
          ial.(pc + 1) rv upvs
    | Load(ra, ci) ->
      let v = match List.nth cs ci with
        | RInt i -> Vint i
      in
      fun[@inline] rv upvs ->
        let () = rv.(ra) := v in
        ial.(pc + 1) rv upvs
    | Move(a, b) ->
      fun[@inline] rv upvs ->
        let () = rv.(a) := !(rv.(b)) in
        ial.(pc + 1) rv upvs
    | Unit(a) ->
      fun[@inline] rv upvs ->
        let () = rv.(a) := Vunit in
        ial.(pc + 1) rv upvs
    | SetBool(a, x, p) ->
      fun[@inline] rv upvs ->
        let () = rv.(a) := Vbool(x > 0) in
        if p = 1 then
          ial.(pc + 2) rv upvs
        else
          ial.(pc + 1) rv upvs
    | Add(a, b, c)  ->
      fun[@inline] rv upvs ->
        let () = rv.(a) := !(rv.(b)) +@ !(rv.(c)) in
        ial.(pc + 1) rv upvs
    | Sub(a, b, c)  ->
      fun[@inline]  rv upvs ->
        let () = rv.(a) := !(rv.(b)) -@ !(rv.(c)) in
        ial.(pc + 1) rv upvs
    | Mul(a, b, c)  ->
      fun[@inline] rv upvs ->
        let () = rv.(a) := !(rv.(b)) *@ !(rv.(c)) in
        ial.(pc + 1) rv upvs
    | Div(a, b, c)  ->
      fun[@inline] rv upvs ->
        let () = rv.(a) := !(rv.(b)) /@ !(rv.(c)) in
        ial.(pc + 1) rv upvs
    | Lt(a, b)  ->
      fun[@inline] rv upvs ->
        if ?@ (!(rv.(a)) <@ !(rv.(b))) then
          ial.(pc + 2) rv upvs
        else
          ial.(pc + 1) rv upvs
    | Eq(a, b)  ->
      fun[@inline] rv upvs ->
        if !(rv.(a)) = !(rv.(b)) then
          ial.(pc + 2) rv upvs
        else
          ial.(pc + 1) rv upvs
    | Ge(a, b)  ->
      fun[@inline] rv upvs ->
        if ?@ (!(rv.(a)) >=@ !(rv.(b))) then
          ial.(pc + 2) rv upvs
        else
          ial.(pc + 1) rv upvs
    | Clos(a, b, p)  ->
      let C(_, _, _, fvsc) as c' = List.nth cls b in
      fun[@inline] rv upvs ->
        let upvs' = fvsc |> List.map (fun[@inline] ri -> !(rv.(ri))) in
        let upr = ref upvs' in
        let cl = Vclos(c', upr) in
        let () = rv.(a) := cl in
        let () = if p = 1 then upr := cl :: !upr in
        ial.(pc + 1) rv upvs
    | Upval(a, b)  ->
      fun[@inline] rv upvs ->
        let () = rv.(a) := List.nth upvs b in
        ial.(pc + 1) rv upvs
    | Call(a, b)  ->
      fun[@inline] rv upvs ->
        let isf, csf, clsf, fvsf, upr = unwrap_vclos rv a in
        let rv' = new_reg () in
        let ep = get (C(isf, csf, clsf, fvsf)) in
        let () = rv'.(0) := !(rv.(b)) in
        let () = rv.(a) := runwrap ep rv' (!upr @ upvs) in
        ial.(pc + 1) rv upvs
    | TailCall(a, b)  ->
      let rv' = new_reg () in
      fun[@inline] rv upvs ->
        let isf, csf, clsf, fvsf, upr = unwrap_vclos rv a in
        let () = rv'.(0) := !(rv.(b)) in
        get (C(isf, csf, clsf, fvsf)) rv' (!upr @ upvs)
  in
  let ial : ep array = Array.init (List.length is) @@ fun[@inline] pc _ _ ->
    failwith @@ Printf.sprintf "invalid access %d" pc
  in
  let _ =
    is |> List.iteri @@ fun pc is0 ->
    ial.(pc) <- product is0 pc ial
  in
  ial.(0)

let morecompile (C(is, cs, cls, fvs)) =
  let rec work cls =
    cls |> List.iter @@ fun (C(is, cs, cls', _) as c) ->
    let ep = morecomp0 is cs cls' in
    Hashtbl.add cfhash c ep;
    work cls'
  in
  let () = work cls in
  morecomp0 is cs cls

let run c =
  let ep = morecompile c in
  let rv = new_reg () in
  runwrap ep rv []

