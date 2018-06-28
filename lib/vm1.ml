open Irepr

let __REG_SIZE__ = 32

let new_reg () = Array.init __REG_SIZE__ (fun _ -> ref Null)

let (+@) (Vint i) (Vint j) = Vint(i + j)
let (-@) (Vint i) (Vint j) = Vint(i - j)
let ( *@) (Vint i) (Vint j) = Vint(i * j)
let (/@) (Vint i) (Vint j) = Vint(i / j)
let (<@) (Vint i) (Vint j) = Vbool(i < j)
let (>=@) (Vint i) (Vint j) = Vbool(i >= j)

let (?@) (Vbool b) = b

let run (C(is, cs, cls, fvs)) =
  let rec work is cs cls fvs upvs (rv : reg) =
    match is with
    | [] -> raise @@ Failure "exit without RETURN"
    | Return(i) :: _ -> !(rv.(i))
    | Jump(i) :: is' -> work (Util.drop i is) cs cls fvs upvs rv
    | Test(a, p) :: is' ->
      let b = ?@ !(rv.(a)) in
      if p > 0 && b || p <= 0 && not b then
        work (List.tl is') cs cls fvs upvs rv
      else
        work is' cs cls fvs upvs rv
    | Load(ra, ci) :: is' ->
      let v = match List.nth cs ci with
        | RInt i -> Vint i
      in
      let () = rv.(ra) := v in
      work is' cs cls fvs upvs rv
    | Unit(a) :: is' ->
      let () = rv.(a) := Vunit in
      work is' cs cls fvs upvs rv
    | SetBool(a, p) :: is' ->
      let () = rv.(a) := Vbool(p > 0) in
      work is' cs cls fvs upvs rv
    | Add(a, b, c) :: is' ->
      let () = rv.(a) := !(rv.(b)) +@ !(rv.(c)) in
      work is' cs cls fvs upvs rv
    | Sub(a, b, c) :: is' ->
      let () = rv.(a) := !(rv.(b)) -@ !(rv.(c)) in
      work is' cs cls fvs upvs rv
    | Mul(a, b, c) :: is' ->
      let () = rv.(a) := !(rv.(b)) *@ !(rv.(c)) in
      work is' cs cls fvs upvs rv
    | Div(a, b, c) :: is' ->
      let () = rv.(a) := !(rv.(b)) /@ !(rv.(c)) in
      work is' cs cls fvs upvs rv
    | Lt(a, b) :: is' ->
      if ?@ (!(rv.(a)) <@ !(rv.(b))) then
        work (List.tl is') cs cls fvs upvs rv
      else
        work is' cs cls fvs upvs rv
    | Eq(a, b) :: is' ->
      if !(rv.(a)) = !(rv.(b)) then
        work (List.tl is') cs cls fvs upvs rv
      else
        work is' cs cls fvs upvs rv
    | Ge(a, b) :: is' ->
      if ?@ (!(rv.(a)) >=@ !(rv.(b))) then
        work (List.tl is') cs cls fvs upvs rv
      else
        work is' cs cls fvs upvs rv
    | Clos(a, b) :: is' ->
      let C(isc, csc, clsc, fvsc) as c' = List.nth cls b in
      let upvs' = fvsc |> List.map (fun ri -> !(rv.(ri))) in
      let () = rv.(a) := Vclos(c', upvs') in
      work is' cs cls fvs upvs rv
    | Move(a, b) :: is' ->
      let () = rv.(a) := !(rv.(b)) in
      work is' cs cls fvs upvs rv
    | Upval(a, b) :: is' ->
      let () = rv.(a) := List.nth upvs b in
      work is' cs cls fvs upvs rv
    | Call(a, b) :: is' ->
      let Vclos(C(isf, csf, clsf, fvsf), upvs') = !(rv.(a)) in
      let rv' = new_reg () in
      let () = rv'.(0) := !(rv.(b)) in
      let () = rv.(a) := work isf csf clsf fvsf (upvs' @ upvs) rv' in
      work is' cs cls fvs upvs rv
  in
  work is cs cls fvs [] @@ new_reg ()

