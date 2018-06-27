type i = (* 'r is type of register *)
  | Add of int * int * int (* Add(a, b, c) = Reg[a] = Reg[b] + Reg[c] *)
  | Sub of int * int * int
  | Load of int * int (* const load; Load(a, b) = Reg[a] = Const[b] *)
  | Clos of int * int (* closure load; Clos(a, b) = Reg[a] = Clos[b] *)
  | Call of int * int (* Call(a, b) = Reg[a] = Reg[a](b)  *)
  | Move of int * int (* Move(a, b) = Reg[a] = b *)
  | Upval of int * int (* Upval(a, b) = Reg[a] = Upval[b] *)
  | Return of int (* Return(i) = exit with Reg[i] *)
(* VM values *)
and regv =
  | Null (* initialized value *)
  | Vint of int
  | Vclos of cl * regv list
and reg = regv ref array
and rawval =
  | RInt of int
  (* values *)
and cs = rawval list
(* closure *)
and cl = C of i list * cs * cl list * int list
[@@deriving show { with_path = false }]

let __REG_SIZE__ = 32

let new_reg () = Array.init __REG_SIZE__ (fun _ -> ref Null)

let (@+) (Vint i) (Vint j) = Vint(i + j)
let (@-) (Vint i) (Vint j) = Vint(i - j)

let run c =
  let rec work (C(is, cs, cls, fvs)) upvs (rv : reg) =
    match is with
    | [] -> raise @@ Failure "exit without RETURN"
    | Return(i) :: _ -> !(rv.(i))
    | Load(ra, ci) :: is' ->
      let v = match List.nth cs ci with
        | RInt i -> Vint i
      in
      let () = rv.(ra) := v in
      work (C(is', cs, cls, fvs)) upvs rv
    | Add(a, b, c) :: is' ->
      let () = rv.(a) := !(rv.(b)) @+ !(rv.(c)) in
      work (C(is', cs, cls, fvs)) upvs rv
    | Sub(a, b, c) :: is' ->
      let () = rv.(a) := !(rv.(b)) @- !(rv.(c)) in
      work (C(is', cs, cls, fvs)) upvs rv
    | Clos(a, b) :: is' ->
      let C(isc, csc, clsc, fvsc) as c' = List.nth cls b in
      let upvs' = fvsc |> List.map (fun ri -> !(rv.(ri))) in
      let () = rv.(a) := Vclos(c', upvs') in
      work (C(is', cs, cls, fvs)) upvs rv
    | Move(a, b) :: is' ->
      let () = rv.(a) := !(rv.(b)) in
      work (C(is', cs, cls, fvs)) upvs rv
    | Upval(a, b) :: is' ->
      let () = rv.(a) := List.nth upvs b in
      work (C(is', cs, cls, fvs)) upvs rv
    | Call(a, b) :: is' ->
      let Vclos(cl', upvs') = !(rv.(a)) in
      let rv' = new_reg () in
      let () = rv'.(0) := !(rv.(b)) in
      let () = rv.(a) := work cl' (upvs' @ upvs) rv' in
      work (C(is', cs, cls, fvs)) upvs rv
  in
  work c [] @@ new_reg ()

