module T = Term

module SS = Set.Make(struct
    type t = string
    let compare = (compare: t -> t -> int)
  end)

let rec get_fv = T.(function
    | Add(e1, e2) | Sub(e1, e2) | App(e1, e2) -> SS.union (get_fv e1) (get_fv e2)
    | Int i -> SS.empty
    | Var x -> SS.singleton x
    | Fun(x, e) -> SS.remove x @@ get_fv e
    | Let(x, e, body) -> SS.(union (get_fv e) @@ remove x @@ get_fv body) 
  )

let var_to_reg xs (env: (string * int) list) = xs |> List.map @@ fun x -> List.assoc x env

let squash ls =
  let rec work acc = function
    | [] -> acc
    | ("_", _) :: kvs -> work acc kvs
    | ((x, _) as kv) :: kvs ->
      if List.exists (fun (y, _) -> x = y) acc then
        work acc kvs
      else
        work (kv :: acc) kvs
  in work [] ls

let compile term =
  let rec work term argnum fvs env =
    let create_ref () =
      let r = ref (-1) in
      r, fun () -> incr r; !r
    in
    let regref, regi = create_ref () in
    ignore @@ regi ();
    let () = for i = 0 to argnum - 2 do ignore @@ regi () done in
    let constref, consti = create_ref () in
    let closref, closi = create_ref () in
    let rec inner term is cs cls fvs env =
      match term with
      | T.Int i ->
        (* Int v --> is*; Load(ra, i); Return(ra), Consts[i = v] *)
        let reg = regi () in
        let is' = Vm.(Return(reg) :: Load(reg, consti()) :: is) in
        let cs' = Vm.RInt(i) :: cs in
        is', cs', cls, fvs, env
      | T.Var x ->
        (* Var x --> is*; (if Env[x] > 0 then Move(ra, Env[x]) else Upval(ra, -Env[x] - 1)); Return(reg) *)
        let idx = List.assoc x env in
        let reg = regi () in
        let ist =
          if idx >= 0 then
            Vm.Move(reg, idx)
          else
            Vm.Upval(reg, (-idx - 1))
        in
        Vm.Return(reg) :: ist :: is, cs, cls, fvs, env
      | T.Let(x, e, body) ->
        (* Let(x, e, body) -->
         * [[e]] ~ Is = is*; Return(rx), Env
         * [[body]] with Is; Move(ra, rx), Env[x = ra]
         * *)
        let Vm.Return(latest_reg) :: is', cs', cls', fvs', env' = inner e is cs cls fvs env in
        let env'' = (x, latest_reg) :: env' in
        inner body (Vm.Move(regi(), latest_reg) :: is') cs' cls' fvs' env'' 
      | T.Add(e1, e2) ->
        (* Add(e1, e2) -->
         * [[e1]] ~ Is = Is1; Return(re1), Env1
         * [[e2]] ~ Is = Is2; Return(re2), Env2 with Is = Is1, Env = Env1
         * Is2; Add(ra, re1, re2); Return(ra), Env2
        *)
        let Vm.Return(re1) :: is', cs', cls', fvs', env' = inner e1 is cs cls fvs env in
        let Vm.Return(re2) :: is'', cs'', cls'', fvs'', env'' = inner e2 is' cs' cls' fvs' env' in
        let reg = regi () in
        Vm.(Return(reg) :: Add(reg, re1, re2) :: is''), cs'', cls'', fvs'', env''
      | T.Sub(e1, e2) ->
        (* Sub(e1, e2) -->
         * [[e1]] ~ Is = Is1; Return(re1), Env1
         * [[e2]] ~ Is = Is2; Return(re2), Env2 with Is = Is1, Env = Env1
         * Is2; Sub(ra, re1, re2); Return(ra), Env2
        *)
        let Vm.Return(re1) :: is', cs', cls', fvs', env' = inner e1 is cs cls fvs env in
        let Vm.Return(re2) :: is'', cs'', cls'', fvs'', env'' = inner e2 is' cs' cls' fvs' env' in
        let reg = regi () in
        Vm.(Return(reg) :: Sub(reg, re1, re2) :: is''), cs'', cls'', fvs'', env''
      | T.Fun(x, e) ->
        let reg = regi () in
        let fv = get_fv term in
        (* create new environment with NEEDED var-reg pairs from the old *)
        let env0 = env |> List.filter @@ fun (x, _) -> SS.mem x fv in
        (* upvalues are indexed by -(i + 1) *)
        let env' = (squash env0 |> List.mapi (fun i (x, _) -> (x, -(i + 1)))) in
        (* fvs is map from Upval's index to current register:
         * create closure on the Vm with upvalues; Upval(_, i) refers fvs[i] -> CURRENT Reg[fvs[i]]
        *)
        let fvs' = var_to_reg (SS.fold (fun x z -> x :: z) fv []) env0 in
        let Vm.C(isf, csf, clsf, _) = work e 1 fvs' ((x, 0) :: env') in
        (* append fvs map; Upvalue store is growing *)
        let cl' = Vm.C(isf, csf, clsf, fvs @ fvs') in
        Vm.(Return(reg) :: Clos(reg, closi ()) :: is), cs, cl' :: cls, fvs, env
      | T.App(e1, e2) ->
        (* App(e1, e2) -->
         * [[e1]] ~ Is = Is1; Return(re1), Env1
         * [[e2]] ~ Is = Is2; Return(re2), Env2 with Is = Is1, Env = Env1
         * Is2; Return(re1, re2); Return(re1), Env2
        *)
        let Vm.Return(re1) :: is', cs', cls', fvs', env' = inner e1 is cs cls fvs env in
        let Vm.Return(re2) :: is'', cs'', cls'', fvs'', env'' = inner e2 is' cs' cls' fvs' env' in
        Vm.(Return(re1) :: Call(re1, re2) :: is''), cs'', cls'', fvs'', env''
    in
    let is, cs, cls, fvs, env' = inner term [] [] [] [] env in
    List.(Vm.C(rev is, rev cs, rev cls, rev fvs))
  in work term 1 [] []

