open Hvm

let test t =
  print_endline @@ Term.show t;
  print_endline "->";
  let c = Compile.compile t in
  print_endline @@ Irepr.show_cl c;
  print_endline "~~>";
  let result = Vm1.run c in
  print_endline @@ Irepr.show_regv result

let () =
  let t1 = Term.(Let("x", Int 4, Let("y", Int 8, If(Lt(Var"x", Var"y"), Add(Var"x", Var"y"), Sub(Var"x", Var"y"))))) in
  test t1;

  (** (fun x _ -> x) 3 5 *)
  let t2 = Term.(App(App(Fun("x", Fun("y", Var "x")), Int 3), Int 5)) in
  test t2;

  (** let x = 3 in (fun y -> x) 0 *)
  let t3 = Term.( Let("_", Int 0, Let("j", Int 10, Let("x", Int 3, App(Fun("y", Var "x"), Int 0))))) in
  test t3;

  (** let f = fun x -> x + 3 in
   ** let g = fun y -> y + 5 in
   ** f (g 1) *)
  let t4 = Term.(Let("f", Fun("x", Add(Var"x", Int 3)), Let("g", Fun("y", Add(Var"y", Int 5)), App(Var"f", App(Var"g", Int 1))))) in
  test t4;

  (** let rec f x = if x = 0 then 0 else f (x - 1) in f 10 **)
  let t5 = Term.(LetRecFun("f", "x", If(Eq(Var"x", Int 0), Int 0, App(Var"f", Sub(Var"x", Int 1))), App(Var"f", Int 10))) in
  test t5;

