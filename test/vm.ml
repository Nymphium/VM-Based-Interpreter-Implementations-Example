open Hvm

let test t =
  print_endline @@ Term.show t;
  print_endline "->";
  let c = Compile.compile t in
  print_endline @@ Vm.show_cl c;
  print_endline "~~>";
  let result = Vm.run c in
  print_endline @@ Vm.show_regv result

let () =
  (** (fun x _ -> x) 3 5 *)
  let t1 = Term.(App(App(Fun("x", Fun("y", Var "x")), Int 3), Int 5)) in
  test t1;

  (** let x = 3 in (fun y -> x) 0 *)
  let t2 = Term.( Let("_", Int 0, Let("j", Int 10, Let("x", Int 3, App(Fun("y", Var "x"), Int 0))))) in
  test t2;

  (** let f = fun x -> x + 3 in
   ** let g = fun y -> y + 5 in
   ** f (g 1) *)
  let t3 = Term.(Let("f", Fun("x", Add(Var"x", Int 3)), Let("g", Fun("y", Add(Var"y", Int 5)), App(Var"f", App(Var"g", Int 1))))) in
  test t3;

