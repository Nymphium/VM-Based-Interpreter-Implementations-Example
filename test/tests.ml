open Hvm

module O(Vm : VM) : sig
  val bench : ?sample:int -> ?batch:int -> unit -> unit
end = struct
  let test t =
    (* print_endline @@ Term.show t; *)
    (* print_endline "->"; *)
    let c = Compile.compile t in
    (* print_endline @@ Irepr.show_cl c; *)
    (* print_endline "~~>"; *)
    let result = Vm.run c in
    ignore result
  (* print_endline @@ Irepr.show_regv result *)

  let bench ?(sample = 10) ?(batch = 10) () =
    let t1 = Term.DSL.(
        (let_ "x" ~&4 @@
         let_ "y" ~&8 @@
         if_ (lt ~?"x" ~?"y")
           (add ~?"x" ~?"y")
           (sub ~?"x" ~?"y")))
    in
    let t2 = Term.DSL.
               ((@) (fun_ "x" @@ fun_ "y" ~?"x") ~&3 @ ~& 5)
    in
    let t3 = Term.DSL.
               (let_ "_" ~&0 @@
                let_ "j" ~&10 @@
                let_ "x" ~&3 @@
                (fun_ "y" ~?"x") @ ~&0)
    in
    let t4 = Term.DSL.
               (let_ "f" (fun_ "x" (add ~?"x" ~&3)) @@
                let_ "g" (fun_ "y" (add ~?"y" ~&5)) @@
                ~?"f" @ (~?"g" @ ~&1))
    in
    let t5 = Term.DSL.
               (letrec "f" "x"
                  (if_ (eq ~?"x" ~&0) ~&0
                     (~?"f" @ (sub ~?"x" ~&1))) @@
                ~?"f" @ ~&10)
    in
    let t6 = Term.DSL.
               (letrec "fib" "x"
                  (if_ (lt ~?"x" ~&2)
                     ~?"x"
                     (add (~?"fib" @ (sub ~?"x" ~&1)) (~?"fib" @ (sub ~?"x" ~&1)))) @@
                ~?"fib" @ ~&20)
    in
    let t7 = Term.DSL.
               (let_ "f" (fun_ "x" @@ (fun_ "y" @@ (add ~?"x" ~?"y"))) @@
                ((~?"f" @ ~&10) @ ~&20))
    in
    (* test t1; *)
    (* test t2; *)
    (* test t3; *)
    (* test t4; *)
    (* test t5; *)
    (* test t6; *)
    (* test t7; *)

    BenchUtils.sample := sample;
    BenchUtils.batch := batch;
    BenchUtils.(
      bench [
        create ~name:"t1" (fun () -> test t1);
        create ~name:"t2" (fun () -> test t2);
        create ~name:"t3" (fun () -> test t3);
        create ~name:"t4" (fun () -> test t4);
        create ~name:"t5" (fun () -> test t5);
        create ~name:"t6" (fun () -> test t6);
        create ~name:"t7" (fun () -> test t7);
      ])
end

