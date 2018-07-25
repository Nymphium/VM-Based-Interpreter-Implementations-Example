module Term = Term
module Irepr = Irepr
module Compile = struct
  let compile t = Compile.compile t
end

module type VM = sig
  val run : Irepr.cl -> Irepr.regv
end

module Vm1 : VM = Vm1
module Vm2 : VM = Vm2

module Cfg = struct
  type t = Cfg.t
  type fullcfg = Cfg.t

  let mkcfg = Cfg.mkcfg
end

