module Term = Term
module Irepr = Irepr
module Compile = struct
  let compile t = Compile.compile t
end

module Vm1 = Vm1

module Cfg = struct
  type t = Cfg.t
  type fullcfg = Cfg.t

  let mkcfg = Cfg.mkcfg
end

