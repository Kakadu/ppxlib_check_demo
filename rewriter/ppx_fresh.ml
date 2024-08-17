open Ppxlib
open Ppxlib.Ast_helper
open Stdppx

let mapper =
  object (self)
    inherit Ast_traverse.map as super

    method! expression e =
      match e.pexp_desc with
      | Pexp_fun (l, opt, pat, e) ->
          { e with pexp_desc = Pexp_fun (l, opt, pat, self#expression e) }
      | Pexp_constraint (ee, t) ->
          { e with pexp_desc = Pexp_constraint (self#expression ee, t) }
      | _ -> e
  end

let () = Ppxlib.Driver.register_transformation ~impl:mapper#structure "asdfasdf"
