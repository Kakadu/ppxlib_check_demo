
open Ppxlib


let string_of_expression e =
  Format.set_margin 1000;
  Format.set_max_indent 2000;
  let ans = Format.asprintf "%a" Pprintast.expression e in
  ans
;;

let mapper =
  object
    inherit Ast_traverse.map as super

    method! expression e =
      match e with
      | { pexp_desc = Pexp_construct ({ txt = Lident "REPR"; _ }, Some e); _ } as expr ->
          let text = string_of_expression e in
          { expr with
            pexp_desc =
              Pexp_tuple [ Ast_helper.Exp.constant (Pconst_string (text, e.pexp_loc, None)); e ]
          }
      | e -> super#expression e
  end
;;

let () = Ppxlib.Driver.register_transformation ~impl:(fun s -> mapper#structure s) "repr"
