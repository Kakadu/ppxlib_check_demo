open Ppxlib

let string_of_expression e =
  Format.set_margin 1000;
  Format.set_max_indent 2000;
  Format.asprintf "%a" Pprintast.expression e

let mapper =
  object
    inherit Ast_traverse.map as super

    method! expression e =
      match e with
      | { pexp_desc = Pexp_construct ({ txt = Lident "REPR"; _ }, Some e); _ }
        ->
          let text = string_of_expression e in
          let pexp_loc = { e.pexp_loc with loc_ghost = true } in
          {
            e with
            pexp_loc;
            pexp_desc =
              Pexp_tuple
                [
                  Ast_helper.Exp.constant (* ~loc:Location.none *)
                    ~loc:pexp_loc
                    (Pconst_string (text, pexp_loc, None));
                  (* (let loc = pexp_loc in
                     [%expr 1 ^ ""]); *)
                  e;
                ];
          }
      | e -> super#expression e
  end

let () = Ppxlib.Driver.register_transformation ~impl:mapper#structure "repr"
