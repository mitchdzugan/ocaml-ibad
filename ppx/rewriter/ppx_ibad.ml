open Ppxlib

let name = "ibad"

let expand ~loc ~path:_ optic =
  [%expr (module struct include (val [%e optic]) end)]

let ext =
  Extension.declare
    name
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand

let () = Ppxlib.Driver.register_transformation name ~extensions:[ext]