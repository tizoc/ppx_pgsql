open Migrate_parsetree
open Ast_406

open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let connection : unit PGOCaml.t option ref = ref None

let nullable_query = "nullable"
let unravel_query = "unravel"
let typname_query = "typname"

let reflection_queries = [
  nullable_query, "SELECT attnotnull FROM pg_attribute " ^
                  "WHERE attrelid = $1 AND attnum = $2";
  unravel_query, "SELECT typtype, typbasetype FROM pg_type WHERE oid = $1";
  typname_query, "SELECT typname FROM pg_type WHERE oid = $1";
]

let prepare_connection_reflection_queries dbh =
  List.iter
    (fun (name, query) ->
       PGOCaml.prepare dbh ~query:query ~name:name ())
    reflection_queries;
  dbh

let connect () =
  match !connection with
  | Some dbh -> dbh
  | None ->
    let dbh = PGOCaml.connect () in
    connection := Some dbh;
    prepare_connection_reflection_queries dbh

let rec name_of_type ~loc ?modifier dbh oid =
  try
    PGOCaml.name_of_type ?modifier oid
  with PGOCaml.Error msg as exn ->
    let params = [Some (PGOCaml.string_of_oid oid)] in
    let exec = PGOCaml.execute dbh ~params in
    match exec ~name:typname_query () with
    | [[Some "citext"]] -> "string"
    | [[Some "hstore"]] -> "hstore"
    (* TODO: option for custom handling of types *)
    | _ ->
      begin
        match exec ~name:unravel_query () with
        | [[Some "e"; _]] -> "string"       (* Enum, treat as string *)
        | [[Some "d"; Some typbasetype]] ->
          (* Result of CREATE DOMAIN, have to find underlying data type *)
          (* Follow parents until one is recognized *)
          name_of_type ~loc ?modifier dbh (PGOCaml.oid_of_string typbasetype)
        | _ -> Location.raise_errorf ~loc "[%%sqlf]: %s" (Printexc.to_string exn)
      end

let rec enumerate start finish =
  if start < finish
  then start :: enumerate (start+1) finish
  else []

let mkconst_string ~loc str =
  Exp.constant ~loc (Const.string str)

let mkident ~loc name =
  Exp.ident ~loc { txt = Lident name; loc }

module Varmap = struct
  type t = {
    size: int;
    elements: (string * bool * bool) array;
  }

  let empty_value = ("", false, false)

  let empty () =
    { size = 0; elements = Array.init 1000 (fun _ -> empty_value); }

  let verify { size; elements; } =
    for i = 0 to size do
      let name, is_list, is_option = elements.(i) in
      for j = i + 1 to size do
        let name', is_list', is_option' = elements.(j) in
        if name = name' && not (is_list = is_list' && is_option = is_option') then begin
          failwith (
            "Parameter '" ^ name ^
            "' appears more than once with different modifiers"
          )
        end
      done
    done

  let from_query_fragments query_fragments =
    let result =
      List.fold_left
        begin fun vm fragment ->
          match fragment with
          | `Literal text -> vm
          | `Variable v ->
            let varnum = vm.size + 1 in
            vm.elements.(varnum) <- v;
            { vm with size = varnum }
        end
        (empty ())
        query_fragments
    in
    verify result;
    result

  let size vm = vm.size

  let get vm i =
    if i > vm.size then
      Printf.fprintf stderr "accessed var out of range %d\n%!" i;
    vm.elements.(i)

  let to_list vm =
    Array.to_list @@ Array.sub vm.elements 1 vm.size
end

(* OCaml varname regexp *)
let varname_re =
  let open Re in
  seq [
    alt [char '_'; rg 'a' 'z'];
    rep (
      alt [char '_';
           char '\'';
           rg 'a' 'z';
           rg 'A' 'Z';
           rg '0' '9'])]

(* Embedded SQL variable regexp.
 * Matches:
 *   $name     - normal value
 *   $@name    - list expression value
 *   $?name    - option value (None becomes NULL)
 *   $@?name   - option list expression value
*)
let query_vars_re =
  let open Re in
  compile @@
  seq [
    char '$';                   (* variable *)
    opt (group (char '@'));     (* list expression *)
    opt (group (char '?'));     (* option *)
    group (varname_re);]

type query_fragment = [
  | `Literal of string
  | `Variable of (string * bool * bool) (* name, list expression, option *)
]

let split_query query =
  query
  |> Re.split_full query_vars_re
  |> List.map begin function
    | `Text text -> `Literal text
    | `Delim subs ->
      let name, is_list, is_option =
        Re.get subs 3, Re.test subs 1, Re.test subs 2 in
      `Variable (name, is_list, is_option)
  end

let build_query_template query_fragments =
  let i = ref 0 in
  String.concat "" @@
  List.map (
    function
    | `Literal text -> text
    | `Variable (name, is_list, is_option) ->
      incr i;
      if is_list
      then Printf.sprintf "($%d)" !i
      else Printf.sprintf "$%d" !i)
    query_fragments

(* Builders for subexpressions used un generated code *)
module Build_expr = struct
  let params_mapper ~loc ~dbh ~params ~varmap =
    List.fold_right
      (fun (i, { PGOCaml.param_type }) rest ->
         let name, is_list, is_option = Varmap.get varmap i in
         let type_name = name_of_type ~loc dbh param_type in
         let converter_name = "string_of_" ^ type_name in
         let to_string_func = mkident ~loc converter_name in
         let to_string_func = [%expr PGOCaml.([%e to_string_func])] in
         let variable = mkident ~loc name in
         let mapper =
           match is_list, is_option with
           | false, false ->      (* not list nor optional *)
             [%expr [Some ([%e to_string_func] [%e variable])]]
           | false, true ->       (* not list, but optional *)
             [%expr [PGOCaml_aux.Option.map
                       [%e to_string_func]
                       [%e variable]]]
           | true, false ->       (* list, not optional *)
             [%expr
               Stdlib.List.map
                 (fun x -> Some ([%e to_string_func] x))
                 [%e variable]]
           | true, true ->        (* list and optional *)
             [%expr
               Stdlib.List.map
                 (fun x -> PGOCaml_aux.Option.map [%e to_string_func])
                 [%e variable]]
         in
         [%expr [%e mapper]::[%e rest]]
      )
      (List.combine (enumerate 1 (Varmap.size varmap + 1)) params)
      [%expr []]

  let results_mapper ~loc dbh results =
    let conversions =
      List.mapi (
        fun i result ->
          let field_type = result.PGOCaml.field_type in
          let type_name = name_of_type ~loc dbh field_type in
          let converter_name = type_name ^ "_of_string" in
          let of_string_func = mkident ~loc converter_name in
          let of_string_func = [%expr PGOCaml.([%e of_string_func])] in
          let nullable =
            match (result.PGOCaml.table, result.PGOCaml.column) with
            | Some table, Some column ->
              (* Find out whether the column is nullable from the
               * database pg_attribute table. *)
              let params =
                [Some (PGOCaml.string_of_oid table);
                 Some (PGOCaml.string_of_int column);] in
              let _rows =
                PGOCaml.execute dbh ~name:nullable_query ~params () in
              let not_nullable =
                match _rows with
                | [[Some b]] -> PGOCaml.bool_of_string b
                | _ -> false in
              not not_nullable
            | _ -> true (* Assume it could be nullable. *) in
          let col = mkident ~loc  ("c" ^ string_of_int i) in
          if nullable then
            [%expr
              PGOCaml_aux.Option.map
                [%e of_string_func]
                [%e col]]
          else
            [%expr
              let value =
                try PGOCaml_aux.Option.get [%e col] with
                  _ -> failwith
                         "ppx_pgsql's nullability heuristic has failed"
              in [%e of_string_func] value]
      ) results in
    match conversions with
    | [] -> [%expr ()]
    | [a] -> a
    | conversions -> Exp.tuple conversions

  let match_row ~loc results =
    List.fold_right
      (fun i tail ->
         let var = Pat.var { txt = "c" ^ string_of_int i; loc } in
         [%pat? [%p var]::[%p tail]])
      (enumerate 0 (List.length results))
      [%pat? []]

  let query_fragments ~loc query_fragments =
    List.fold_right (
      fun s tail ->
        let head = match s with
          | `Literal text ->
            [%expr `Literal [%e mkconst_string ~loc text]]
          | `Variable (varname, is_list, is_option) ->
            let is_list =
              if is_list then [%expr true] else [%expr false] in
            let is_option =
              if is_option then [%expr true] else [%expr false] in
            [%expr `Variable
                ([%e mkconst_string ~loc varname],
                 [%e is_list],
                 [%e is_option])] in
        [%expr [%e head] :: [%e tail]]
    ) query_fragments [%expr []]

  let exec_query ~loc ~params_mapper_expr ~query_fragments_expr =
    [%expr
      let rebuild_query_with_placeholders query_fragments params =
        let param_idx = ref 0 in
        let placeholder_num = ref 0 in
        let mkplaceholder _ =
          incr placeholder_num;
          "$" ^ string_of_int !placeholder_num in
        let fragments =
          Stdlib.List.map (
            function
            | `Literal text -> text
            | `Variable (name, false (* is_list *), _) ->
              incr param_idx;
              mkplaceholder ()
            | `Variable (name, true (* is_list *), _) ->
              let list_param = Stdlib.List.nth params !param_idx in
              let list_placeholders = Stdlib.List.map mkplaceholder list_param in
              incr param_idx;
              "(" ^ Stdlib.String.concat "," list_placeholders ^ ")"
          ) query_fragments in
        Stdlib.String.concat "" fragments in
      let params : string option list list = [%e params_mapper_expr] in
      let query_fragments = [%e query_fragments_expr] in
      let query = rebuild_query_with_placeholders query_fragments params in
      let params = Stdlib.List.flatten params in
      let unique_query_identifier =
        "ppx_pgsql." ^ Stdlib.Digest.to_hex (Stdlib.Digest.string query) in
      let dbh_private_data dbh =
        try PGOCaml.private_data dbh
        with
        | Not_found ->
          let hash = Stdlib.Hashtbl.create 17 in
          PGOCaml.set_private_data dbh hash;
          hash
      in
      let maybe_prepare_query ~dbh ~query ~name =
        let hash = dbh_private_data dbh in
        let is_prepared = Stdlib.Hashtbl.mem hash name in
        if is_prepared then PGOCaml.return ()
        else
          PGOCaml.bind
            (PGOCaml.prepare dbh ~name ~query ())
            (fun () ->
               Stdlib.Hashtbl.add hash name true;
               PGOCaml.return ())
      in
      fun dbh ->
        PGOCaml.bind
          (maybe_prepare_query ~dbh ~query ~name:unique_query_identifier)
          (fun () ->
             PGOCaml.execute_rev dbh ~name:unique_query_identifier ~params ())
    ]

  let columns_count_mismatch_error ~loc query =
    [%expr
      let original_query = [%e mkconst_string ~loc query] in
      let values = Stdlib.String.concat "; " (
          Stdlib.List.map (
            function
            | Some str -> Stdlib.Printf.sprintf "%S" str
            | None -> "NULL"
          ) row) in             (* 'row' comes from context *)
      let msg =
        "ppx_pgsql: internal error: " ^
        "Incorrect number of columns returned from query: " ^
        original_query ^ ". Values are: " ^ values in
      raise (PGOCaml.Error msg)]

  let rec labelled_fun ~loc ?(seen_names = []) vars body_expr =
    match vars with
    | [] -> body_expr
    | (name, _, _)::rest when List.mem name seen_names ->
      labelled_fun ~loc ~seen_names rest body_expr
    | (name, _, is_option)::rest ->
      let seen_names = name::seen_names in
      let param = if is_option then (Optional name) else (Labelled name) in
      Exp.fun_ ~loc param None
        (Pat.var ~loc { txt = name; loc })
        (labelled_fun ~loc ~seen_names rest body_expr)
end

let expand_query loc query =
  let ct_dbh = connect () in
  let query_fragments = split_query query in
  let varmap =
    try Varmap.from_query_fragments query_fragments
    with Failure msg -> Location.raise_errorf ~loc "[%%sqlf] %s" msg
  in
  let query = build_query_template query_fragments in
  let params, results =
    try
      PGOCaml.prepare ct_dbh ~query ();
      PGOCaml.describe_statement ct_dbh ()
    with
    | PGOCaml.PostgreSQL_Error (msg, _) -> Location.raise_errorf ~loc "[%%sqlf]: %s" msg
    | exn -> Location.raise_errorf ~loc "[%%sqlf]: %s" (Printexc.to_string exn) in

  if Varmap.size varmap <> List.length params then
    Location.raise_errorf ~loc "[%%sqlf]: Unexpected amount of parameters detected";

  let params_mapper_expr =
    Build_expr.params_mapper ~loc ~dbh:ct_dbh ~params ~varmap in
  let query_fragments_expr =
    Build_expr.query_fragments ~loc query_fragments in
  let exec_query_expr =
    Build_expr.exec_query ~loc ~params_mapper_expr ~query_fragments_expr in

  let final_expr = begin
    match results with
    | Some results ->             (* has results, build mapping code *)
      let match_row_pat = Build_expr.match_row ~loc results in
      let results_mapper_expr = Build_expr.results_mapper ~loc ct_dbh results in
      [%expr
        fun dbh ->
          PGOCaml.bind
            ([%e exec_query_expr] dbh)
            (fun rows ->
               PGOCaml.return @@
               Stdlib.List.rev_map begin function
                 | [%p match_row_pat] -> [%e results_mapper_expr]
                 | row -> [%e Build_expr.columns_count_mismatch_error ~loc query]
               end rows)
      ]
    | None ->                     (* 0 columns result, return unit *)
      [%expr
        fun dbh ->
          PGOCaml.bind
            ([%e exec_query_expr] dbh)
            (fun _ -> PGOCaml.return ())]
  end in
  Build_expr.labelled_fun ~loc (Varmap.to_list varmap) final_expr

let pgsql_mapper _config _cookies =
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      (* [%sqlf <sql string>] *)
      | { pexp_desc =
            Pexp_extension (
              { txt = "sqlf"; _ },
              PStr [{
                  pstr_desc = Pstr_eval ({
                      pexp_desc = Pexp_constant (Pconst_string (str, _))}, _)
                }]);
          pexp_loc = loc;
        } ->
        expand_query loc str
      | other ->
        default_mapper.expr mapper other
  }

let () =
  Driver.register ~name:"pgsql" Versions.ocaml_406 pgsql_mapper
