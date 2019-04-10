module PGOCamlBind (PGOCaml : PGOCaml_generic.PGOCAML_GENERIC) = struct
  let dbh_private_data dbh =
    try PGOCaml.private_data dbh
    with
    | Not_found  ->
      let hash = Hashtbl.create 17  in
      (PGOCaml.set_private_data dbh hash; hash)

  let maybe_prepare_query ~dbh ~query ~name =
    let hash = dbh_private_data dbh in
    let is_prepared = Hashtbl.mem hash name in
    if is_prepared
    then PGOCaml.return ()
    else
      PGOCaml.bind (PGOCaml.prepare dbh ~name ~query ())
        (fun ()  ->
           Hashtbl.add hash name true; PGOCaml.return ())
end

let concat_row_values row =
  String.concat "; "
    (List.map
       (function
         | Some str -> Printf.sprintf "%S" str
         | None  -> "NULL") row)

let columns_count_error original_query values =
  "ppx_pgsql: internal error: " ^
  ("Incorrect number of columns returned from query: "
   ^ (original_query ^ (". Values are: " ^ values)))

let rebuild_query_with_placeholders query_fragments params =
  let param_idx = ref 0 in
  let placeholder_num = ref 0 in
  let mkplaceholder _ =
    incr placeholder_num;
    "$" ^ string_of_int !placeholder_num in
  let fragments =
    List.map (
      function
      | `Literal text -> text
      | `Variable (_name, false (* is_list *), _) ->
        incr param_idx;
        mkplaceholder ()
      | `Variable (_name, true (* is_list *), _) ->
        let list_param = List.nth params !param_idx in
        let list_placeholders = List.map mkplaceholder list_param in
        let expr = match list_placeholders with
          | [] -> "NULL"
          | _ -> String.concat "," list_placeholders in
        incr param_idx;
        "(" ^ expr ^ ")"
    ) query_fragments in
  String.concat "" fragments

let make_unique_identifier query =
  "ppx_pgsql." ^ (Digest.to_hex (Digest.string query))

module List = struct
  let map = List.map
  let rev_map = List.rev_map
  let flatten = List.flatten
end