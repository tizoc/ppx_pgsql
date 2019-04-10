let test1 = [%sqlf "SELECT 1"]

let test2 = [%sqlf "SELECT hasindexes FROM pg_catalog.pg_tables WHERE hasrules = $hasrules"]

let test_list = [%sqlf {|
  SELECT true WHERE $?needle::integer IN $@haystack
|}]

let check msg f =
  print_string @@ msg ^ "... " ;
  try
    assert (f ());
    print_endline "OK"
  with e ->
    print_endline "FAIL";
    raise e

let () =
  let dbh = PGOCaml.connect () in
  check "test 1" (fun () ->
      let res = test1 dbh in
      res = [Some 1l]);

  check "test 2" (fun () ->
      let _res = test2 ~hasrules:true dbh in
      (* TODO: fixme *)
      true);

  check "test list param positive" (fun () ->
      let res = test_list ~needle:1l ~haystack:[1l;2l;3l] dbh in
      res = [Some true]);

  check "test list param negative" (fun () ->
      let res = test_list ~needle:4l ~haystack:[1l;2l;3l] dbh in
      res = []);

  check "test list param with NULL" (fun () ->
      let res = test_list ?needle:None ~haystack:[1l;2l;3l] dbh in
      res = []);

  check "test list param with empty list" (fun () ->
      let res = test_list ~needle:1l ~haystack:[] dbh in
      res = []);

  PGOCaml.close dbh
