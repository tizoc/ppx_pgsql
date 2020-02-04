let test1 = [%sqlf "SELECT 1"]

let test2 = [%sqlf "SELECT hasindexes FROM pg_catalog.pg_tables WHERE hasrules = $hasrules"]

let test3 = [%sqlf "SELECT aggmtransspace FROM pg_catalog.pg_aggregate WHERE aggmtransspace = ANY($array::int[])"]

let test4 = [%sqlf "SELECT COUNT(*) FROM users WHERE id NOT IN (SELECT unnest($ids::int[]))"]