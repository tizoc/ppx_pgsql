# ppx_pgsql

Syntax extension for embedded SQL queries using PG'OCaml.

## How it works

Expressions of the form `[%sqlf <sql string>]` are validated and converted into a function that will execute the query. The generated function will take each named argument (denoted as names starting with the dollar symbol, e.g. `$parameter_name`) as a keyword argument with the same name, and a database handle as the last argument.

### Example:

```ocaml
  let update_account =
    [%sqlf {|
      UPDATE accounts
         SET email = $email, account_type = $account_type
       WHERE account_id = $account_id
   RETURNING account_id, created_at, account_type, email
    |}]
```

The type of `update_account` is:

```ocaml
email:string ->
account_type:string ->
account_id:int64 ->
(string, bool) Hashtbl.t Pg_store_helpers.PGOCaml.t ->
(int64 * CalendarLib.Calendar.t * string * string) list
PGOCaml.monad
```

## Named arguments syntax

- `$name` - normal value
- `$@name` - list expression value
- `$?name` - option value (None becomes NULL)
- `$@?name` - option list expression value

## To install

```
opam pin add ppx_pgsql -k git https://github.com/tizoc/ppx_pgsql.git
```
