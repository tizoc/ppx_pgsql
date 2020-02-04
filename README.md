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

## Some tips

### Views and NULL-able heuristic

This rewriter tries its best to figure out which columns are NULL-able and which are not, but sometimes it fails to do so.

One case is with columns in views, which will be assumed to always be NULL-able.

To fix this, you can alter the view metadata contained in the `pg_attribute` table, and set `attnotnull` to `true`:

```sql
UPDATE pg_attribute SET attnotnull = 't'
 WHERE attrelid IN (
   SELECT oid FROM pg_class
    WHERE relname = 'name_of_view');
```

### Outer joins, and using `COALESCE`

When performing joins, columns that on the original table are qualified as not NULL-able, may become NULL-able, this will make the heuristic fail.

One workaround is to create a view for such query, and then use the trick described above.

[Another option](https://github.com/tizoc/ppx_pgsql/issues/4#issuecomment-479106321) it to use the `COALESCE` function to force the column to be NULL-able:

```sql
-- Given these tables
CREATE TABLE authors (id serial PRIMARY KEY, name varchar(255) NOT NULL);
INSERT INTO authors (id, name) VALUES (1, 'John Doe');
CREATE TABLE books (id serial PRIMARY KEY, title varchar(255) NOT NULL, author int NOT NULL REFERENCES authors(id) ON DELETE CASCADE);

-- This join could result in NULL values for books.title
SELECT
 authors.name,
 coalesce(books.title) -- inferred as NULL-able now
FROM authors
LEFT OUTER JOIN books ON books.author = authors.id
```

Credit to @NightBlues for coming up with this solution.

### IN/NOT IN operator when using a possibly empty dynamic list of values

Using list expressions to build `IN`/`NOT IN` query expresions (`IN $@name` or `NOT IN $@name`) is not encouraged when the list of values is dynamic and has the potential of being empty.

The problem with doing so is that the list may be empty, resulting in an invalud query being generated (`IN ()` and `NOT IN ()` are not valid SQL). What is worse, this failure will happen at runtime.
Additionaly, by doing so with lists of varying length, a new prepared statement will be created at runtime for each one of the lengths.

An alternative is to use the `ANY` and `ALL` operators with arrays:

```sql
-- This
SELECT COUNT(*) FROM users
WHERE id IN $@user_ids_list

-- Becomes
SELECT COUNT(*) FROM users
WHERE id = ANY($user_ids_list::int[])

-- And this
SELECT COUNT(*) FROM users
WHERE id NOT IN $@user_ids_list

-- Becomes
SELECT COUNT(*) FROM users
WHERE id <> ALL($user_ids_list::int[])
```

Another option is to use the `unnest` array function and an inner `SELECT`:

```sql
SELECT COUNT(*) FROM users
WHERE id IN (SELECT unnest($user_ids_list::int[]))

SELECT COUNT(*) FROM users
WHERE id NOT IN (SELECT unnest($user_ids_list::int[]))
```