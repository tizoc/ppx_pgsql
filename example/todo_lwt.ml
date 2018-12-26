module Store = struct
  module Lwt_thread = struct
    include Lwt
    let close_in = Lwt_io.close
    let really_input = Lwt_io.read_into_exactly
    let input_binary_int = Lwt_io.BE.read_int
    let input_char = Lwt_io.read_char
    let output_string = Lwt_io.write
    let output_binary_int = Lwt_io.BE.write_int
    let output_char = Lwt_io.write_char
    let flush = Lwt_io.flush
    let open_connection a = Lwt_io.open_connection a
    type in_channel = Lwt_io.input_channel
    type out_channel = Lwt_io.output_channel
  end

  module Db = PGOCaml_generic.Make(Lwt_thread)

  let database = Sys.getenv "USER"
  let connect = Db.connect ~database
  let pool : ((string, bool) Hashtbl.t Db.t) Lwt_pool.t =
    Lwt_pool.create 16 ~validate:Db.alive connect
  let with_pool f = Lwt_pool.use pool f

  let run_query f =
    with_pool f
end

(* Important: module named `PGOCaml` has to be in scope *)
(* this is what the generated code will reference *)
module PGOCaml = Store.Db

module Queries = struct
  let initialize = [%sqlf {|
      CREATE TABLE IF NOT EXISTS ppx_pgsql_example_todo_entries (
        id SERIAL PRIMARY KEY,
        line TEXT NOT NULL,
        status TEXT NOT NULL DEFAULT 'pending',
        created_at TIMESTAMP NOT NULL DEFAULT now(),
        updated_at TIMESTAMP
      )
    |}]

  let add_todo = [%sqlf {|
      INSERT INTO ppx_pgsql_example_todo_entries
             (line)
      VALUES ($line)
   RETURNING id
    |}]

  let set_todo_status = [%sqlf {|
      UPDATE ppx_pgsql_example_todo_entries
         SET status = $status,
             updated_at = now()
       WHERE id = $id
    |}]

  let remove_todo = [%sqlf {|
    DELETE FROM ppx_pgsql_example_todo_entries WHERE id = $id
  |}]

  let retrieve_all_todos = [%sqlf {|
      SELECT id, line, status, created_at, updated_at
        FROM ppx_pgsql_example_todo_entries
    ORDER BY created_at DESC
    |}]

  let retrieve_todos_by_status = [%sqlf {|
      SELECT id, line, status, created_at, updated_at
        FROM ppx_pgsql_example_todo_entries
       WHERE status = $status
    ORDER BY created_at DESC
    |}]
end

type todo = {
  id: int;
  line: string;
  status: [`Pending | `Done];
  created_at: CalendarLib.Calendar.t;
  updated_at: CalendarLib.Calendar.t option;
}

let string_of_status = function
  | `Pending -> "pending"
  | `Done -> "done"

let status_of_string = function
  | "pending" -> `Pending
  | "done" -> `Done
  | _ -> assert false

let todo_of_row (id, line, status, created_at, updated_at) =
  let id = Int32.to_int id in
  let status = status_of_string status in
  { id; line; status; created_at; updated_at; }

let init_db () =
  Store.run_query Queries.initialize

let add_todo line =
  match%lwt Store.run_query @@ Queries.add_todo ~line with
  | [id] -> Lwt.return @@ Int32.to_int id
  | _ -> assert false

let set_todo_status id status =
  let status = string_of_status status in
  Store.run_query @@
  Queries.set_todo_status ~id ~status

let remove_todo id =
  Store.run_query @@
  Queries.remove_todo ~id

let retrieve_todos ?status () =
  let%lwt results =
    Store.run_query @@ begin
      match status with
      | None -> Queries.retrieve_all_todos
      | Some status ->
        let status = string_of_status status in
        Queries.retrieve_todos_by_status ~status
    end
  in
  Lwt.return @@ List.map todo_of_row results

let show_todo todo =
  let { id; line; status; created_at; updated_at } = todo in
  let status = string_of_status status in
  let created_at = CalendarLib.Printer.Calendar.to_string created_at in
  let updated_at = match updated_at with
    | None -> "*"
    | Some ts -> CalendarLib.Printer.Calendar.to_string ts
  in
  Lwt_io.printf "%d. %s %s %s %s\n" id created_at updated_at status line

let show_todos todos =
  let%lwt _ = Lwt_list.map_s show_todo todos in
  Lwt.return_unit

let main args =
  Lwt_main.run @@ begin
    match args with
    | ["init"] -> init_db ()
    | ["show"] ->
      let%lwt results = retrieve_todos () in
      show_todos results
    | ["show"; "--pending"] ->
      let%lwt results = retrieve_todos ~status:`Pending () in
      show_todos results
    | ["show"; "--done"] ->
      let%lwt results = retrieve_todos ~status:`Done () in
      show_todos results
    | ["add"; line] ->
      let%lwt id = add_todo line in
      Lwt_io.printf "Added ID=%d\n" id
    | ["set-status"; id; status] -> begin
        match Int32.of_string id, status_of_string status with
        | exception _ -> Lwt_io.printf "Invalid id=%s or status=%s\n" id status
        | id, status ->
          let%lwt () = set_todo_status id status in
          Lwt_io.printf "Updated.\n"
      end
    | ["remove"; id] -> begin
        match Int32.of_string_opt id with
        | None -> Lwt_io.printf "Invalid id: %s\n" id
        | Some id -> remove_todo id
      end
    | _ ->
      Lwt_io.printf "Invalid command or parameters\n"
  end

let () =
  Sys.argv
  |> Array.to_list
  |> List.tl
  |> main
