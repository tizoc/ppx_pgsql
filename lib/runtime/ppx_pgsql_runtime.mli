module PGOCamlBind : functor (PGOCaml : PGOCaml_generic.PGOCAML_GENERIC) -> sig
  val maybe_prepare_query :
    dbh:(string, bool) Hashtbl.t PGOCaml.t ->
    query:string -> name:string -> unit PGOCaml.monad
end

module List : sig
  val map : ('a -> 'b) -> 'a list -> 'b list
  val rev_map : ('a -> 'b) -> 'a list -> 'b list
  val flatten : 'a list list -> 'a list
end

val concat_row_values : string option list -> string
val columns_count_error : string -> string -> string
val rebuild_query_with_placeholders :
    [< `Literal of string | `Variable of 'string * bool * 'bool ] list -> string option list list -> string
val make_unique_identifier : string -> string