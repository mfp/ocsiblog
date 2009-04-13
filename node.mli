(* Copyright (C) 2009 Mauricio Fernandez <mfp@acm.org> *)
include Catalog.ENTRY with type sort_criterion = [ `Date | `Title ]

val split_headers_body : string -> (string * string) list * string list

val make : name:string -> file:string -> 'html entry
val date : _ entry -> float
val markup : _ entry -> Simple_markup.paragraph list
val syndicated : _ entry -> bool
val allow_comments : _ entry -> bool
val title : _ entry -> string
val get_html :
  (Simple_markup.paragraph list -> 'html XHTML.M.elt list) ->
  'html entry -> 'html XHTML.M.elt list

val is_inner_link : string -> bool

