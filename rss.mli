(* Copyright (C) 2009 Mauricio Fernandez <mfp@acm.org> *)
type image = { img_url : string; img_title : string; img_link : string; }
type item

val item_date : item -> float option

val make :
  title:string -> link:string -> description:string -> ?self_link:string ->
  ?language:string -> ?copyright:string -> ?managingEditor:string ->
  ?webMaster:string -> ?pubDate:float -> ?lastBuildDate:float ->
  ?category:string -> ?generator:string -> ?ttl:int -> ?image:image ->
  item list -> XML.elt

val make_item : title:string -> link:string -> description:string ->
  ?author:string -> ?category:string -> ?comments:string -> ?pubDate:float ->
  ?guid:string * bool -> unit -> item

val make_rdf : title:string -> link:string -> description:string ->
  date:float -> ?language:string -> ?copyright:string -> ?image:image ->
  item list -> XML.elt
