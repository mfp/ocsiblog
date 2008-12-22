type image = { img_url : string; img_title : string; img_link : string; }

type item = {
  it_title : string option;
  it_link : string option;
  it_description : string option;
  it_author : string option;
  it_category : string option;
  it_comments : string option;
  it_pubDate : float option;
  it_guid : (string * bool) option;
}

val make :
  title:string -> link:string -> description:string ->
  ?language:string -> ?copyright:string -> ?managingEditor:string ->
  ?webMaster:string -> ?pubDate:float -> ?lastBuildDate:float ->
  ?category:string -> ?generator:string -> ?ttl:int -> ?image:image -> 
  item list -> XML.elt

val make_item : ?title:string -> ?link:string -> ?description:string ->
  ?author:string -> ?category:string -> ?comments:string -> ?pubDate:float -> 
  ?guid:string * bool -> unit -> item
