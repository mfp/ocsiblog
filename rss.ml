open XML
open ExtList

type image = {
  img_url : string;
  img_title : string;
  img_link : string;
}

type item = {
  it_title : string option;
  it_link : string option;
  it_description : string option;
  it_author : string option;
  it_category : string option;
  it_comments : string option;
  (* it_enclosure :  *)
  it_pubDate : float option;
  it_guid : (string * bool) option;
  (* it_source:  *)
}

let elm kind ?(attrs = []) v = Node(kind, attrs, [PCDATA v])
let opt_elm kind ?attrs = Option.map (elm kind ?attrs)
let some_elm kind ?attrs v = Some (elm kind ?attrs v)

let format_date t = Netdate.mk_mail_date t

let filter_concat ls = List.filter_map (fun x -> x) (List.concat ls)

let xml_of_item item =
  Node
    ("item", [],
     filter_concat
       [[opt_elm "title" item.it_title;
         opt_elm "link" item.it_link;
         opt_elm "description" item.it_description;
         opt_elm "author" item.it_author;
         opt_elm "category" item.it_category;
         opt_elm "comments" item.it_comments;
         opt_elm "pubDate" (Option.map format_date item.it_pubDate);
         Option.map
           (fun (guid, is_perm) ->
              let perm = string_attrib "isPermaLink" (string_of_bool is_perm) in
                Node("guid", [perm], [PCDATA guid]))
           item.it_guid
       ]])

let make
      ~title ~link ~description
      ?language ?copyright ?managingEditor ?webMaster ?pubDate
      ?lastBuildDate ?category ?generator ?ttl ?image
      items =
  let channel =
    filter_concat
      [[some_elm "title" title;
        some_elm "link" link;
        some_elm "description" description;

        opt_elm "language" language;
        opt_elm "copyright" copyright;
        opt_elm "managingEditor" managingEditor;
        opt_elm "webMaster" webMaster;
        opt_elm "pubDate" (Option.map format_date pubDate);
        opt_elm "lastBuildDate" (Option.map format_date lastBuildDate);
        opt_elm "category" category;
        opt_elm "generator" generator;
        opt_elm "ttl" (Option.map string_of_int ttl);
        Option.map
          (fun img ->
             Node("image", [],
                  [elm "url" img.img_url;
                   elm "title" img.img_title;
                   elm "link" img.img_link]))
          image;
       ];
       List.map (fun it -> Some (xml_of_item it)) items
      ]
  in
    Node("rss", [string_attrib "version" "2.0"],
         [Node("channel", [], channel)])

let make_item
      ?title ?link ?description ?author ?category ?comments ?pubDate ?guid () =
  {
    it_title = title;
    it_link = link;
    it_description = description;
    it_author = author;
    it_category = category;
    it_comments = comments;
    it_pubDate = pubDate;
    it_guid = guid
  }
