open XML
open ExtList

type image = {
  img_url : string;
  img_title : string;
  img_link : string;
}

type item = {
  it_title : string;
  it_link : string;
  it_description : string;
  it_author : string option;
  it_category : string option;
  it_comments : string option;
  (* it_enclosure :  *)
  it_pubDate : float option;
  it_guid : (string * bool) option;
  (* it_source:  *)
}

let item_date it = it.it_pubDate

let elm kind ?(attrs = []) v = Node(kind, attrs, [PCDATA v])
let opt_elm kind ?attrs = Option.map (elm kind ?attrs)
let some_elm kind ?attrs v = Some (elm kind ?attrs v)

let format_date t = Netdate.mk_mail_date t
let format_date_rdf t = Netdate.format "%Y-%m-%dT%H:%M:%SZ" (Netdate.create ~zone:0 t)

let filter_concat ls = List.filter_map (fun x -> x) (List.concat ls)
let filter_some = List.filter_map (fun x -> x)

let encode_pcdata s = [PCDATA s] (* TODO: use CDATA instead? *)

let xml_of_item item =
  Node
    ("item", [],
     filter_concat
       [[some_elm "title" item.it_title;
         some_elm "link" item.it_link;
         some_elm "description" item.it_description;
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

let make_rdf
      ~title ~link ~description ~date ?language ?copyright ?image items =
  let it_list_elm =
    Node
      ("items", [],
       [Node ("rdf:Seq", [],
             List.map
               (fun it -> Leaf ("rdf:li", [string_attrib "resource" it.it_link]))
               items)]) in
  let channel_elm =
    Node("channel", [string_attrib "rdf:about" link],
         filter_some
           [some_elm "title" title;
            some_elm "link" link;
            some_elm "description" description;
            Option.map
              (fun img -> Leaf("image", [string_attrib "rdf:resource" img.img_url]))
              image;
            opt_elm "dc:language" language;
            opt_elm "dc:rights" copyright;
            some_elm "dc:date" (format_date_rdf date);
            Some it_list_elm]) in
  let image_elm =
    Option.map
      (fun img ->
         [Node ("image", [string_attrib "rdf:about" img.img_url],
               [elm "title" img.img_title;
                elm "link" img.img_link;
                elm "url" img.img_url])])
      image in
  let elm_of_item it =
    let c = Node("content:encoded", [], encode_pcdata it.it_description) in
      Node ("item", [string_attrib "rdf:about" it.it_link],
            (elm "title" it.it_title ::
             elm "link" it.it_link ::
             c ::
             Option.map_default
               (fun date -> [elm "dc:date" (format_date_rdf date)])
               []
               it.it_pubDate));
  in Node
       ("rdf:RDF",
        [string_attrib "xmlns" "http://purl.org/rss/1.0/";
         string_attrib "xmlns:dc" "http://purl.org/dc/elements/1.1/";
         string_attrib "xmlns:rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
         string_attrib "xmlns:content" "http://purl.org/rss/1.0/modules/content/";
         string_attrib "xml:lang" "en"],
        List.concat
          [[channel_elm]; Option.default [] image_elm; List.map elm_of_item items])

let make_item
      ~title ~link ~description ?author ?category ?comments ?pubDate ?guid () =
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
