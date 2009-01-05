open Printf
open Eliom_parameters
open Lwt
open XHTML.M
open Eliom_predefmod.Xhtmlcompact
open ExtString
open ExtList

module Pages = Catalog.Make(Node)
module SM = Simple_markup

let with_class f ?(a = []) klass = f ?a:(Some (a_class [klass] :: a))
let div_with_class klass ?(a = []) l = div ~a:(a_class [klass] :: a) l
let div_with_id id ?(a = []) l = div ~a:(a_id id :: a) l

let pagedir = ref "pages"
let commentdir = ref "comments"
let toplevel_link = ref "eigenclass.org"
let toplevel_title = ref "eigenclass"
let toplevel_pages = ref 5
let toplevel_links = ref 10
let refresh_period = ref 10.
let rss_title = ref "eigenclass"
let rss_link = ref "http://eigenclass.org"
let rss_description = ref !rss_title
let rss_nitems = ref 10
let port = 80
let encoding = ref "UTF-8"
let css_uri = uri_of_string "/R2/ocsiblog.css"
let ctype_meta = meta ~content:("text/html; charset=" ^ !encoding)
                   ~a:[a_http_equiv "Content-Type"] ()
let copyright =
  p [pcdata "Copyright "; entity "copy"; pcdata " 2005-2009 Mauricio Fernández"]
let footer = div_with_id "footer" [copyright]

let pages = Pages.make !pagedir
let comments = Comments.make !commentdir

let not_found () = raise Eliom_common.Eliom_404
let (!!) = Lazy.force

let attachment_file page basename =
  String.join "/" [!pagedir; page ^ ".files"; basename]

let anchor ?title ~name contents =
  let title = Option.map_default (fun t -> [a_title t]) [] title in
    XHTML.M.a
      ~a:(a_class ["anchor"] :: a_href (uri_of_string ("#" ^ name)) :: title)
      [pcdata contents]

let maybe_ul ?a = function
    [] -> pcdata ""
  | hd::tl -> ul ?a hd tl

let format_date t = Netdate.format "%d %B %Y" (Netdate.create t)
let format_date_time t = Netdate.format "%d %B %Y at %R" (Netdate.create t)

let abs_service_uri service ~sp params =
  make_full_uri ~sp ~port ~service:!!service params

let abs_service_link service ~sp desc params =
  XHTML.M.a ~a:[a_href (abs_service_uri service ~sp params)] desc

let map_body_uri ~relative ~broken ~not_relative uri =
  try
    let url = Neturl.parse_url uri in
      not_relative url
  with Neturl.Malformed_URL -> (* a relative URL, basic verification *)
    match Str.split_delim (Str.regexp "/") uri with
        [ page; file ] when file <> "" && Pages.has_entry pages page ->
          relative page file
      | _ -> (* broken relative link *) broken uri

let render_link_aux ~link_attachment ~link_page href =
  let uri = href.SM.href_target in
  let desc = pcdata href.SM.href_desc in
    if Node.is_inner_link uri then begin
      if Pages.has_entry pages uri then link_page [desc] uri
      else desc
    end else
      map_body_uri
        ~not_relative:(fun _ -> XHTML.M.a ~a:[a_href (uri_of_string uri)] [desc])
        ~relative:(fun page file -> link_attachment [desc] (page, file))
        ~broken:(fun _ -> desc)
        uri

let rec page_with_title sp thetitle thebody =
  let html =
    (html
       (head (title (pcdata thetitle)) [css_link css_uri (); ctype_meta; rss2_link sp])
       (body thebody)) in
  let txt = Xhtmlcompact_lite.xhtml_print ~version:`HTML_v04_01 ~html_compat:true html
  in return (txt, "text/html")

and page_service =
  lazy (Eliom_predefmod.Text.register_new_service
          ~path:[""] ~get_params:(suffix (string "page")) serve_page)

and serve_page sp page () = match Pages.get_entry pages page with
    None -> not_found ()
  | Some node ->
      let thetitle = Node.title node in
      let toplink = a ~service:!!toplevel_service ~sp [pcdata !toplevel_link] ()
      in page_with_title sp thetitle
           [div_with_id "article_body"
              (div_with_id "header"
                 [h1 [a ~service:!!page_service ~sp [pcdata thetitle] page];
                  with_class p "date" [pcdata (format_date (Node.date node))];
                  p [toplink]] ::
               (node_body_with_comments ~sp node @ [footer]))]

and attachment_service = lazy begin
  Eliom_predefmod.Files.register_new_service
    ~path:[""]
    ~get_params:(suffix (string "page" ** string "file"))
    (fun sp (page, file) () ->
       if not (Pages.has_entry pages page) then not_found ()
       else
         match String.nsplit file "/" with
             [basename] -> return (attachment_file page file)
           | _ -> not_found ())
end

and toplevel_service = lazy begin
  Eliom_predefmod.Text.register_new_service
    ~path:[""]
    ~get_params:unit
    (fun sp () () ->
       let all = Pages.sorted_entries ~reverse:true `Date pages in
       let pages = List.take !toplevel_pages (List.filter Node.syndicated all) in
       let links = List.map (entry_link sp) (List.take !toplevel_links all) in
         page_with_title sp
            !toplevel_title
            [div_with_id "toplevel_body"
               [div_with_id "main" (List.map (entry_div sp) pages);
                div_with_id "sidebar" [maybe_ul links; subscribe sp; footer]]])
end

and subscribe sp =
  a ~a:[a_class ["subscribe"]] ~service:!!rss2_service ~sp [pcdata "Subscribe"] ""

and dummy_comment_service = lazy begin
  Eliom_predefmod.Redirection.register_new_service
    ~path:[""]
    ~get_params:(string "page")
    (fun sp page () -> return (Eliom_services.preapply !!page_service page))
end

and post_comment_service = lazy begin
  Eliom_predefmod.String_redirection.register_new_post_service
    ~options:`Permanent
    ~fallback:!!dummy_comment_service
    ~post_params:(string "author" ** string "body")
    (fun sp page (author, body) ->
       let uri = make_full_string_uri ~sp ~service:!!page_service page in
       let ret x = return (uri_of_string x) in
         if String.strip body = "" then
           ret uri
         else
           try
             if Pages.has_entry pages page then
               let c = Comments.add_comment comments page ~author ~body () in
                 ret (uri ^ "#" ^ comment_id c)
                 else ret uri (* will 404 in page_service *)
           with _ -> ret uri)
end

and rss2_service = lazy begin
  Eliom_predefmod.Text.register_new_service
    ~path:["rss2"]
    ~get_params:(suffix (string "tags"))
    (fun sp taglist () ->
       generate_xml
         (Rss.make
            ~title:!rss_title
            ~link:!rss_link
            ~description:!rss_description
            ~ttl:180
            (get_rss_items sp (String.nsplit taglist ","))))
end

and rss1_service = lazy begin
  Eliom_predefmod.Text.register_new_service
    ~path:["rss1"]
    ~get_params:(suffix (string "tags"))
    (fun sp taglist () ->
       let items = get_rss_items sp (String.nsplit taglist ",") in
       let max_f (a : float) b = if a > b then a else b in
       let date = match List.filter_map Rss.item_date items with
           [] -> Unix.gettimeofday ()
         | hd::tl -> List.fold_left max_f hd tl
       in generate_xml (Rss.make_rdf
                          ~title:!rss_title
                          ~link:!rss_link
                          ~description:!rss_description
                          ~date
                          items))
end

and make_rss_link ?(type_="application/rss+xml") sp serv title =
  link ~a:[a_href (abs_service_uri serv ~sp ""); a_rel [`Alternate];
           a_title title; a_type type_] ()

and rss2_link sp = make_rss_link sp rss2_service !rss_title

and generate_xml xml =
  let b = Buffer.create 256 in
  let add = Buffer.add_string b in
    XML.decl ~version:"1.0" ~encoding:!encoding add ();
    XML.output add xml;
    return (Buffer.contents b, "text/xml")

and get_rss_items sp tags =
  let all = Pages.sorted_entries ~reverse:true `Date pages in
  let tags_match node =
    tags = [] || tags = ["all"] ||
    List.exists (fun t -> List.mem t tags) (Node.tags node) in
  let nodes =
    List.take !rss_nitems
      (List.filter (fun n -> Node.syndicated n && tags_match n) all)
  in
    List.map
      (fun node ->
         let link = string_of_uri (abs_service_uri page_service ~sp (Node.name node))
         in Rss.make_item ~title:(Node.title node) ~link
           ~pubDate:(Node.date node) ~guid:(link, true)
           ~description:(render_node_for_rss ~sp node) ())
      nodes

and node_body_with_comments ~sp node =
  let page = Node.name node in
  let allow_comments = Node.allow_comments node in
  let body = Node.get_html (render_node sp) node in
  let cs = Option.default [] (Comments.get_comments comments page) in
  let comment_form = match allow_comments with
      true -> [ post_form !!post_comment_service sp comment_form page ]
    | false -> [] in
  let comments_div = match cs, allow_comments with
      [], false -> []
    | _ -> h2 [pcdata "Comments"] :: format_comments ~sp cs
  in
    [div_with_id "article" body;
     div_with_id "addendum"
       [ hr ();
         div_with_class "article_date" [pcdata (format_date_time (Node.date node))]];
     div_with_id "comments" ( comments_div @ comment_form )]

and render_node sp =
  Simple_markup__html.to_html
    ~render_pre:(render_pre sp)
    ~render_link:(render_link sp)
    ~render_img:(render_img sp)

and render_pre sp ~kind txt = match kind with
    "html" -> unsafe_data txt
  | _ -> pre [code [pcdata txt]]

and render_img sp img =
  XHTML.M.img
    ~a:[a_class ["centered"]]
    ~src:(uri_of_string img.SM.img_src) ~alt:img.SM.img_alt ()

and render_link sp href =
  render_link_aux
    ~link_page:(a ~service:!!page_service ~sp)
    ~link_attachment:(a ~service:!!attachment_service ~sp)
    href

and format_comments ~sp l = match List.fast_sort (Comments.compare `Date) l with
    [] -> []
  | c :: cs -> [ol ~a:[a_class ["comments"]]
                 (format_comment ~sp c) (List.map (format_comment ~sp) cs)]

and format_comment ~sp c = let comm_id = comment_id c in
  li ~a:[a_id comm_id]
    [div_with_class "comment"
       [ div_with_class "comment_body"
           (render_comment_body sp c.Comments.c_markup);
         div_with_class "comment_meta"
           [ entity "mdash";
             with_class span "comment_author" [pcdata c.Comments.c_author];
             pcdata ", ";
             with_class span "comment_date"
               [pcdata (format_date_time c.Comments.c_date) ];
             anchor ~title:"Permanent link to this comment" ~name:comm_id "#" ]]]

and comment_id c = "comment-" ^ c.Comments.c_id

and render_comment_body sp =
  Simple_markup__html.to_html
    ~render_pre:(fun ~kind txt -> pre [code [pcdata txt]])
    ~render_img:(fun img ->
                   XHTML.M.a ~a:[a_href (uri_of_string img.SM.img_src)]
                     [pcdata img.SM.img_alt])
    ~render_link:(render_link sp)

and comment_form (author_name, body_name) =
  [p [ pcdata "Author:"; br ();
       string_input ~input_type:`Text ~name:author_name (); br ();
       pcdata "Comment:"; br ();
       textarea ~name:body_name ~rows:10 ~cols:82 (); br ();
       string_input ~input_type:`Submit ~value:"Leave comment" () ]]

and entry_div sp node =
  div_with_class "entry"
    [h2 ~a:[a_class ["entry_title"]]
       [span ~a:[a_class ["date"]] [pcdata (format_date_time (Node.date node))];
        pcdata " ";
        span ~a:[a_class ["title"]] [link_to_node sp node]];
     div_with_class "entry_body" (Node.get_html (render_node sp) node)]

and entry_link sp node = li [ link_to_node sp node ]

and link_to_node sp node =
  a ~service:!!page_service ~sp [pcdata (Node.title node)] (Node.name node)

and render_node_for_rss ~sp node =
  let html =
    Simple_markup__html.to_html
      ~render_pre:(render_pre sp)
      ~render_link:begin
        render_link_aux
          ~link_attachment:(abs_service_link attachment_service ~sp)
          ~link_page:(abs_service_link page_service ~sp)
      end
      ~render_img:begin fun img ->
        let uri = img.SM.img_src and alt = img.SM.img_alt in
          map_body_uri
            ~not_relative:(fun _ -> XHTML.M.img ~src:(uri_of_string uri) ~alt ())
            ~relative:(fun p f ->
                         XHTML.M.img
                           ~src:(abs_service_uri attachment_service ~sp (p, f))
                           ~alt ())
            ~broken:(fun _ -> pcdata alt)
            uri
      end
      (Node.markup node)
  in Xhtmlcompact_lite.xhtml_list_print ~html_compat:true html

let rec reload_pages () =
  printf "[%s] reloading pages\n%!" (Netdate.mk_mail_date (Unix.gettimeofday ()));
  Pages.refresh pages;
  Lwt_unix.sleep !refresh_period >>= fun () -> reload_pages ()

let init x = ignore !!x

let () =
  Eliom_services.register_eliom_module "ocsiblog"
    (fun () -> init page_service;
               init attachment_service;
               init toplevel_service;
               init rss1_service;
               init rss2_service;
               init dummy_comment_service;
               init post_comment_service;
               ignore (reload_pages ()))
