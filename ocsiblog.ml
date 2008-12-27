open Printf
open Eliom_parameters
open Lwt
open XHTML.M
open Eliom_predefmod.Xhtml
open ExtString
open ExtList

module Pages = Catalog.Make(Node)
module SM = Simple_markup

let pagedir = ref "pages"
let commentdir = ref "comments"
let toplevel_title = ref "eigenclass"
let toplevel_pages = ref 5
let toplevel_links = ref 10
let refresh_period = ref 10.
let rss_title = ref "eigenclass"
let rss_link = ref "http://eigenclass.org"
let rss_description = ref !rss_title
let rss_nitems = ref 10
let encoding = ref "UTF-8"

let pages = Pages.make !pagedir
let comments = Comments.make !commentdir

let not_found () = raise Eliom_common.Eliom_404

let force = Lazy.force

let attachment_file page basename =
  String.join "/" [!pagedir; page ^ ".files"; basename]

let with_class f ?(a = []) klass = f ?a:(Some (a_class [klass] :: a))

let div_with_class klass ?(a = []) l = div ~a:(a_class [klass] :: a) l

let anchor ?title ~name contents =
  let title = Option.map_default (fun t -> [a_title t]) [] title in
    XHTML.M.a ~a:(a_href (uri_of_string ("#" ^ name)) :: title) [pcdata contents]

let maybe_ul ?a = function
    [] -> pcdata ""
  | hd::tl -> ul ?a hd tl

let format_date t = Netdate.mk_mail_date t

let page_with_title thetitle thebody =
  return (html
            (head (title (pcdata thetitle))
               [css_link (uri_of_string "/blog/ocsiblog.css") ()])
            (body thebody))

let render_pre _ ~kind txt = pre [pcdata txt]

let absolute_service_link service ~sp desc params =
  XHTML.M.a
    ~a:[a_href (make_full_uri ~sp ~service:(force service) params)]
    desc

let map_uri ~relative ~broken ~not_relative uri =
  try
    let url = Neturl.parse_url uri in
      not_relative url
  with Neturl.Malformed_URL -> (* a relative URL, basic verification *)
    match Str.split_delim (Str.regexp "/") uri with
        [ page; file ] when file <> "" && Pages.has_entry pages page ->
          relative page file
      | _ -> (* broken relative link *) broken uri

let rec render_link_aux ~link_attachment ~link_page href =
  let uri = href.SM.href_target in
  let desc = pcdata href.SM.href_desc in
    if Node.is_inner_link uri then begin
      if Pages.has_entry pages uri then link_page [desc] uri
      else desc
    end else
      map_uri
        ~not_relative:(fun _ -> XHTML.M.a ~a:[a_href (uri_of_string uri)] [desc])
        ~relative:(fun page file -> link_attachment [desc] (page, file))
        ~broken:(fun _ -> desc)
        uri

and render_link sp href =
  render_link_aux
    ~link_page:(a ~service:(force page_service) ~sp)
    ~link_attachment:(a ~service:(force attachment_service) ~sp)
    href

and render_img sp img =
  XHTML.M.img
    ~a:[a_class ["centered"]]
    ~src:(uri_of_string img.SM.img_src) ~alt:img.SM.img_alt ()

and render_node sp =
  Simple_markup__html.to_html
    ~render_pre:(render_pre sp)
    ~render_link:(render_link sp)
    ~render_img:(render_img sp)

and serve_page sp page () = match Pages.get_entry pages page with
    None -> not_found ()
  | Some node ->
      let thetitle = Node.title node in
        page_with_title thetitle
          ((h1 [pcdata thetitle]) :: node_body_with_comments ~sp node)

and page_service = lazy begin
  register_new_service
    ~path:[""]
    ~get_params:(suffix (string "page"))
    serve_page
end

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
  register_new_service
    ~path:[""]
    ~get_params:unit
    (fun sp () () ->
       let all = Pages.sorted_entries ~reverse:true `Date pages in
       let pages = List.take !toplevel_pages (List.filter Node.syndicated all)
       in page_with_title
            !toplevel_title
            [div_with_class "main" (List.map (entry_div sp) pages);
             div_with_class "sidebar"
               [maybe_ul (List.map (entry_link sp) (List.take !toplevel_links all))]])
end

and dummy_comment_service = lazy begin
  Eliom_predefmod.Redirection.register_new_service
    ~path:[""]
    ~get_params:(string "page")
    (fun sp page () ->
       return (Eliom_services.preapply (force page_service) page))
end

and post_comment_service = lazy begin
  Eliom_predefmod.String_redirection.register_new_post_service
    ~options:`Permanent
    ~fallback:(force dummy_comment_service)
    ~post_params:(string "author" ** string "body")
    (fun sp page (author, body) ->
       let uri = make_full_string_uri ~sp ~service:(force page_service) page in
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
       let tags = String.nsplit taglist "," in
       let all = Pages.sorted_entries ~reverse:true `Date pages in
       let tags_match node =
         tags = [] || List.exists (fun t -> List.mem t tags) (Node.tags node) in
       let nodes =
         List.take !rss_nitems
           (List.filter (fun n -> Node.syndicated n && tags_match n) all) in
       let items =
         List.map
           (fun node ->
              let link =
                make_full_string_uri ~sp ~service:(force page_service)
                  (Node.name node)
              in Rss.make_item ~title:(Node.title node) ~link
                ~pubDate:(Node.date node) ~guid:(link, true)
                ~description:(render_node_for_rss ~sp node) ())
           nodes in
       let xml = Rss.make
                   ~title:!rss_title
                   ~link:!rss_link
                   ~description:!rss_description
                   ~ttl:180
                   items in
       let b = Buffer.create 256 in
       let add = Buffer.add_string b in
         XML.decl ~version:"1.0" ~encoding:!encoding add ();
         XML.output add xml;
         return (Buffer.contents b, "text/xml"))
end

and node_body_with_comments ~sp node =
  let page = Node.name node in
  let allow_comments = Node.allow_comments node in
  let body = Node.get_html (render_node sp) node in
  let cs = Option.default [] (Comments.get_comments comments page) in
  let comment_form = match allow_comments with
      true -> [ post_form (force post_comment_service) sp comment_form page ]
    | false -> [] in
  let comments_div = match cs, allow_comments with
      [], false -> []
    | _ -> [ div_with_class "comments"
             (h2 [pcdata "Comments"] :: format_comments ~sp cs) ]
  in
    List.concat
      [ body;
        [ hr ();
          div_with_class "article_date" [pcdata (format_date (Node.date node))] ];
        comments_div; comment_form ]

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
               [pcdata (format_date c.Comments.c_date) ];
             anchor ~title:"Permanent link to this comment" ~name:comm_id "#" ]]]

and comment_id c = "comment-" ^ c.Comments.c_id

and render_comment_body sp =
  Simple_markup__html.to_html
    ~render_pre:(render_pre ())
    ~render_img:(fun img ->
                   XHTML.M.a ~a:[a_href (uri_of_string img.SM.img_src)]
                     [pcdata img.SM.img_alt])
    ~render_link:(render_link sp)

and comment_form (author_name, body_name) =
  [p [ pcdata "Author:"; br ();
       string_input ~input_type:`Text ~name:author_name (); br ();
       pcdata "Comment:"; br ();
       textarea ~name:body_name ~rows:10 ~cols:82 (); br ();
       string_input ~input_type:`Submit ~value:"Click" () ]]

and entry_div sp node =
  div_with_class "entry"
    [h2 ~a:[a_class ["entry_title"]]
       [span ~a:[a_class ["date"]] [pcdata (format_date (Node.date node))];
        pcdata " ";
        span ~a:[a_class ["title"]] [link_to_node sp node]];
     div_with_class "entry_body" (Node.get_html (render_node sp) node)]

and entry_link sp node = li [ link_to_node sp node ]

and link_to_node sp node =
  a ~service:(force page_service) ~sp
    [pcdata (Node.title node)] (Node.name node)

and render_node_for_rss ~sp node =
  let html =
    Simple_markup__html.to_html
      ~render_pre:(render_pre ())
      ~render_link:begin
        render_link_aux
          ~link_attachment:(absolute_service_link attachment_service ~sp)
          ~link_page:(absolute_service_link page_service ~sp)
      end
      ~render_img:begin fun img ->
        let uri = img.SM.img_src and alt = img.SM.img_alt in
          map_uri
            ~not_relative:(fun _ -> XHTML.M.img ~src:(uri_of_string uri) ~alt ())
            ~relative:(fun p f ->
                         XHTML.M.img
                           ~src:(make_full_uri ~sp
                                   ~service:(force attachment_service) (p, f))
                           ~alt ())
            ~broken:(fun _ -> pcdata alt)
            uri
      end
      (Node.markup node) in
  let b = Buffer.create 13 in
    List.iter (XML.output (Buffer.add_string b)) (XHTML.M.toeltl html);
    Buffer.contents b

let rec reload_pages () =
  printf "[%s] reloading pages\n%!"
    (Netdate.mk_mail_date (Unix.gettimeofday ()));
  Pages.refresh pages;
  Lwt_unix.sleep !refresh_period >>= fun () -> reload_pages ()

let init x = ignore (force x)

let () =
  Eliom_services.register_eliom_module "ocsiblog"
    (fun () -> init page_service;
               init attachment_service;
               init toplevel_service;
               init rss2_service;
               init dummy_comment_service;
               init post_comment_service;
               ignore (reload_pages ()))
