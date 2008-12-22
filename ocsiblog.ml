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
let toplevel_title = ref "eigenclass"
let toplevel_pages = ref 5
let toplevel_links = ref 10
let refresh_period = ref 10.
let rss_title = ref "eigenclass"
let rss_link = ref "http://eigenclass.org"
let rss_description = ref !rss_title
let rss_nitems = ref 10

let pages = Pages.make !pagedir

let not_found () = raise Eliom_common.Eliom_404

let attachment_file page basename =
  String.join "/" [!pagedir; page ^ ".files"; basename]

let page_with_title thetitle thebody =
  return (html
            (head (title (pcdata thetitle)) [])
            (body thebody))

let div_with_class klass ?(a = []) l = div ~a:(a_class [klass] :: a) l

let maybe_ul ?a = function
    [] -> pcdata ""
  | hd::tl -> ul ?a hd tl

let format_date t = Netdate.mk_mail_date t

let render_pre _ ~kind txt = pre [pcdata txt]

let absolute_service_link ~service ~sp desc params =
  XHTML.M.a
    ~a:[a_href (make_full_uri ~sp ~service:(Lazy.force service) params)]
    desc

let rec render_link_aux ~link_attachment ~link_page href =
  let uri = href.SM.href_target in
  let desc = pcdata href.SM.href_desc in
    if Node.is_inner_link uri then begin
      if Pages.has_entry pages uri then
        link_page [desc] uri
      else
        desc
    end else
      try
        let url = Neturl.parse_url uri in
          XHTML.M.a
            ~a:[a_href (uri_of_string (Neturl.string_of_url url))] [desc]
      with Neturl.Malformed_URL -> (* a relative URL, basic verification *)
        match String.nsplit uri "/" with
            [ page; file ] when Pages.has_entry pages page ->
              link_attachment [desc] (page, file)
          | _ -> (* broken relative link *) desc

and render_link sp href =
  render_link_aux
    ~link_page:(a ~service:(Lazy.force page_service) ~sp)
    ~link_attachment:(a ~service:(Lazy.force attachment_service) ~sp)
    href

and render_img sp img =
  XHTML.M.img ~src:(uri_of_string img.SM.img_src) ~alt:img.SM.img_alt ()

and render_node sp =
  Simple_markup__html.to_html
    ~render_pre:(render_pre sp)
    ~render_link:(render_link sp)
    ~render_img:(render_img sp)

and page_service = lazy begin
  register_new_service
    ~path:[""]
    ~get_params:(suffix (string "page"))
    (fun sp page () -> match Pages.get_entry pages page with
         None -> not_found ()
       | Some node ->
           let thetitle = Node.title node in
           let body_html = Node.get_html (render_node sp) node in
             page_with_title thetitle ((h1 [pcdata thetitle]) :: body_html))
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
            [div_with_class "entries" (List.map (entry_div sp) pages);
             div_with_class "sidebar"
               [maybe_ul (List.map (entry_link sp) (List.take !toplevel_links all))]])
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
                make_full_string_uri ~sp ~service:(Lazy.force page_service)
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
         XML.decl ~version:"1.0" ~encoding:"UTF-8" add ();
         XML.output add xml;
         return (Buffer.contents b, "text/xml"))
end

and entry_div sp node =
  div [
    h2 ~a:[a_class ["entry_title"]]
      [span ~a:[a_class ["date"]] [pcdata (format_date (Node.date node))];
       pcdata " ";
       span ~a:[a_class ["title"]] [link_to_node sp node]];

    div_with_class "entry_body" (Node.get_html (render_node sp) node)]

and entry_link sp node = li [ link_to_node sp node ]

and link_to_node sp node =
  a ~service:(Lazy.force page_service) ~sp
    [pcdata (Node.title node)] (Node.name node)

and render_node_for_rss ~sp node =
  let html =
    Simple_markup__html.to_html
      ~render_pre:(render_pre ())
      ~render_link:begin
        render_link_aux
          ~link_attachment:(absolute_service_link ~service:attachment_service ~sp)
          ~link_page:(absolute_service_link ~service:page_service ~sp)
      end
      ~render_img:begin fun img ->
        let uri = img.SM.img_src and alt = img.SM.img_alt in
          try
          let _ = Neturl.parse_url uri in
            XHTML.M.img ~src:(uri_of_string uri) ~alt ()
          with Neturl.Malformed_URL -> (* a relative URL, basic verification *)
            match String.nsplit uri "/" with
                [ page; file ] when Pages.has_entry pages page ->
                  XHTML.M.img
                    ~src:(make_full_uri ~sp ~service:(Lazy.force attachment_service)
                            (page, file))
                    ~alt ()
             | _ -> pcdata alt
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

let init x = ignore (Lazy.force x)

let () =
  Eliom_services.register_eliom_module "ocsiblog"
    (fun () -> init page_service;
               init attachment_service;
               init toplevel_service;
               init rss2_service;
               ignore (reload_pages ()))
