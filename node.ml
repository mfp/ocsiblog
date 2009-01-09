(* Copyright (C) 2009 Mauricio Fernandez <mfp@acm.org> *)
open ExtString
open Printf

module S = Set.Make(String)

type entry = {
  name : string;
  title : string;
  date : float;
  markup : Simple_markup.paragraph list;
  mutable html : Simple_markup__html.html_output XHTML.M.elt list option;
  deps : string list Lazy.t;
  tags : string list;
  syndicated : bool;
  allow_comments : bool;
}

type sort_criterion = [`Date | `Title]

let assoc_default default key l =
  try List.assoc key l with Not_found -> default ()

let parse_date s = Netdate.since_epoch (Netdate.parse s)

let split_headers_body s =
  let rec loop headers = function
      [] -> (headers, [])
    | l::ls -> begin match (try Some (String.split l ":") with _ -> None) with
          Some (k, v) -> loop ((String.strip k, String.strip v) :: headers) ls
        | None -> (headers, (l :: ls))
      end
  in loop [] (Str.split_delim (Str.regexp "\n") s)

let inner_link_re = Str.regexp "^[A-Za-z0-9_-]+$"
let is_inner_link s = Str.string_match inner_link_re s 0

let parse_relative_url s = match String.nsplit s "/" with
    [page; file] -> Some (page, file)
  | _ -> None


class depfinder =
object(self)
  inherit Simple_markup.fold as super
  val deps = S.empty

  method href h =
    let uri = h.Simple_markup.href_target in
      if is_inner_link uri then {< deps = S.add uri deps >}
      else match parse_relative_url uri with
          Some (page, _) -> {< deps = S.add page deps >}
        | None -> self

  method img i = match parse_relative_url i.Simple_markup.img_src with
      None -> self
    | Some (page, _) -> {< deps = S.add page deps >}

  method deps = deps
end

let (@@) f x = f x
let set_keys s = S.fold (fun k l -> k :: l) s []

let find_deps ps =
  let s =
    (List.fold_left (fun o par -> o#paragraph par) (new depfinder) ps)#deps
  in set_keys s

let make ~name ~file =
  let text = Std.input_file ~bin:true file in
  let headers, body_lines = split_headers_body text in
  let lookup k = List.assoc k headers in
  let bool default k = try bool_of_string (lookup k) with _ -> default in
  let markup = Simple_markup.parse_lines body_lines in
    {
      name = name;
      title = assoc_default (fun _ -> Filename.basename file) "title" headers;
      date = (try parse_date (lookup "date")
              with _ -> (Unix.stat file).Unix.st_ctime);
      markup = markup;
      html = None;
      deps = lazy (find_deps markup);
      tags = (try String.nsplit (lookup "tags") " " with _ -> []);
      syndicated = bool false "syndicate";
      allow_comments = bool false "allow_comments";
    }

let name e = e.name
let date e = e.date
let markup e = e.markup
let syndicated e = e.syndicated
let title e = e.title
let tags e = e.tags
let deps e = Lazy.force e.deps
let allow_comments e = e.allow_comments

let signal_deps_changed e =
  e.html <- None

let get_html f e = match e.html with
    None ->
      let html = f e.markup in
        e.html <- Some html;
        html
  | Some html -> html

let compare ?secondary crit =
  Catalog.compare_by_criteria
    (function
         `Date -> (fun x y -> compare x.date y.date)
       | `Title -> (fun x y -> compare x.title y.title))
    ?extra:secondary crit
