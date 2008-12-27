open ExtString
open ExtList

type comment = {
  c_id : string;
  c_author : string;
  c_date : float;
  c_markup : Simple_markup.paragraph list;
}

type sort_criterion = [`Date]

module M = Map.Make(String)
module S = Set.Make(struct
                      type t = comment
                      let compare a b = compare a.c_id b.c_id
                    end)

type t = {
  basedir : string;
  mutable comments : S.t M.t;
  mutable last_read : float M.t;
  refresh_period : float;
}

let compare = function
    `Date -> (fun x y -> compare x.c_date y.c_date)

let (/^) = Filename.concat

let read_comment file =
  try
    let hs, body = Node.split_headers_body (Std.input_file ~bin:true file) in
      Some {
        c_id = Filename.basename file;
        c_author = List.assoc "author" hs;
        c_date = Netdate.since_epoch (Netdate.parse (List.assoc "date" hs));
        c_markup = Simple_markup.parse_text body;
      }
  with _ -> None

let refresh_comments t page =
  print_endline ("refreshing comments for " ^ page);
  let list_comments dir =
    Catalog.dir_filter_map
      (fun f ->
         let fname = dir /^ f in
           match (Unix.stat fname).Unix.st_kind with
               Unix.S_REG -> Some fname
             | _ -> None)
      dir
  in
    try
      let comment_files = list_comments (t.basedir /^ page) in
      let comments = List.filter_map read_comment comment_files in
      let s = List.fold_left (fun s c -> S.add c s) S.empty comments in
        t.comments <- M.add page s t.comments;
        t.last_read <- M.add page (Unix.gettimeofday ()) t.last_read
    with _ -> ()


let make ?(refresh_period = 60.) dir =
  {
    basedir = dir; comments = M.empty;
    last_read = M.empty; refresh_period = refresh_period;
  }

let find_default default k m = try M.find k m with Not_found -> default

let get_comments t page =
  try
    let last_read = find_default 0. page t.last_read in
      if Unix.gettimeofday () > last_read +. t.refresh_period then
        refresh_comments t page;
      Some (S.elements (M.find page t.comments))
  with _ -> None

let is_directory path =
  try match (Unix.stat path).Unix.st_kind with
      Unix.S_DIR -> true
    | _ -> false
  with _ -> false

let rec mkdir_p ?(perms = 0o750) path =
  if path.[0] <> '/' then
    mkdir_p ~perms (String.concat "/" [Unix.getcwd(); path])
  else try
    (* common case first *)
    Unix.mkdir path perms
  with Unix.Unix_error _ ->
    if not (is_directory path) then begin
      let rec paths p l =
        if p = "/" then l else paths (Filename.dirname p) (p :: l)
      in
        List.iter
          (fun p ->
             try
               Unix.mkdir p perms
             with Unix.Unix_error _ as e ->
               if not (is_directory p) then raise e)
          (paths path [])
    end

let get_set_elm x s = S.choose (S.diff s (S.diff s (S.singleton x)))

let add_comment t page ~author ?(date = Unix.gettimeofday ()) ~body () =
  let author = match String.strip author with
      "" -> "anonymous"
    | s -> (try fst (String.split s "\n") with _ -> s) in
  let basename =
    Digest.to_hex (Digest.string (String.concat "\n" [author; body])) in
  let page_comments = find_default S.empty page t.comments in
  let c =
    {
      c_id = basename;
      c_author = author;
      c_date = date;
      c_markup = Simple_markup.parse_text body;
    }
  in
    if S.mem c page_comments then
      get_set_elm c page_comments
    else
      let dir = t.basedir /^ page in
        mkdir_p dir;
        let fname = dir /^ basename in
        let io = IO.output_channel (open_out_bin fname) in
        let comments = M.add page (S.add c page_comments) t.comments in
          Std.finally
            (fun () -> IO.close_out io)
            (fun io ->
               IO.printf io
                 "date: %s\nauthor: %s\n\n" (Netdate.mk_mail_date date) author;
               IO.nwrite io body)
            io;
          t.comments <- comments;
          c
