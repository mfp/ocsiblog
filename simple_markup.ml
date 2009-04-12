(* Copyright (C) 2009 Mauricio Fernandez <mfp@acm.org> *)
open Printf
open ExtString
open ExtList

TYPE_CONV_PATH "Simple_markup"

type ref = { src : string; desc : string }

type paragraph =
    Normal of par_text
  | Pre of string * string option
  | Heading of int * par_text
  | Quote of paragraph list
  | Ulist of paragraph list * paragraph list list
  | Olist of paragraph list * paragraph list list

and par_text = text list

and text =
    Text of string
  | Emph of string
  | Bold of string
  | Struck of par_text
  | Code of string
  | Link of href
  | Anchor of string
  | Image of img_ref

and href = { href_target : string; href_desc : string; }

and img_ref = { img_src : string; img_alt : string; }

and par_list = paragraph list with sexp

class fold = Camlp4Filters.GenerateFold.generated

type parse_state = { max : int; current : Buffer.t; fragments : text list; }

let string_of_paragraph p = Sexplib.Sexp.to_string_hum (sexp_of_paragraph p)
let string_of_paragraphs ps = Sexplib.Sexp.to_string_hum (sexp_of_par_list ps)

let indentation ?(ts=8) s =
  let rec loop n indent max =
    if n >= max then indent
    else match s.[n] with
        ' ' -> loop (n + 1) (indent + 1) max
      | _ -> indent
  in loop 0 0 (String.length s)

let unescape s =
  let b = Buffer.create (String.length s) in
  let len = String.length s in
  let rec loop i =
    if i >= len then Buffer.contents b
    else match s.[i] with
        '\\' when i < len - 1 -> Buffer.add_char b s.[i+1]; loop (i + 2)
      | c -> Buffer.add_char b c; loop (i + 1)
  in loop 0

let unescape_slice s ~first ~last =
  unescape (String.strip (String.slice ~first ~last s))

let snd_is_space s = String.length s > 1 && s.[1] = ' '
let ulist_re = Str.regexp "^[*+-]+[ ]+"
let olist_re = Str.regexp "^[0-9]+\\.[ ]+"
let hlevel = function '=' -> 1 | '-' -> 2 | _ -> invalid_arg "hlevel"

let matches_re re s = Str.string_match re s 0
let match_end re s = if matches_re re s then Some (Str.match_end ()) else None

let is_olist, olist_offset = matches_re olist_re, match_end olist_re
let is_ulist, ulist_offset = matches_re ulist_re, match_end ulist_re

let collect f x =
  let rec loop acc = match f x with
      None -> List.rev acc
    | Some y -> loop (y :: acc)
  in loop []

let push_remainder ?(first=2) indent s e =
  let s = String.slice ~first s in
  let s' = String.strip s in
    Enum.push e (indent + first + indentation s, s', s' = "")

let new_fragment () = Buffer.create 8
let adds = Buffer.add_string
let addc = Buffer.add_char

let push_current st =
  if Buffer.length st.current > 0 then
    Text (Buffer.contents st.current) :: st.fragments
  else st.fragments

let rec read_paragraph indent ?(prev = indent) e = match Enum.peek e with
    None -> None
  | Some (indentation, line, isblank) -> match isblank with
        true -> Enum.junk e; read_paragraph indent ~prev e
      | false ->
          if indentation < indent then
            None
          else begin
            Enum.junk e;
            read_nonempty ~prev indentation e line
          end

and skip_blank_line e = match Enum.peek e with
    None | Some (_, _, false) -> ()
  | Some (_, _, true) -> Enum.junk e; skip_blank_line e

and read_nonempty ~prev indent e s = match s.[0] with
  | _ when indent >= prev + 4 ->
      Enum.push e (indent, s, false); read_pre prev e
  | '#' -> read_heading s
  | _ when is_ulist s -> push_remainder indent s e; read_ul indent e
  | _ when is_olist s -> push_remainder indent s e; read_ol indent e
  | '>' when snd_is_space s || s = ">" ->
      (* last check needed because "> " becomes ">" *)
      Enum.push e (indent, s, false); read_quote indent e
  | _ -> match Enum.peek e with
      | Some (_, l, _) when is_all l '=' || is_all l '-' ->
          Enum.junk e; Some (Heading (hlevel l.[0], parse_text s))
      | _ -> Enum.push e (indent, s, false); read_normal prev e

and is_all s c =
  let rec loop s c n max =
    if n >= max then true else if c = s.[n] then loop s c (n + 1) max else false
  in String.length s > 0 && loop s c 0 (String.length s)

and read_heading s =
  let s' = String.strip ~chars:"#" s in
  let level = String.length s - String.length s' in
    Some (Heading (level, parse_text s'))

and read_ul indent e = read_list (fun f o -> Ulist (f, o)) ulist_offset indent e

and read_ol indent e = read_list (fun f o -> Olist (f, o)) olist_offset indent e

and read_list f item_indent indent e =
  let read_item ?prev indent ps = collect (read_paragraph indent ?prev) e in
  let rec read_all fst others =
    skip_blank_line e;
    match Enum.peek e with
      | Some (indent', s, _) when indent' >= indent ->
          (match item_indent s with
              None -> f fst (List.rev others)
            | Some n -> Enum.junk e;
                        push_remainder ~first:n indent' s e;
                        read_all fst
                          (read_item ~prev:(indent' + n) (indent' + 1) [] :: others))
      | None | Some _ -> f fst (List.rev others)
  in Some (read_all (read_item ~prev:10000 (indent + 1) []) [])

and read_pre prev e =
  let rec loop lines e = match Enum.peek e with
      Some (indentation, s, _) when indentation >= prev + 4 ->
        Enum.junk e;
        loop ((String.make (indentation - prev - 4) ' ' ^ s) :: lines) e
    | _ -> Some (Pre (String.concat "\n" (List.rev ("" :: lines)), None))
  in loop [] e

and read_quote indent e =
  let push_and_finish e elm = Enum.push e elm; raise Enum.No_more_elements in
  let next_without_lt e = function
    | (_, _, true) as line -> push_and_finish e line
    | (n, s, false) as line ->
        if n < indent || s.[0] <> '>' then
          push_and_finish e line
        else
          let s = String.slice ~first:1 s in
          let s' = String.strip s in
            (String.length s - String.length s', s', s' = "")

  in match collect (read_paragraph 0) (Enum.map (next_without_lt e) e) with
      [] -> None
    | ps -> Some (Quote ps)

and read_normal prev e =
  let rec gettxt prev ls =
    let return () = String.concat " " (List.rev ls) in
    match Enum.peek e with
        None | Some (_, _, true) -> return ()
      | Some (indent, l, _) -> match l.[0] with
          | _ when indent >= prev + 4 -> return ()
          | '#' | '>' when snd_is_space l -> return ()
          | _ when (is_olist l || is_ulist l) -> return ()
          | _ -> Enum.junk e; gettxt indent (l :: ls)
  in Some (Normal (parse_text (gettxt prev [])))

and parse_text s =
  scan s { max = String.length s; fragments = []; current = new_fragment (); } 0

  (* scan s starting from n, upto max (exclusive) *)
and scan s st n =
  let max = st.max in
  let delim f d s st n =
    delimited (fun fst lst -> f (unescape_slice s fst lst)) d s st n in
  if n >= max then List.rev (push_current st)

  else match s.[n] with
    | '`' -> delim (fun s -> Code s) "`" s st n
    | '*' when n + 1 < max && s.[n+1] = '*' -> delim (fun s -> Bold s) "**" s st n
    | '_' when n + 1 < max && s.[n+1] = '_' -> delim (fun s -> Bold s) "__" s st n
    | '*' | '_' as d -> delim (fun s -> Emph s) (String.make 1 d) s st n
    | '=' ->
        delimited
          (fun fst lst ->
             Struck (scan s
                       { max = lst; fragments = []; current = new_fragment (); } fst))
          "==" s st n
    | '!' when matches_at s ~max n "![" ->
        maybe_link
          "![" (fun ref -> Image { img_src = ref.src; img_alt = ref.desc })
          s st (n + 2)
    | '[' ->
        maybe_link "["
          (fun ref -> match ref.src, ref.desc with
               "", "" -> Text ""
             | "", desc -> Link { href_target = desc; href_desc = desc }
             | src, "" when src.[0] = '#' -> Anchor (String.slice ~first:1 src)
             | src, desc -> Link { href_target = ref.src; href_desc = ref.desc})
          s st (n + 1)
    | '\\' when (n + 1) < max -> addc st.current s.[n+1]; scan s st (n + 2)
    | c -> addc st.current c; scan s st (n + 1)

(* [delimited f delim first] tries to match [delim] starting from [first],
 * returns Some (offset of char after closing delim) or None *)
and delimited f delim s st first =
  let delim_len = String.length delim in
  let scan_from_next_char () =
    addc st.current s.[first];
    scan s st (first + 1)
  in
    if not (matches_at s ~max:st.max first delim) then scan_from_next_char ()
    else match scan_past ~delim s ~max:st.max (first + String.length delim) with
        Some n ->
          let chunk = f (first + delim_len) (n - String.length delim)
          in scan s
               { st with fragments = chunk :: push_current st;
                         current = new_fragment () }
               n
      | None -> scan_from_next_char ()

and maybe_link delim f s st n = match scan_link s ~max:st.max n with
    None -> adds st.current delim; scan s st n
  | Some (ref, n) ->
      scan s
        { st with fragments = f ref :: push_current st;
                  current = (new_fragment ()) }
        n

(* return None if delim not found, else Some (offset of char *after* delim) *)
and scan_past ~delim s ~max n =
  let re = Str.regexp (Str.quote delim) in
  let rec loop m ~max =
    if m >= max then None else
      match (try Some (Str.search_forward re s m) with Not_found -> None) with
        | Some m when m < max && s.[m-1] <> '\\' -> Some (m + String.length delim)
        | Some m when m < max -> loop (m + 1) ~max
        | _ -> None (* no match or >= max  *)
  in loop n ~max

(* returns None or offset of char after the reference
 * (i.e. after closing ')'). *)
and scan_link s ~max n = match scan_past ~delim:"]" s ~max n with
    None -> None
  | Some end_of_desc ->
      if end_of_desc >= max then None
      else match s.[end_of_desc] with
          '(' ->
            begin match scan_past ~delim:")" s ~max (end_of_desc + 1) with
                None -> None
              | Some end_of_uri ->
                  let ref =
                    {
                      desc = unescape_slice s ~first:n ~last:(end_of_desc - 1);
                      src = unescape_slice s
                              ~first:(end_of_desc + 1)
                              ~last:(end_of_uri - 1)
                    }
                  in Some (ref, end_of_uri)
            end
        | _ -> None

and matches_at s ~max n delim =
  let len = String.length delim in
    if n + len > max then false
    else
      let rec loop n m k =
        if k = 0 then true
        else if s.[n] = delim.[m] then loop (n + 1) (m + 1) (k - 1)
        else false
      in loop n 0 len

let expand_tabs s =
  let len = String.length s in
  let b = Buffer.create (len + 8) in
  let n = ref 0 in
    for i = 0 to len - 1 do
      match s.[i] with
          '\t' -> let l = 8 - (!n mod 8) in adds b (String.make l ' '); n := !n + l
        | c -> incr n; addc b c
    done;
    Buffer.contents b

let parse_enum e =
  collect (read_paragraph 0)
    (Enum.map (fun l -> let l' = String.strip l in (indentation l, l', l' = ""))
       (Enum.map expand_tabs e))

let parse_lines ls = parse_enum (List.enum ls)
let parse_text s = parse_lines ((Str.split (Str.regexp "\n") s))
