open Printf
open ExtString
open ExtList

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

and href = {
  href_target : string;
  href_desc : string;
}

and img_ref = {
  img_src : string;
  img_alt : string;
}

class fold = Camlp4Filters.GenerateFold.generated

type parse_state = {
  max : int;
  current : Buffer.t;
  fragments : text list;
}

module PP =
struct
  open Format

  let rec fprintfs sep f pp = function
      [] -> ()
    | [x] -> fprintf pp "%a" f x
    | x::l -> fprintf pp ("%a" ^^ sep ^^ "%a") f x (fprintfs sep f) l

  let pp_list f pp l =
    let pr_elems = fprintfs ";@ " f in
      fprintf pp "[@[<1>@ %a@ @]]" pr_elems l

  let pp_option f pp = function
      None -> fprintf pp "None"
    | Some x -> fprintf pp "Some %a" f x

  let pp_string fmt s = fprintf fmt "%S" s

  let rec pp_paragraph fmt = function
      Normal text -> fprintf fmt "@[<2>Normal@ %a@]" pp_par_text text
    | Pre (s, kind) -> fprintf fmt "@[<2>Pre@ (@[%S,@ %a)@]@]"
                         s (pp_option pp_string) kind
    | Heading (lev, text) -> fprintf fmt "@[<2>Heading@ (@[%d,@ %a)@]@]"
                               lev pp_par_text text
    | Quote ps -> fprintf fmt "@[<2>Quote@ %a@]" (pp_list pp_paragraph) ps
    | Ulist (l, ls) ->
        fprintf fmt "@[<2>Ulist@ (@[%a,@ %a)@]@]"
          (pp_list pp_paragraph) l (pp_list (pp_list pp_paragraph)) ls
    | Olist (l, ls) ->
        fprintf fmt "@[<2>Olist@ (@[%a,@ %a)@]@]"
          (pp_list pp_paragraph) l (pp_list (pp_list pp_paragraph)) ls

  and pp_par_text fmt = pp_list pp_text fmt

  and pp_text fmt = function
      Text s -> fprintf fmt "@[<2>Text@ %S@]" s
    | Emph s -> fprintf fmt "@[<2>Emph@ %S@]" s
    | Bold s -> fprintf fmt "@[<2>Bold@ %S@]" s
    | Code s -> fprintf fmt "@[<2>Code@ %S@]" s
    | Struck txt -> fprintf fmt "@[<2>Struck@ %a@]" pp_par_text txt
    | Anchor s -> fprintf fmt "@[<2>Anchor@ %S@]" s
    | Link href ->
        fprintf fmt "@[<2>Link@ {@[target=%S,@ %S}@]@]" href.href_target href.href_desc
    | Image img ->
        fprintf fmt "@[<2>Image@ {@[src=%S,@ %S}@]@]" img.img_src img.img_alt

  let pp f x =
    let b = Buffer.create 16 in
    let pp = formatter_of_buffer b in
      fprintf pp "%a@?" f x;
      Buffer.contents b;;
end

let string_of_paragraph = PP.pp PP.pp_paragraph
let string_of_paragraphs = PP.pp (PP.pp_list PP.pp_paragraph)

let indentation ?(ts=8) s =
  let rec loop n indent max =
    if n >= max then indent
    else match s.[n] with
        ' ' -> loop (n + 1) (indent + 1) max
      | '\t' -> loop (n + 1) (indent + 8) max
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

let snd_is s c = String.length s > 1 && s.[1] = c
let snd_is_space s = snd_is s ' ' || snd_is s '\t'

let collect f x =
  let rec loop acc = match f x with
      None -> List.rev acc
    | Some y -> loop (y :: acc)
  in loop []

let push_remainder ?(first=2) indent s e =
  let s = String.slice ~first s in
  let s' = String.strip s in
    Enum.push e (indent + first + indentation s, s', s' = "")

let adds = Buffer.add_string

let addc = Buffer.add_char

let new_fragment () = Buffer.create 8

let push fragment fragments =
  if Buffer.length fragment > 0 then
    Text (Buffer.contents fragment) :: fragments
  else fragments

let rec read_paragraph ?(skip_blank=true) indent e = match Enum.peek e with
    None -> None
  | Some (indentation, line, isblank) -> match isblank with
        true ->
          Enum.junk e;
          if skip_blank then read_paragraph indent e else None
      | false ->
          if indentation < indent then
            None
          else begin
            Enum.junk e;
            read_nonempty indentation e line
          end

and skip_blank_line e = match Enum.peek e with
    None | Some (_, _, false) -> ()
  | Some (_, _, true) -> Enum.junk e; skip_blank_line e

and read_nonempty indent e s = match s.[0] with
    '!' -> read_heading s
  | '*' when snd_is_space s -> push_remainder indent s e; read_ul indent e
  | '#' when snd_is_space s -> push_remainder indent s e; read_ol indent e
  | '{' when snd_is s '{' -> read_pre (String.slice s ~first:2) e
  | '>' when snd_is_space s || s = ">" ->
      (* last check needed because "> " becomes ">" *)
      Enum.push e (indent, s, false); read_quote indent e
  | _ -> Enum.push e (indent, s, false); read_normal e

and read_heading s =
  let s' = String.strip ~chars:"!" s in
  let level = String.length s - String.length s' in
    Some (Heading (level, parse_text s'))

and read_ul indent e =
  read_list
    (fun fst others -> Ulist (fst, others))
    (fun s -> snd_is_space s && s.[0] = '*')
    indent e

and read_ol indent e =
  read_list
    (fun fst others -> Olist (fst, others))
    (fun s -> snd_is_space s && s.[0] = '#')
    indent e

and read_list f is_item indent e =
  let read_item indent ps = collect (read_paragraph (indent + 1)) e in
  let rec read_all fst others =
    skip_blank_line e;
    match Enum.peek e with
      | Some (indentation, s, _) when indentation >= indent && is_item s ->
          Enum.junk e;
          push_remainder indentation s e;
          read_all fst (read_item indentation [] :: others)
      | None | Some _ -> f fst (List.rev others)
  in Some (read_all (read_item indent []) [])

and read_pre kind e =
  let kind = match kind with "" -> None | s -> Some s in
  (*  don't forget the last \n *)
  let ret ls = Some (Pre (String.concat "\n" (List.rev ("" :: ls)), kind)) in
  let rec read_until_end fstindent ls = match Enum.get e with
      None | Some (_, "}}", _) -> ret ls
    | Some (indentation, s, _) ->
        let spaces = String.make (max 0 (indentation - fstindent)) ' ' in
          read_until_end fstindent ((spaces ^ s) :: ls)
  in match Enum.get e with
      None | Some (_, "}}", _) -> ret []
    | Some (indentation, s, _) -> read_until_end indentation [s]

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

and read_normal e =
  let rec gettxt ls =
    let return () = String.concat " " (List.rev ls) in
    match Enum.peek e with
      None | Some (_, _, true) -> return ()
    | Some (_, l, _) -> match l.[0] with
            '!' | '*' | '#' | '>' when snd_is_space l -> return ()
          | '{' when snd_is l '{' -> return ()
          | _ -> Enum.junk e; gettxt (l :: ls) in
  let txt = gettxt [] in
    Some (Normal (parse_text txt))

and parse_text s =
  scan
    s
    { max = String.length s;
      fragments = [];
      current = new_fragment (); }
    0

  (* scan s starting from n, upto max (exclusive) *)
and scan s st n =
  let max = st.max in
  if n >= max then List.rev (push st.current st.fragments)

  else match s.[n] with
    | '`' ->
        delimited (fun ~first ~last -> Code (unescape_slice s ~first ~last)) "`"
          s st n
    | '*' ->
        delimited (fun ~first ~last -> Bold (unescape_slice s ~first ~last)) "*"
          s st n
    | '_' ->
        delimited (fun ~first ~last -> Emph (unescape_slice s ~first ~last)) "_"
          s st n
    | '=' ->
        delimited
          (fun ~first ~last ->
             Struck (scan s
                       { max = last; fragments = []; current = new_fragment (); }
                       first))
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
  let max = st.max in
  let delim_len = String.length delim in
  let scan_from_next_char () =
    addc st.current s.[first];
    scan s st (first + 1)
  in
    if not (matches_at s ~max first delim) then scan_from_next_char ()
    else match scan_past ~delim s ~max (first + String.length delim) with
        Some n ->
          let chunk = f ~first:(first + delim_len)
                        ~last:(n - String.length delim)
          in scan s
               { st with fragments = chunk :: push st.current st.fragments;
                         current = new_fragment () }
               n
      | None -> scan_from_next_char ()

and maybe_link delim f s st n = match scan_link s ~max:st.max n with
    None -> adds st.current delim; scan s st n
  | Some (ref, n) ->
      scan s
        { st with fragments = f ref :: push st.current st.fragments;
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

let parse_text s =
  let input =
     List.map
       (fun l -> let l' = String.strip l in (indentation l, l', l' = ""))
       (Str.split (Str.regexp "\n") s)
  in collect (read_paragraph 0) (List.enum input)
