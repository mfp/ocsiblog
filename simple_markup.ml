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

let indentation s =
  let rec loop n max =
    if n >= max then n
    else match s.[n] with
        ' ' -> loop (n + 1) max
      | _ -> n
  in loop 0 (String.length s)

let unescape s =
  let b = Buffer.create (String.length s) in
  let len = String.length s in
  let rec loop i =
    if i >= len then Buffer.contents b
    else match s.[i] with
        '\\' when i < len - 1 -> Buffer.add_char b s.[i+1]; loop (i + 2)
      | c -> Buffer.add_char b c; loop (i + 1)
  in loop 0

let snd_is s c = String.length s > 1 && s.[1] = c

let collect f x =
  let rec loop acc = match f x with
      None -> List.rev acc
    | Some y -> loop (y :: acc)
  in loop []

let push_remainder ?(first=2) indent s e =
  let s = String.slice ~first s in
  let s' = String.strip s in
    Enum.push e (indent + first + String.length s' - String.length s, s, s = "")

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
  | '*' when snd_is s ' ' -> push_remainder indent s e; read_ul indent e
  | '#' when snd_is s ' ' -> push_remainder indent s e; read_ol indent e
  | '{' when snd_is s '{' -> read_pre (String.slice s ~first:2) e
  | '>' when snd_is s ' ' || s = ">" ->
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
    (fun s -> String.length s >= 2 && s.[0] = '*' && s.[1] = ' ')
    indent e

and read_ol indent e =
  read_list
    (fun fst others -> Olist (fst, others))
    (fun s -> String.length s >= 2 && s.[0] = '#' && s.[1] = ' ')
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
  let rec read_until_end ls = match Enum.get e with
      None | Some (_, "}}", _) -> (* don't forget the last \n *)
        Some (Pre (String.concat "\n" (List.rev ("" :: ls)), kind))
    | Some (indentation, s, _) ->
        read_until_end ((String.make indentation ' ' ^ s) :: ls)
  in read_until_end []

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
            '!' | '*' | '#' | '>' when snd_is l ' ' -> return ()
          | '{' when snd_is l '{' -> return ()
          | _ -> Enum.junk e; gettxt (l :: ls) in
  let txt = gettxt [] in
    Some (Normal (parse_text txt))

and parse_text s =
  let new_fragment () = Buffer.create 8 in
  let adds = Buffer.add_string in
  let addc = Buffer.add_char in
  let push fragment fragments =
    if Buffer.length fragment > 0 then
      Text (Buffer.contents fragment) :: fragments
    else fragments in

  let unescape_slice ~first ~last =
    unescape (String.strip (String.slice ~first ~last s)) in

  (* scan s starting from n, upto max (exclusive) *)
  let rec scan max fragments current n =

    (* [delimited f delim first] tries to match [delim] starting from [first],
     * returns Some (offset of char after closing delim) or None *)
    let delimited f delim first =
      let delim_len = String.length delim in
      let scan_from_next_char () =
        addc current s.[first];
        scan max fragments current (first + 1)
      in
        if not (matches_at s ~max first delim) then scan_from_next_char ()
        else match scan_past delim ~max (first + String.length delim) with
            Some n ->
              let chunk = f ~first:(first + delim_len)
                            ~last:(n - String.length delim)
              in scan max (chunk :: push current fragments) (new_fragment ()) n
          | None -> scan_from_next_char () in

    let maybe_link delim f n = match scan_link ~max n with
        None -> adds current delim; scan max fragments current n
      | Some (ref, n) ->
          scan max (f ref :: push current fragments) (new_fragment ()) n in

    if n >= max then List.rev (push current fragments)

    else match s.[n] with
      | '*' ->
          delimited (fun ~first ~last -> Bold (unescape_slice ~first ~last)) "*" n
      | '_' ->
          delimited (fun ~first ~last -> Emph (unescape_slice ~first ~last)) "_" n
      | '=' ->
          delimited
            (fun ~first ~last -> Struck (scan last [] (new_fragment ()) first))
            "==" n
      | '!' when matches_at s ~max n "![" ->
          maybe_link "![" 
            (fun ref -> Image { img_src = ref.src; img_alt = ref.desc }) (n + 2)
      | '[' ->
          maybe_link "[" 
            (fun ref -> Link { href_target = ref.src; href_desc = ref.desc}) (n + 1)
      | '\\' when (n + 1) < max ->
          addc current s.[n+1]; scan max fragments current (n + 2)
      | c -> addc current c; scan max fragments current (n + 1)

  (* return None if delim not found, else Some (offset of char *after* delim) *)
  and scan_past delim ~max n =
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
  and scan_link ~max n = match scan_past "]" ~max n with
      None -> None
    | Some end_of_desc ->
        if end_of_desc >= max then None
        else match s.[end_of_desc] with
            '(' ->
              begin match scan_past ")" ~max (end_of_desc + 1) with
                  None -> None
                | Some end_of_uri ->
                    let ref =
                      {
                        desc = unescape_slice ~first:n ~last:(end_of_desc - 1);
                        src = unescape_slice 
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
  in
    scan (String.length s) [] (new_fragment ()) 0

let parse_text s =
  let input =
     List.map
       (fun l -> let l' = String.strip l in (indentation l, l', l' = ""))
       (String.nsplit s "\n")
  in collect (read_paragraph 0) (List.enum input)
