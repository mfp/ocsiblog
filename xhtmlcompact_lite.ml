(* Ocsigen
 * Copyright (C) 2008 Vincent Balat, Mauricio Fernandez
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open XML

module B = Buffer

let mem_func l =
  let h = Hashtbl.create (List.length l) in
    List.iter (fun x -> Hashtbl.add h x true) l;
    fun x -> Hashtbl.mem h x

(* as per XHTML 1.0, appendix C.8; name attr deprecated in 1.1 *)
let need_name = ["a"; "applet"; "form"; "frame"; "iframe"; "img"; "map"]
let needs_id2name = mem_func need_name

let emptytags = ["hr"; "br"; "img"; "meta"; "link"; "input";
                 "col"; "area"; "param"; "base"; "basefont";
                 "isindex"; "frame"]
let is_emptytag = mem_func emptytags

(* FIXME: ugly & unsafe. XML.attrib is abstract, so we copy the definition and
 * use Obj.magic*)
type attrib =
  | AInt of aname * int
  | AStr of aname * string
  | AStrL of separator * aname * string list

let x_print, xh_print =

  let aux b ~encode ?(html_compat = false) doctype arbre =
    let endemptytag = if html_compat then ">" else " />" in
    let rec xh_print_attrs doid2name encode attrs = match attrs with
      [] ->  ();
    | attr::queue ->
        B.add_char b ' ';
        B.add_string b (XML.attrib_to_string encode attr);
        (* FIXME: report to ocsigen devels, need means to operate on attrib *)
        if doid2name then begin match (Obj.magic attr : attrib) with
            AStr ("id", id) ->
              B.add_char b ' ';
              B.add_string b (XML.attrib_to_string encode (string_attrib "name" id))
          | _ -> ()
        end;
        xh_print_attrs doid2name encode queue

    and xh_print_closedtag encode tag attrs =
      if html_compat && not (is_emptytag tag) then begin
        B.add_char b '<';
        B.add_string b tag;
        xh_print_attrs (html_compat && needs_id2name tag) encode attrs;
        B.add_string b "></";
        B.add_string b tag;
        B.add_string b ">"
      end else begin
        B.add_char b '<';
        B.add_string b tag;
        xh_print_attrs (html_compat && needs_id2name tag) encode attrs;
        B.add_string b endemptytag;
      end

    and xh_print_tag encode tag attrs taglist =
      if taglist = []
      then xh_print_closedtag encode tag attrs
      else begin
        B.add_string b ("<" ^ tag);
        xh_print_attrs (html_compat && needs_id2name tag) encode attrs;
        B.add_char b '>';
        xh_print_taglist taglist;
        B.add_string b ("</" ^ tag);
        B.add_char b '>';
      end

    and print_nodes name xh_attrs xh_taglist queue =
      xh_print_tag encode name xh_attrs xh_taglist;
      xh_print_taglist queue

    and xh_print_taglist taglist =
      match taglist with

      | [] -> ()

      | (Comment texte)::queue ->
          (* REVIEW: strip comments? *)
          Buffer.add_string b "<!--";
          Buffer.add_string b (encode texte);
          Buffer.add_string b "-->";
          xh_print_taglist queue;

      | (Entity e)::queue ->
          B.add_string b ("&"^e^";"); (* no encoding *)
          xh_print_taglist queue;

      | (PCDATA texte)::queue ->
          B.add_string b (encode texte);
          xh_print_taglist queue;

      | (EncodedPCDATA texte)::queue ->
          B.add_string b texte;
          xh_print_taglist queue;

          (* Nodes and Leafs *)
      | (Element (name, xh_attrs, xh_taglist))::queue
      | (BlockElement (name, xh_attrs, xh_taglist))::queue
      | (SemiBlockElement (name, xh_attrs, xh_taglist))::queue
      | (Node (name, xh_attrs, xh_taglist))::queue ->
          print_nodes name xh_attrs xh_taglist queue

      | (Leaf (name,xh_attrs))::queue ->
          print_nodes name xh_attrs [] queue

            (* Whitespaces *)
      | (Whitespace(texte))::queue ->
          B.add_string b (encode texte);
          xh_print_taglist queue

      | Empty::queue ->
          xh_print_taglist queue

    in
    xh_print_taglist [arbre]
  in
  ((fun ?header ?(encode = encode_unsafe) ?html_compat doctype foret ->
      let b = B.create 16384 in
        (match header with Some s -> B.add_string b s | None -> ());
        List.iter (aux b ?encode ?html_compat doctype) foret;
        B.contents b),

   (fun ?header ?(encode = encode_unsafe) ?html_compat doctype arbre ->
      let b = B.create 16384 in
         (match header with Some s -> B.add_string b s | None -> ());
         aux b ?encode ?html_compat doctype arbre;
         B.contents b))


let xhtml_print ?(header = "") ?(version=`XHTML_01_01) ?encode ?html_compat arbre =
  xh_print ~header ?encode ?html_compat
    (XHTML.M.doctype version) (XHTML.M.toelt arbre)

let xhtml_list_print ?(header = "") ?(version=`XHTML_01_01)
    ?encode ?html_compat foret =
  x_print ~header ?encode ?html_compat
    (XHTML.M.doctype version) (XHTML.M.toeltl foret)

