
open Simple_markup
open XHTML.M

type html_output = 
    [`Address | `Blockquote | `Del | `Div | `Dl | `Fieldset
     | `Form | `H1 | `H2 | `H3 | `H4 | `H5 | `H6 | `Hr | `Ins
     | `Noscript | `Ol | `P | `Pre | `Script | `Table | `Ul ]

let rec elm_to_html ~render_pre ~render_link ~render_img elm = 
  let self = elm_to_html ~render_pre ~render_link ~render_img in
  let item l = li (List.map self l) 
  
  in match elm with
      Normal text -> p (par_text_to_html ~render_link ~render_img text)
    | Pre (s, kind) -> begin match kind with
          Some k -> render_pre ~kind:k s
        | None -> pre [pcdata s]
      end
    | Heading (l, text) ->
        let f =
          match l with 1 -> h1 | 2 -> h2 | 3 -> h3 | 4 -> h4 | 5 -> h5 | _ -> h6
        in f (par_text_to_html render_link render_img text)
    | Quote ps -> blockquote (List.map self ps)
    | Ulist (fst, others) ->
        ul (item fst) (List.map item others)
    | Olist (fst, others) ->
        let item l = li (List.map self l) in
          ol (item fst) (List.map item others)

and par_text_to_html ~render_link ~render_img =
  List.map (text_to_html ~render_link ~render_img)

and text_to_html ~render_link ~render_img = function
    Text s -> pcdata s
  | Emph s -> em [pcdata s]
  | Bold s -> b [pcdata s]
  | Struck l -> del (List.map (text_to_html ~render_link ~render_img) l)
  | Code s -> code [pcdata s]
  | Link href -> render_link href
  | Image href -> render_img href

let to_html ~render_pre ~render_link ~render_img l : 
      [> html_output] XHTML.M.elt list =
  List.map (elm_to_html ~render_pre ~render_link ~render_img) l
