open ExtList

module type ENTRY =
sig
  type entry
  type sort_criterion
  val name : entry -> string
  val tags : entry -> string list
  val deps : entry -> string list
  val signal_deps_changed : entry -> unit
  val compare :
    ?secondary : sort_criterion list -> sort_criterion -> entry -> entry -> int
end

module type S =
sig
  type entry
  type sort_criterion
  type t
  val refresh : t -> unit
  val entries : t -> entry list
  val sorted_entries :
    ?reverse : bool ->
    ?secondary : sort_criterion list -> sort_criterion -> t -> entry list
  val get_entry : t -> string -> entry option
  val has_entry : t -> string -> bool
end

let compare_by_criteria basic_comparison ?(extra = []) crit =
  let basic_sort f criterion =
    let cmp = basic_comparison criterion in
      (fun x y -> match cmp x y with 0 -> f x y | n -> n) in
  let rec compare_loop = function
      [] -> (fun x y -> 0)
    | c::cs -> basic_sort (compare_loop cs) c
  in compare_loop (crit :: extra)

module Make(Entry : sig
              include ENTRY
              val make : name:string -> file:string -> entry
            end)
: sig
  include S with type entry = Entry.entry
             and type sort_criterion = Entry.sort_criterion
  val make : string -> t
end =
struct
  module M = Map.Make(String)
  module S = Set.Make(String)

  type entry = Entry.entry
  type sort_criterion = Entry.sort_criterion

  type t = {
    basedir : string;
    mutable entries : entry_info M.t;
  }

  and entry_info = {
    e_time : float;
    e_name : string;
    e_data : entry;
  }

  let (/^) = Filename.concat

  type entry_file = string * float * string

  let get_files basedir : entry_file list =
    List.filter_map
      (fun f ->
         let fname = basedir /^ f in
         let stat = Unix.stat fname in
           match stat.Unix.st_kind with
               Unix.S_REG -> Some (fname, stat.Unix.st_mtime, f)
             | _ -> None)
      (Array.to_list (Sys.readdir basedir))

  let entry_info ~mtime ~name ~fname =
    { e_time = mtime; e_name = name; e_data = Entry.make ~file:fname ~name }

  let make basedir =
    let entries =
      List.fold_left
        (fun m (fname, mtime, name) ->
           M.add name (entry_info ~mtime ~name ~fname) m)
        M.empty
        (get_files basedir)
    in
      {
        basedir = basedir;
        entries = entries;
      }

  let map_values m = List.rev (M.fold (fun _ v l -> v :: l) m [])

  module ES =
    Set.Make(struct type t = entry_file let compare = compare end)

  let refresh t =
    let entries', mtimes', entry_files' =
      List.fold_left
        (fun (s, m, s2) ((_, mtime, name) as entry_file)->
           (S.add name s, M.add name mtime m, ES.add entry_file s2))
        (S.empty, M.empty, ES.empty)
        (get_files t.basedir) in
    let entries, mtimes =
      M.fold
        (fun name e (s, m) -> (S.add name s, M.add name e.e_time m))
        t.entries
        (S.empty, M.empty) in
    let add_entries = S.diff entries' entries in
    let del_entries = S.diff entries entries' in
    let changed_entries =
      S.filter
        (fun name -> M.find name mtimes' > M.find name mtimes)
        (S.inter entries entries') in

    let changed_deps e =
      List.exists
        (fun dep -> S.mem dep add_entries || S.mem dep del_entries)
        (Entry.deps e.e_data) in

    let update_entry e =
      if S.mem e.e_name del_entries then None
      else if S.mem e.e_name changed_entries then begin
        Some (entry_info
                ~name:e.e_name ~fname:(t.basedir /^ e.e_name)
                ~mtime:(M.find e.e_name mtimes'))
      end else if changed_deps e then begin
        Entry.signal_deps_changed e.e_data;
        Some e
      end else Some e (* not changed *) in

    let new_entries =
      ES.fold
        (fun (fname, mtime, name) m -> match S.mem name entries with
             true -> m
           | false -> M.add name (entry_info ~mtime ~fname ~name) m)
        entry_files'
        M.empty

    in match List.map S.is_empty [add_entries; del_entries; changed_entries] with
        [true; true; true] -> ()
      | _ ->
          t.entries <- M.fold
                         (fun name e m -> match update_entry e with
                              None -> m
                            | Some e -> M.add name e m)
                         (M.fold M.add new_entries t.entries)
                         M.empty

  let entries t = List.map (fun e -> e.e_data) (map_values t.entries)

  let sorted_entries ?(reverse = false) ?secondary criterion t =
    let l = List.fast_sort (Entry.compare ?secondary criterion)
              (List.map (fun e -> e.e_data) (map_values t.entries))
    in match reverse with
        true -> List.rev l
      | false -> l

  let get_entry t name =
    try Some (M.find name t.entries).e_data with Not_found -> None

  let has_entry t name = M.mem name t.entries
end
