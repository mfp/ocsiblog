(* Copyright (C) 2009 Mauricio Fernandez <mfp@acm.org> *)
open ExtList
open ExtString

module type ENTRY =
sig
  type 'a entry
  type sort_criterion
  val name : _ entry -> string
  val tags : _ entry -> string list
  val deps : _ entry -> string list
  val signal_deps_changed : _ entry -> unit
  val compare :
    ?secondary : sort_criterion list -> sort_criterion -> 'a entry -> 'a entry -> int
end

module type S =
sig
  type 'a entry
  type sort_criterion
  type 'a t
  val refresh : _ t -> unit
  val entries : 'a t -> 'a entry list
  val sorted_entries :
    ?reverse : bool ->
    ?secondary : sort_criterion list -> sort_criterion -> 'a t -> 'a entry list
  val get_entry : 'a t -> string -> 'a entry option
  val has_entry : _ t -> string -> bool
end

let compare_by_criteria basic_comparison ?(extra = []) crit =
  let basic_sort f criterion =
    let cmp = basic_comparison criterion in
      (fun x y -> match cmp x y with 0 -> f x y | n -> n) in
  let rec compare_loop = function
      [] -> (fun x y -> 0)
    | c::cs -> basic_sort (compare_loop cs) c
  in compare_loop (crit :: extra)

let dir_filter_map f dir =
  List.filter_map
    (fun fname -> if String.starts_with fname "." then None else (f fname))
    (Array.to_list (Sys.readdir dir))

module Make(Entry : sig
              include ENTRY
              val make : name:string -> file:string -> 'a entry
            end)
: sig
  include S with type 'a entry = 'a Entry.entry
             and type sort_criterion = Entry.sort_criterion
  val make : string -> 'a t
end =
struct
  module M = Map.Make(String)
  module S = Set.Make(String)

  type 'a entry = 'a Entry.entry
  type sort_criterion = Entry.sort_criterion

  type 'a t = {
    basedir : string;
    mutable entries : 'a entry_info M.t;
  }

  and 'a entry_info = {
    e_time : float;
    e_name : string;
    e_data : 'a entry;
  }

  let (/^) = Filename.concat

  type entry_file = { fname : string; mtime : float; name : string }

  let entry_info { mtime = mtime; name = name; fname = file } =
    { e_time = mtime; e_name = name; e_data = Entry.make ~file ~name }

  let get_files basedir =
    dir_filter_map
      (fun f ->
         let fname = basedir /^ f in
         let stat = Unix.stat fname in
           match stat.Unix.st_kind with
               Unix.S_REG ->
                 Some { fname = fname; mtime = stat.Unix.st_mtime; name = f; }
             | _ -> None)
      basedir

  let make basedir =
    let entries =
      List.fold_left
        (fun m t -> M.add t.name (entry_info t) m) M.empty (get_files basedir)
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
        (fun (s, m, s2) (t as entry_file)->
           (S.add t.name s, M.add t.name t.mtime m, ES.add entry_file s2))
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
                { name = e.e_name; fname = (t.basedir /^ e.e_name);
                  mtime = M.find e.e_name mtimes'; })
      end else if changed_deps e then begin
        Entry.signal_deps_changed e.e_data;
        Some e
      end else Some e (* not changed *) in

    let new_entries =
      ES.fold
        (fun efile m -> match S.mem efile.name entries with
             true -> m
           | false -> M.add efile.name (entry_info efile) m)
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
