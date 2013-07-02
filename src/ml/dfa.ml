open Ast
open Sexplib.Conv

module State = struct

  type state_id = int with sexp
  type subset = Nfa.State.t list with sexp

  type t = {
    id : state_id;
    subset : subset;
  } with sexp

  module SubsetMap = Hashtbl.Make(struct
    type t = subset

    let hash l = List.fold_left ( * ) 1 l
    let rec equal a b =
      a == b ||
      match a, b with
      | hd1 :: tl1, hd2 :: tl2 -> hd1 == hd2 && equal tl1 tl2
      | [], [] -> true
      | _ -> false
  end)

  type store = {
    states  : state_id SubsetMap.t;
    subsets : subset BatDynArray.t;
  }

  let make_store () = {
    states = SubsetMap.create 13;
    subsets = BatDynArray.create ();
  }

  let sexp_of_store s = Sexplib.Sexp.List []
  let store_of_sexp s = make_store ()

  let encode { id } = id
  let decode store id = { id; subset = BatDynArray.get store.subsets id }

  let make store = failwith "Dfa.State.make called"

  let of_list { states; subsets } subset =
    let id =
      try
        SubsetMap.find states subset
      with Not_found ->
        let id = SubsetMap.length states in
        assert (id == BatDynArray.length subsets);
        SubsetMap.add states subset id;
        BatDynArray.add subsets subset;
        id
    in
    { id; subset }

  let start store = of_list store [0]
  let to_string { subset } = String.concat ", " (List.map string_of_int subset)

end


module Transition = struct

  (* DFA transition functions *)
  type t =
    | Chr of char (* transition on a character *)
    | Accept of int (* transition from end to start, executing code *)
    with sexp

  let encode = function
    | Chr c -> Char.code c
    | Accept a -> a + (CharClass.set_end + 1)
  let decode = function
    | c when c < CharClass.set_end -> Chr (Char.chr c)
    | a -> Accept (a - (CharClass.set_end + 1))

  let to_string = function
    | Chr c -> Some (Char.escaped c)
    | Accept action -> Some ("A" ^ string_of_int action)
  let is_final = function
    | Accept _ -> true
    | _ -> false

end


module Fsm = Automaton.DFA(State)(Transition)


(* epsilon closure: a list of epsilon-reachable states for each state *)
type eclosure = Nfa.State.t list IntMap.t with sexp


let uniq estates =
  BatList.sort_unique (-) estates


let get_e_closure eclosure state =
  IntMap.find_default [] state eclosure

(* compute epsilon-accessible states for a state *)
let compute_estates nfa state outgoing eclosure =
  let estates =
    List.fold_left (fun estates (func, target) ->
      match func with
      | Nfa.Transition.Eps ->
          (* directly epsilon-accessible states *)
          target :: estates
      | _ ->
          estates
    ) (get_e_closure eclosure state) outgoing
  in

  (* add this state's estates to the closure *)
  IntMap.add state (uniq estates) eclosure


let get_e_closure eclosure state =
  IntMap.find state eclosure

let e_closure eclosure estates =
  (* add all epsilon-accessible states from other epsilon-accessible
   * states to the e-states list for the current state *)
  List.fold_left (fun estates target ->
    CoreList.unordered_append estates (get_e_closure eclosure target)
  ) estates estates

let rec estates_closure eclosure =
  let closure =
    IntMap.fold (fun state estates closure ->
      let combined_estates = e_closure closure estates in

      (* eliminate duplicates *)
      let combined_estates = uniq combined_estates in

      (* if anything changed *)
      if combined_estates <> estates then
        (* update the map *)
        IntMap.add state combined_estates closure
      else
        (* otherwise, don't change anything *)
        closure
    ) eclosure eclosure
  in

  (* loop until no changes *)
  if closure != eclosure then
    estates_closure closure
  else
    closure


let compute_eclosure nfa =
  (* first step: compute directly epsilon-accessible states *)
  let eclosure = Nfa.Fsm.fold (compute_estates nfa) nfa IntMap.empty in

  (* second step: closure *)
  estates_closure eclosure


let next_dfa_state nfa subset_map estate_ids =
  (* clear map *)
  Array.fill subset_map 0 (Array.length subset_map) [];

  List.fold_left (fun accept state_id ->
    List.fold_left (fun accept (func, target) ->
      match func with
      | Nfa.Transition.Chr chr ->
          if false then
            Printf.printf "-> %d with %s\n" target (Char.escaped chr);

          let chr = Char.code chr in
          subset_map.(chr) <- target :: subset_map.(chr);

          accept

      | Nfa.Transition.Accept action ->
          if false then
            Printf.printf "-> %d action %d [final]\n" target action;
          action :: accept

      | Nfa.Transition.Eps ->
          accept

    ) accept (Nfa.Fsm.outgoing nfa state_id)
  ) [] estate_ids


let add_transition state_store state dfa todo c targets =
  match targets with
  | [] ->
      todo

  | _  ->
      (* each element in the map is a new state in the DFA *)
      let target_state = State.of_list state_store targets in
      Fsm.add_outgoing dfa state (Transition.Chr (Char.chr c)) target_state;
      target_state :: todo


let add_dfa_state state_store dfa accept subset_map state =
  (* add this subset-state to the automaton *)
  Fsm.add dfa state;

  (* if any state in the epsilon closure is a final state, then
   * this state is also a final state *)
  begin match List.rev accept with
  | [] ->
      (* not a final state *)
      ()
  | [action] ->
      (* a single accept action *)
      Fsm.add_outgoing dfa state (Transition.Accept action) (State.start state_store)
  | action :: _ ->
      (* add the first action *)
      Fsm.add_outgoing dfa state (Transition.Accept action) (State.start state_store)
  end;

  BatArray.fold_lefti (add_transition state_store state dfa) [] subset_map


let set_dfa_markers nfa dfa state estate_ids =
  List.iter (fun nfa_state ->
    List.iter (fun marker ->
      Fsm.set_mark dfa state marker
    ) (Nfa.Fsm.marks nfa nfa_state)
  ) estate_ids


let rec construct_subsets nfa state_store subset_map eclosure dfa state =
  if not (Fsm.mem dfa state) then (
    if false then (
      Printf.printf "--- %s ---\n"
        (State.to_string state)
    );

    let subset = state.State.subset in

    (* get the epsilon closure for each state in the subset *)
    let estate_ids = e_closure eclosure subset in

    (* eliminate duplicates *)
    let estate_ids = uniq estate_ids in

    (* make a map from input symbol to a set of states *)
    let accept = next_dfa_state nfa subset_map estate_ids in

    let todo = add_dfa_state state_store dfa accept subset_map state in
    set_dfa_markers nfa dfa state estate_ids;
    List.iter (construct_subsets nfa state_store subset_map eclosure dfa) todo
  )


let of_nfa (name, args, (nfa, actions)) =
  if false then (
    Sexplib.Sexp.output_hum stdout (Nfa.Fsm.sexp_of_t nfa);
    print_newline ();
  );

  if Options._dot () then (
    BatStd.with_dispose ~dispose:close_out
      (fun out -> Nfa.Fsm.to_dot ~lr:true ~final:false out nfa) (open_out "nfa.dot");
    ignore (Sys.command "dot -Tpng nfa.dot -o nfa.png");
  );

  let eclosure = Timing.progress "computing epsilon closure" compute_eclosure nfa in
  if false then (
    Sexplib.Sexp.output_hum stdout (sexp_of_eclosure eclosure);
    print_newline ();
  );

  (* start at the subset containing only the NFA's start state *)
  let dfa =
    let state_store = State.make_store () in
    let subset_map = Array.make CharClass.set_end [] in
    let dfa = Fsm.empty state_store in

    Timing.progress "constructing DFA"
      (Valgrind.Callgrind.instrumented
        (construct_subsets nfa state_store subset_map eclosure dfa)) (State.start state_store);

    if Options._dot () then (
      BatStd.with_dispose ~dispose:close_out
        (fun out -> Fsm.to_dot ~lr:true ~final:false out dfa) (open_out "dfa.dot");
      ignore (Sys.command "dot -Tpng dfa.dot -o dfa.png");
    );

    dfa
  in

  name, args, (dfa, actions)
