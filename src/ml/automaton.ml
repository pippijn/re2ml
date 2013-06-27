open Sexplib.Conv

(* resize array if necessary *)
let resize_array arr index default =
  if Array.length arr <= index then (
    let resized = Array.make (index * 2 + 1) default in
    Array.blit
      arr 0
      resized 0
      (Array.length arr);
    resized
  ) else (
    arr
  )


module type StateType = sig
  include Sig.ConvertibleType

  type store

  val store_of_sexp : Sexplib.Sexp.t -> store
  val sexp_of_store : store -> Sexplib.Sexp.t

  val encode : t -> int
  val decode : store -> int -> t

  val make : store -> int -> t

  (* the local start state for an automaton *)
  val start : store -> t

  val to_string : t -> string
end

module type TransitionType = sig
  include Sig.ConvertibleType

  val encode : t -> int
  val decode : int -> t

  val is_final : t -> bool
  val to_string : t -> string option
end


module type S = sig
  include Sig.ConvertibleType

  module S : StateType
  type state = S.t

  module T : TransitionType
  type transition = T.t

  type edge = transition * state with sexp

  val mem : t -> state -> bool
  val cardinal : t -> int

  val add : t -> state -> unit
  val add_state : t -> state
  val add_outgoing : t -> state -> transition -> state -> unit
  val add_transition : t -> state -> transition -> state

  val set_mark : t -> state -> string -> unit
  val mark : t -> state -> string -> ('a -> state) -> 'a -> state
  val marks : t -> state -> string list

  val outgoing : t -> state -> edge list

  val fold_outgoing : ('a -> transition -> state -> 'a) -> 'a -> t -> state -> 'a

  val fold : (state -> edge list -> 'a -> 'a) -> t -> 'a -> 'a
  val iter : (state -> edge list -> unit) -> t -> unit

  val is_final : t -> state -> bool

  val empty : S.store -> t
  val start : S.store -> t

  val to_dot : lr:bool -> ?final:bool -> out_channel -> t -> unit
end


module type Determinism = sig
  val deterministic : bool
end

module Deterministic : Determinism = struct
  let deterministic = true
end

module Nondeterministic : Determinism = struct
  let deterministic = false
end


module Make(D : Determinism)(S : StateType)(T : TransitionType)
: S with module S = S
     and module T = T
= struct

  module S = S
  type state = S.t with sexp

  module T = T
  type transition = T.t with sexp

  type edge = transition * state with sexp

  type vertex = {
    state_id : state;
    mutable outgoing : state list array;
  } with sexp

  type t = {
    store : S.store;
    mutable states : vertex option array;
    mutable markers : string list array;
  } with sexp


  let state_invariants fsm state =
    state = S.decode fsm.store (S.encode state)

  let transition_invariants fsm transition =
    transition = T.decode (T.encode transition)


  let mem fsm state =
    assert (state_invariants fsm state);
    let state_id = S.encode state in
    Array.length fsm.states > state_id &&
    fsm.states.(state_id) != None


  let cardinal fsm =
    ExtArray.count BatOption.is_some fsm.states


  (* add a new empty state *)
  let add fsm state_id =
    assert (state_invariants fsm state_id);
    if mem fsm state_id then
      invalid_arg "can not replace existing state";
    let state = { state_id; outgoing = Array.make (CharClass.set_end / 2) [] } in
    let state_id = S.encode state_id in

    fsm.states <- resize_array fsm.states state_id None;
    fsm.states.(state_id) <- Some state


  let next_state_id fsm =
    try
      (* XXX: this makes adding new states an O(n log n) operation,
       * but it only happens during NFA construction, which is fast
       * enough (0.003 seconds for the C++ lexer) *)
      BatArray.findi BatOption.is_none fsm.states
    with Not_found ->
      Array.length fsm.states


  let add_state fsm =
    let b = S.make fsm.store (next_state_id fsm) in
    add fsm b;
    b


  (* add transition on c from a to b *)
  let add_outgoing fsm a t b =
    assert (state_invariants fsm a);
    assert (transition_invariants fsm t);
    assert (state_invariants fsm b);

    let a = S.encode a in
    let t = T.encode t in

    let a = BatOption.get (fsm.states.(a)) in

    a.outgoing <- resize_array a.outgoing t [];
    if D.deterministic then (
      assert (a.outgoing.(t) == []);
      a.outgoing.(t) <- [b]
    ) else (
      a.outgoing.(t) <- b :: a.outgoing.(t)
    )


  let add_transition fsm a c =
    let b = add_state fsm in
    add_outgoing fsm a c b;
    b


  let set_mark fsm state marker =
    assert (state_invariants fsm state);
    let state_id = S.encode state in
    fsm.markers <- resize_array fsm.markers state_id [];
    fsm.markers.(state_id) <-
      BatList.sort_unique (fun a b ->
        if a == b then
          0
        else
          match String.compare a b with
          | 0 -> -1
          | o -> o
      ) (marker :: fsm.markers.(state_id))

  let mark fsm start_id name f x =
    set_mark fsm start_id name;
    let end_state = f x in
    set_mark fsm end_state name;
    end_state


  let marks fsm state =
    assert (state_invariants fsm state);
    let state_id = S.encode state in
    if state_id >= Array.length fsm.markers then
      []
    else
      fsm.markers.(state_id)


  let outgoing state =
    BatArray.fold_lefti (fun outgoing transition targets ->
      List.fold_right (fun target outgoing -> (T.decode transition, target) :: outgoing) targets outgoing
    ) [] state.outgoing


  let fold f fsm x =
    Array.fold_left (fun x state ->
      match state with
      | None -> x
      | Some state ->
          f state.state_id (List.rev (outgoing state)) x
    ) x fsm.states


  let iter f fsm =
    Array.iter (function
      | None -> ()
      | Some state ->
          f state.state_id (List.rev (outgoing state))
    ) fsm.states


  let fold_outgoing f x fsm state_id =
    assert (state_invariants fsm state_id);
    BatArray.fold_lefti (fun x transition targets ->
      List.fold_left (fun x target ->
        f x (T.decode transition) target
      ) x targets
    ) x (BatOption.get fsm.states.(S.encode state_id)).outgoing


  let outgoing fsm state_id =
    assert (state_invariants fsm state_id);
    outgoing (BatOption.get (fsm.states.(S.encode state_id)))


  (* empty automaton *)
  let empty store = { store; states = [||]; markers = [||] }
  (* automaton with only a start state *)
  let start store = let fsm = empty store in add fsm (S.start store); fsm


  let rec inverted_ranges invs ranges =
    match ranges with
    | (lo1, hi1) :: ((lo2, hi2) :: _ as tl) ->
        let inv = (hi1 + 1, lo2 - 1) in
        inverted_ranges (inv :: invs) tl
    | [] | [(_, _)] ->
        invs


  let string_of_transition c =
    match T.to_string (T.decode c) with
    | None -> ""
    | Some s ->
        "'" ^ String.escaped s ^ "'"

  let string_of_range (lo, hi) =
    if lo = hi then
      string_of_transition lo
    else
      string_of_transition lo ^ "-" ^ string_of_transition hi

  let print_ranges is_inv = function
    | [(lo, hi)] when lo = hi ->
        if is_inv then
          "[^" ^ string_of_transition lo ^ "]"
        else
          string_of_transition lo
    | ranges ->
        Printf.sprintf "[%s%s]"
          (if is_inv then "^" else "")
          (String.concat " " (List.rev_map string_of_range ranges))


  let print_ranges ranges =
    let is_inv =
      let width =
        List.fold_left (fun width (lo, hi) ->
          width + (hi - lo)
        ) 0 ranges
      in

      (* if the ranges cover more than half of the total character
       * range, then this is probably an inverted range *)
      width > CharClass.set_end / 2
    in

    let ranges =
      if is_inv then
        inverted_ranges [] (List.rev ranges)
      else
        ranges
    in

    print_ranges is_inv ranges


  let is_final_id fsm state =
    match fsm.states.(state) with
    | None -> false
    | Some state ->
        BatArray.fold_lefti (fun is_final func target ->
          is_final || (target != [] && T.is_final (T.decode func))
        ) false state.outgoing


  let is_final fsm state =
    assert (state_invariants fsm state);
    is_final_id fsm (S.encode state)


  let marker fsm state_id =
    let marker =
      assert (state_invariants fsm state_id);
      let id = S.encode state_id in
      if id < Array.length fsm.markers then
        fsm.markers.(id)
      else
        []
    in

    match marker with
    | [] -> ""
    | markers -> " [" ^ String.concat ", " marker ^ "]"


  let to_dot ~lr ?(final=true) out fsm =
    output_string out "digraph G {\n";
    if lr then
      output_string out "\trankdir = LR;\n";

    let finals = Array.init (Array.length fsm.states) (is_final_id fsm) in

    output_string out "\tnode [shape = doublecircle];";
    Array.iteri (fun state is_final ->
      if is_final then (
        let state_id = S.decode fsm.store state in
        let marker = marker fsm state_id in

        Printf.fprintf out " \"%s%s\""
          (S.to_string state_id)
          marker
      )
    ) finals;
    output_string out ";\n\tnode [shape = circle];\n";

    Array.iter (function
      | None -> ()
      | Some state ->
          let source_id = state.state_id in

          (* construct inverse map: target -> transition list *)
          let outgoing = Array.make (Array.length fsm.states) [] in
          Array.iteri (fun func targets ->
            if final || not (T.is_final (T.decode func)) then
              List.iter (fun target ->
                assert (state_invariants fsm target);
                let target = S.encode target in
                outgoing.(target) <- func :: outgoing.(target)
              ) targets
          ) state.outgoing;

          let source_marker = marker fsm source_id in

          Array.iteri (fun target funcs ->
            match List.rev funcs with
            | [] -> ()
            | hd :: tl ->
                let target_id = S.decode fsm.store target in

                let ranges, wip =
                  List.fold_left (fun (ranges, (lo, hi as wip)) func ->
                    if hi + 1 = func then
                      (ranges, (lo, func))
                    else
                      (wip :: ranges, (func, func))
                  ) ([], (hd, hd)) tl
                in

                let label =
                  match print_ranges (wip :: ranges) with
                  | "" -> "ε"
                  | s  -> s
                in

                let label =
                  Printf.sprintf " [ label = \"%s\" ]"
                    label
                in

                let target_marker = marker fsm target_id in

                Printf.fprintf out "\t\"%s%s\" -> \"%s%s\"%s;\n"
                  (S.to_string source_id)
                  source_marker
                  (S.to_string target_id)
                  target_marker
                  label
          ) outgoing
    ) fsm.states;
    output_string out "}\n";
    flush out

end


module DFA = Make(Deterministic)
module NFA = Make(Nondeterministic)
