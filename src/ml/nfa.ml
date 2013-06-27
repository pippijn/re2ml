open Ast
open Sexplib.Conv

module State = struct
  type t = int with sexp
  type store = unit with sexp

  let encode id = id
  let decode () id = id
  let make () n = n
  let start () = make () 0
  let to_string = string_of_int
end

module Transition = struct
  (* epsilon-NFA transition functions *)
  type t =
    | Eps (* epsilon transition *)
    | Chr of char (* transition on a character *)
    | Accept of int (* transition from end to start, executing code *)
    with sexp

  let encode = function
    | Eps -> CharClass.set_end + 1
    | Chr c -> Char.code c
    | Accept a -> a + (CharClass.set_end + 2)
  let decode = function
    | c when c == CharClass.set_end + 1 -> Eps
    | c when c < CharClass.set_end -> Chr (Char.chr c)
    | a -> Accept (a - (CharClass.set_end + 2))

  let to_string = function
    | Eps -> None
    | Chr c -> Some (Char.escaped c)
    | Accept action -> Some ("A" ^ string_of_int action)
  let is_final = function
    | Accept _ -> true
    | _ -> false
end

module Fsm = Automaton.NFA(State)(Transition)


let rec construct_regexp nfa state_id regexp =
  match regexp with
  | Eof ->
      (* eof is signalled by NUL *)
      Fsm.add_transition nfa state_id (Transition.Chr '\000')

  | Char c ->
      (* on a character, simply make a transition to the next state *)
      Fsm.add_transition nfa state_id (Transition.Chr (Sloc.value c))

  | Sequence seq ->
      List.fold_left (construct_regexp nfa) state_id seq

  | CharClass (Positive list) ->
      (* make a new state *)
      let common_target = Fsm.add_state nfa in

      (* make a transition on each character in the list to the new state *)
      List.iter (function
        | Range _ -> failwith "unresolved range"
        | Single c ->
            Fsm.add_outgoing nfa state_id (Transition.Chr (Sloc.value c)) common_target
      ) list;

      common_target


  | Alternation group ->
      (* make a common start state *)
      let common_start = Fsm.add_state nfa in

      (* and transition from the current state to that state *)
      Fsm.add_outgoing nfa state_id Transition.Eps common_start;

      (* in a list of alternatives, make a transition from the current
       * state to all alternatives and a transition to a common target
       * state from each alternative end-state *)
      let end_states =
        List.fold_left (fun end_states regexp ->
          let end_state = construct_regexp nfa common_start regexp in
          end_state :: end_states
        ) [] group
      in

      (* make a new state *)
      let common_end = Fsm.add_state nfa in

      (* add an epsilon transition from all end states to the common end *)
      List.iter (fun end_state ->
        Fsm.add_outgoing nfa end_state Transition.Eps common_end
      ) end_states;

      (* the common end is our new state *)
      common_end

  | Binding (regexp, name) ->
      Fsm.mark nfa state_id (Sloc.value name)
        (construct_regexp nfa state_id) regexp

  | Plus (regexp) ->
      (* a+ makes an epsilon transition from the end of 'a' back
       * to this state *)
      let end_state = construct_regexp nfa state_id regexp in
      Fsm.add_outgoing nfa end_state Transition.Eps state_id;
      end_state

  | AnyChar | Lexeme _ | CharClass _ | Question _ | Star _ | Quantified _ | CharProperty _ | String _ ->
      failwith ("unresolved regexp: " ^ Sexplib.Sexp.to_string_hum (sexp_of_regexp regexp))


let rec is_single_char = function
  | CharClass _ | Char _ ->
      true
  | Alternation group ->
      List.for_all is_single_char group
  | _ ->
      false


let construct_rule nfa actions (Rule (regexp, code)) =
  (* create a local start state for this rule and an epsilon transition
   * from the global start state *)
  let start_state = Fsm.add_transition nfa (State.start ()) Transition.Eps in

  (* construct one NFA for this rule's regexp *)
  let end_state = construct_regexp nfa start_state regexp in

  let binding =
    match regexp with
    | Binding (regexp, name) ->
        Some (is_single_char regexp, name)
    | _ ->
        None
  in

  (* make the final accept-transition back to 0 via semantic action *)
  let action = BatDynArray.length actions in
  BatDynArray.add actions (code, binding);
  Fsm.add_outgoing nfa end_state (Transition.Accept action) (State.start ())


let construct_lexer (Lexer (name, args, rules)) =
  let nfa = Fsm.start () in
  let actions = BatDynArray.create () in
  List.iter (construct_rule nfa actions) rules;

  name, args, (nfa, BatDynArray.to_array actions)


let construct (Program (pre, aliases, lexers, post)) =
  assert (aliases == []);
  (* first, construct eNFAs (epsilon NFA) for each lexer *)
  pre, post, List.rev_map construct_lexer lexers
