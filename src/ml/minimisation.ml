(*
P := {F, Q \ F};
W := {F};
while (W is not empty) do
     choose and remove a set A from W
     for each c in ∑ do
          let X be the set of states for which a transition on c leads to a state in A
          for each set Y in P for which X ∩ Y is nonempty do
               replace Y in P by the two sets X ∩ Y and Y \ X
               if Y is in W
                    replace Y in W by the same two sets
               else
                    if |X ∩ Y| <= |Y \ X|
                         add X ∩ Y to W
                    else
                         add Y \ X to W
          end;
     end;
end;
*)


(* construct the initial partition Q \ F
 * i.e. split the states into final and non-final states *)
let initial_partition dfa =
    Dfa.Fsm.fold (fun state outgoing (finals, nonfinals) ->
      if List.exists (fun (func, target) -> Dfa.Transition.is_final func) outgoing then
        state :: finals, nonfinals
      else
        finals, state :: nonfinals
    ) dfa ([], [])


let minimise_step p w =
  match w with
  (* while (W is not empty) do *)
  | [] -> ()
  (* choose and remove a set A from W *)
  | a :: w ->
      ()
      (* let X be the set of states for which a transition on c leads to a state in A *)
      (*
      let x = 

     for each c in ∑ do
          let X be the set of states for which a transition on c leads to a state in A
          for each set Y in P for which X ∩ Y is nonempty do
               replace Y in P by the two sets X ∩ Y and Y \ X
               if Y is in W
                    replace Y in W by the same two sets
               else
                    if |X ∩ Y| <= |Y \ X|
                         add X ∩ Y to W
                    else
                         add Y \ X to W
          end;
     end;
     *)


let minimise dfa =
  let finals, nonfinals = initial_partition dfa in
  let p = [finals; nonfinals] in
  let w = [finals] in

  (*
  let transition_map =
    Array.init CharClass.set_end (fun 
      *)

  minimise_step p w;

  dfa
