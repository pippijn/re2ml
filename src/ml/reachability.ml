let check_reachable dfa actions =
  let reachable = Array.make (Array.length actions) false in

  Dfa.Fsm.iter (fun state outgoing ->
    List.iter (fun (func, target) ->
      match func with
      | Dfa.Transition.Accept action ->
          reachable.(action) <- true
      | Dfa.Transition.Chr _ ->
          ()
    ) outgoing
  ) dfa;

  Array.iteri (fun action reachable ->
    if not reachable then
      Diagnostics.warning (fst actions.(action))
        "This action will never be executed, as earlier rules hide it
         Consider reordering rules";
  ) reachable
