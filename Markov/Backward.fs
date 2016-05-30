module Markov.Backward

(*


The Backward Algorithm


*)
//let backward (startState : Begin<'State>) (hmm : HMM<'State, 'Emission>) (observations : 'Emission list) = 
//    let states : 'State list = hmm |> Map.toList |> List.map fst
//    makeDPTable states observations
//    |> fillBackwardTable startState hmm (0,0)
//    |> terminateBackward
