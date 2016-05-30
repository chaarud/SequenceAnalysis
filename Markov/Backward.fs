module Markov.Backward

(*


The Backward Algorithm


*)
//let Backward (startState : Begin<'State>) (hmm : HMM<'State, 'Emission>) (observations : 'Emission list) = 
//    let states : 'State list = hmm |> Map.toList |> List.map fst
//    makeViterbiTable states observations
//    |> fillBackwardTable startState hmm (0,0)
//    |> terminateBackward
