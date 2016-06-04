module Markov.Backward

open Types
open DPUtils

//let fillBackwardTable startState hmm coord table = 
//    let m, n = numRows table, numColumns table
//    match coord with
//    | (0, 0) -> 
//
//let backward (startState : Begin<'State>) (hmm : HMM<'State, 'Emission>) (observations : 'Emission list) = 
//    let states : 'State list = hmm |> Map.toList |> List.map fst
//    let table = makeDPTable states observations
//    let startCoord = (numRows table, numColumns table)
//    table
//    |> fillBackwardTable startState hmm startCoord
//    |> terminateBackward startState hmm
