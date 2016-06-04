module Markov.PosteriorProbability

open Forward
open Backward
open DPUtils

let findPosteriorProbabilityOfState (state: 'State) (index: int) startState hmm (observations: 'Emission list) =
    let forwardResult = forward startState hmm observations
    let forwardScore = 
        forwardResult.table
        |> getColumnFromTable index
        |> Array.find (fun cell -> cell.state = Some state)
        |> fun cell -> cell.score

    let backwardScore = 
        observations
        |> backward startState hmm
        |> fun backwardResult -> backwardResult.table
        |> getColumnFromTable index
        |> Array.find (fun cell -> cell.state = Some state)
        |> fun cell -> cell.score

    //forward probability should equal backward probability
    forwardScore * backwardScore / forwardResult.probability

//TODO posterior decoding of sequences