﻿module Markov.PosteriorProbability

open Forward
open Backward
open DPUtils

let posteriorProbability prob fwdScore bwdScore = 
    fwdScore * bwdScore / prob

let findPosteriorProbabilityOfState state index startState hmm observations =
    let forwardResult = forward hmm observations
    let forwardScore = 
        forwardResult.table
        |> getColumnFromTable index
        |> Array.find (fun cell -> cell.state = Some state)
        |> fun cell -> cell.score

    let backwardScore = 
        observations
        |> backward hmm
        |> fun backwardResult -> backwardResult.table
        |> getColumnFromTable index
        |> Array.find (fun cell -> cell.state = Some state)
        |> fun cell -> cell.score

    //forward probability should equal backward probability
    posteriorProbability forwardResult.probability forwardScore backwardScore 

let posteriorDecoding hmm observations = 
    let fwdResult = forward hmm observations
    let fwdTable = fwdResult.table
    let bwdResult = backward hmm observations
    let bwdTable = bwdResult.table

    let prob = fwdResult.probability
    let posterior = posteriorProbability prob

    let n = numColumns fwdTable

    [0 .. (n-1)]
    |> List.fold (fun acc i ->
        let fwdCol = getColumnFromTable i fwdTable
        let bwdCol = getColumnFromTable i bwdTable
        let combinedColumn = Array.zip fwdCol bwdCol
        let posteriorCell = 
            combinedColumn
            |> Array.maxBy (fun (fwdCell, bwdCell) ->
                //forward cell state should equal backward cell state
                posterior fwdCell.score bwdCell.score)
            |> fst
        posteriorCell :: acc) []
    |> List.choose (fun cell -> cell.state)
    |> List.rev

// TODO result of the posterior probability applied with a function G(i|x)