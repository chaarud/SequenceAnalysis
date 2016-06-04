﻿module Markov.Backward

open Types
open DPUtils

let pEndTransition transitions = 
    transitions
    |> List.tryFind (fst >> ((=) None))
    |> function
        | Some (_endState, pEnd) -> pEnd
        | None -> 0.

let pEnd startState hmm = function
    | Some state ->
        Map.find state hmm
        |> fun i -> i.transitions
        |> pEndTransition
    | None ->
        startState
        |> pEndTransition

let sumBackwards hmm startState nextColumn currState = 
    nextColumn
    |> Array.sumBy (fun nextCell ->
        match nextCell.state, nextCell.emission with
        | Some nextState, Some nextEmission ->
            let pTransitionToNextCell = 
                match currState with
                | Some currState ->
                    Map.find currState hmm
                    |> fun i -> i.transitions
                    |> List.find (fst >> ((=) (Some nextState)))
                    |> snd
                | None ->
                    //TODO handle this case
                    0.
            let pEmission = 
                Map.find nextState hmm
                |> fun i -> i.emissions
                |> List.find (fst >> ((=) nextEmission))
                |> snd
            let nextBackwardScore = nextCell.score
            pTransitionToNextCell * pEmission * nextBackwardScore
        | _, _ ->
            //TODO handle this case
            //should this ever be matched?
            0.)

let updatedBackwardCell hmm startState table currState i l = 
    let nextColumn = getColumnFromTable (l+1) table
    let backwardSum = sumBackwards hmm startState nextColumn currState
    {table.[i,l] with score = backwardSum}

let rec fillBackwardTable startState hmm coord table = 
    match coord with
    | (foo, k) when k = (numColumns table) - 1 -> 
        let pEnd = pEnd startState hmm table.[foo,k].state
        table.[foo,k] <- {table.[foo,k] with score = pEnd}
    | (x, 0) ->
        // Is this necessary?
        table.[x,0] <- {table.[x,0] with score = 0.}
    | (i, l) -> 
        table.[i,l] <- updatedBackwardCell hmm startState table table.[i,l].state i l
    getNextBackwardCell (numRows table) (numColumns table) coord
    |> function
        | Some newCoord ->
            fillBackwardTable startState hmm newCoord table
        | None ->
            table

//TODO
let terminateBackward startState hmm table = 
    table
    |> getColumnFromTable 0
    |> Array.sumBy (fun cell ->
        let pStartToThisState = 0.
        let pEmission = 0.
        let nextBackwardScore = 0.
        pStartToThisState * pEmission * nextBackwardScore)

let backward (startState : Begin<'State>) (hmm : HMM<'State, 'Emission>) (observations : 'Emission list) = 
    let table = 
        hmm 
        |> Map.toList 
        |> List.map fst
        |> makeDPTable observations
    let startCoord = ((numRows table) - 1, (numColumns table) - 1)
    table
    |> fillBackwardTable startState hmm startCoord
    |> terminateBackward startState hmm
