﻿module Markov.Backward

open Types
open DPUtils

type BackwardResult<'State, 'Emission> = 
    {
        probability : Probability
        table : MarkovDPCell<'State, 'Emission> [,] 
    }

let pEndTransition transitions = 
    transitions
    |> List.tryFind (fst >> ((=) None))
    |> function
        | Some (_endState, pEnd) -> pEnd
        | None -> 0.

let pEnd hmm = function
    | Some state ->
        Map.find state hmm.internalState
        |> fun i -> i.transitions
        |> pEndTransition
    | None ->
        hmm.startState
        |> pEndTransition

let sumBackwards hmm nextColumn currState = 
    nextColumn
    |> Array.sumBy (fun nextCell ->
        match nextCell.state, nextCell.emission with
        | Some nextState, Some nextEmission ->
            let pTransitionToNextCell = 
                match currState with
                | Some currState ->
                    Map.find currState hmm.internalState
                    |> fun i -> i.transitions
                    |> List.find (fst >> ((=) (Some nextState)))
                    |> snd
                | None ->
                    //TODO handle this case
                    0.
            let pEmission = 
                Map.find nextState hmm.internalState
                |> fun i -> i.emissions
                |> List.find (fst >> ((=) nextEmission))
                |> snd
            let nextBackwardScore = nextCell.score
            pTransitionToNextCell * pEmission * nextBackwardScore
        | _, _ ->
            //TODO handle this case
            //should this ever be matched?
            0.)

let updatedBackwardCell hmm table currState i l = 
    let nextColumn = getColumnFromTable (l+1) table
    let backwardSum = sumBackwards hmm nextColumn currState
    {table.[i,l] with score = backwardSum}

let rec fillBackwardTable hmm coord table = 
    match coord with
    | (foo, k) when k = (numColumns table) - 1 -> 
        let pEnd = pEnd hmm table.[foo,k].state
        table.[foo,k] <- {table.[foo,k] with score = pEnd}
    | (x, 0) ->
        // Is this necessary?
        table.[x,0] <- {table.[x,0] with score = 0.}
    | (i, l) -> 
        table.[i,l] <- updatedBackwardCell hmm table table.[i,l].state i l
    getNextBackwardCell (numRows table) (numColumns table) coord
    |> function
        | Some newCoord ->
            fillBackwardTable hmm newCoord table
        | None ->
            table

//TODO
let terminateBackward hmm table = 
    table
    //We want the column with index 1 (the 2nd column) because the first column of the table is filled with nothing/garbage.
    //TODO what if there is no column 1? Ie, there are no observations, because we directly transition from the start state to the end state?
    |> getColumnFromTable 1
    |> Array.sumBy (fun cell ->

        let pStartToThisState = 
            hmm.startState
            |> List.tryFind (fst >> ((=) cell.state))
            |> function
                | Some (stateOpt, prob) -> prob
                | None -> 0.

        //all cells in the first column should have Some emission. 
        let pEmission = 
            match cell.state, cell.emission with
            | Some state, Some emission ->
                Map.find state hmm.internalState
                |> fun i -> i.emissions
                |> List.find (fst >> ((=) emission))
                |> snd
            | _, _ -> 0.

        let firstBackwardScore = cell.score
        pStartToThisState * pEmission * firstBackwardScore)

let backward (hmm : HMM<'State, 'Emission>) (observations : 'Emission list) = 
    let table = 
        hmm.internalState
        |> Map.toList 
        |> List.map fst
        |> makeDPTable observations
    let startCoord = ((numRows table) - 1, (numColumns table) - 1)
    let prob = 
        table
        |> fillBackwardTable hmm startCoord
        |> terminateBackward hmm
    {probability = prob; table = table}
