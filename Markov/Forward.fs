module Markov.Forward

open Types
open DPUtils

type ForwardResult<'State, 'Emission> = 
    {
        probability : Probability
        table : MarkovDPCell<'State, 'Emission> [,]
    }

let sumForwards (hmm: HMM<_,_>) startState prevColumn currState = 
    prevColumn
    |> Array.sumBy (fun prevCell ->
        let pTransition = 
            match prevCell.state with
            | Some prevState ->
                Map.find prevState hmm
                |> fun i -> i.transitions
                |> List.find (fst >> ((=) (Some currState)))
                |> snd
            | None ->
                // should give 0 because pCell.score should be 0 for these cells
                startState
                |> List.find (fst >> ((=) (Some currState)))
                |> snd
        prevCell.score * pTransition)

let updatedForwardCell hmm startState table currState currEmission i l = 
    let prevColumn = getColumnFromTable (l-1) table 
    let pEmission = 
        Map.find currState hmm
        |> fun i -> i.emissions
        |> List.find (fst >> ((=) currEmission))
        |> snd
    let forwardSum = sumForwards hmm startState prevColumn currState
    {table.[i,l] with score = pEmission * forwardSum}

let rec fillForwardTable startState hmm coord (table : MarkovDPCell<_,_> [,]) =
    match coord with
    | (0, 0) ->
        table.[0,0] <- {table.[0,0] with score = 1.}
    | (0, k) ->
        table.[0,k] <- {table.[0,k] with score = 0.} 
    | (x, 0) ->
        table.[x,0] <- {table.[x,0] with score = 0.}
    | (i, l) ->
        match table.[i,l].state, table.[i,l].emission with
        | Some currState, Some currEmission ->
            table.[i,l] <- updatedForwardCell hmm startState table currState currEmission i l
        | _, _ -> 
            printfn "Something went very wrong"
    getNextCell (numRows table) (numColumns table) coord
    |> function
        | Some newCoord ->
            fillForwardTable startState hmm newCoord table
        | None ->
            table

let terminateForward startState hmm table = 
    //define with composition if it doesn't result in error: invalid forward reference
    table
    |> getLastColumn
    |> Array.sumBy (fun cell -> 
        let pEnd =
            match cell.state with
            | Some lastState ->
                Map.find (lastState) hmm
                |> fun info -> info.transitions
                |> List.find (fst >> Option.isNone)
                |> snd
            | None ->
                // This represents the case where there are no observations.
                // The start state transitions directly to the end state.
                // Attempt to find the probability, otherwise return 0.
                List.tryFind (fst >> Option.isNone) startState
                |> function
                    | Some (None, pBeginToEnd) -> pBeginToEnd
                    | _ -> 0.
        cell.score * pEnd)

let forward (startState : Begin<'State>) (hmm : HMM<'State, 'Emission>) (observations : 'Emission list) = 
    let table = 
        hmm 
        |> Map.toList 
        |> List.map fst
        |> makeDPTable observations
    let prob = 
        table
        |> fillForwardTable startState hmm (0, 0)
        |> terminateForward startState hmm
    {probability = prob; table = table}