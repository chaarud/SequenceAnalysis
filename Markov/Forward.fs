module Markov.Forward

open Types
open DPUtils

(*


The Forward Algorithm


*)
// It's unclear if the forwards algorithm is correct, especially wrt termination
// clean up representation - shouldn't use ViterbiCells, etc
let sumForwards (hmm: HMM<_,_>) startState prevColumn currState = 
    prevColumn
    |> Array.sumBy (fun pCell ->
        let pTransition = 
            match pCell.state with
            | Some pState ->
                Map.find pState hmm
                |> fun i -> i.transitions
                |> List.find (fst >> ((=) (Some currState)))
                |> snd
            | None ->
                // should give 0 because pCell.score should be 0 for these cells
                startState
                |> List.find (fst >> ((=) (Some currState)))
                |> snd
        pCell.score * pTransition)

let updatedForwardCell hmm startState table currState currEmission i l = 
    let prevColumn = getColumnFromTable (l-1) table 
    let pEmission = 
        Map.find currState hmm
        |> fun i -> i.emissions
        |> List.find (fst >> ((=) currEmission))
        |> snd
    let forwardSum = sumForwards hmm startState prevColumn currState
    {
        table.[i,l] with
            score = pEmission * forwardSum
            ancestor = None //Ancestors are junk for forwards table filling
    }

let rec fillForwardTable startState hmm coord (table : ViterbiCell<_,_> [,]) =
    match coord with
    | (0, 0) ->
        table.[0,0] <- {table.[0,0] with score = 1.}
    | (0, k) ->
        // do we need an ancestor pointer here?
        table.[0,k] <- {table.[0,k] with score = 0.} 
    | (x, 0) ->
        // there's no way we are in the begin state if it's not the first column, right?
        table.[x,0] <- {table.[x,0] with score = 0.}
    | (i, l) ->
        match table.[i,l].state, table.[i,l].emission with
        | Some currState, Some currEmission ->
            table.[i,l] <- updatedForwardCell hmm startState table currState currEmission i l
        | _, _ -> 
            printf "Something went very wrong"
    getNextViterbiCell (Array2D.length1 table) (Array2D.length2 table) coord
    |> function
        | Some newCoord ->
            fillForwardTable startState hmm newCoord table
        | None ->
            table

let terminateForward startState hmm table = 
    //define with composition if it doesn't result in crazy errors
    table
    |> getLastColumn
    |> Array.sumBy (fun cell -> 
        let state = cell.state
        //could/should pattern match, should be ok though
        let pEnd =
            match state with
            | Some lastState ->
                let lastStateInfo = Map.find (lastState) hmm
                snd <| List.find (fst >> Option.isNone) lastStateInfo.transitions
            | None ->
                //this means the start state
                //should be 0 probability of being in the forward state unless the observations list is empty, 
                //but we should account for this
                //it might be that we want to force the nonzero length observations,
                //so the start state does not have a None transition probability...
                //in this case return 0.
                List.tryFind (fst >> Option.isNone) startState
                |> function
                    | Some (None, pBeginToEnd) -> pBeginToEnd
                    | _ -> 0.
        cell.score * pEnd)

//this ignores transition probabilities to a special end state
let Forward (startState : Begin<'State>) (hmm : HMM<'State, 'Emission>) (observations : 'Emission list) = 
    let states : 'State list = hmm |> Map.toList |> List.map fst
    makeViterbiTable states observations
    |> fillForwardTable startState hmm (0, 0)
    |> terminateForward startState hmm

