module Markov.Viterbi
    
open Types
open DPUtils

type ViterbiResult<'State> = 
    {
        probability : Probability
        path : 'State list
    }

let pGetToHiddenState hmm startState currState prevCell = 
    match prevCell.state with
    | Some prevState ->
        let prevInfo = Map.find prevState hmm
        let pTransitionToThisState = 
            prevInfo.transitions
            |> List.find (fst >> ((=) (Some currState)))
            |> snd
        pTransitionToThisState * prevCell.score
    | None ->
        startState 
        |> List.find (fst >> ((=) (Some currState)))
        |> snd
        //this should give 0, because prevCell.score should be 0
        //TODO can we elmiminate this and get rid of the startState parameter here
        |> (*) prevCell.score 

let updatedCell hmm startState table currState currEmission i l = 
    let prevColumn = getColumnFromTable (l-1) table 
    let pEmission = 
        Map.find currState hmm
        |> fun i -> i.emissions
        |> List.find (fst >> ((=) currEmission))
        |> snd
    let pGetToHiddenState = pGetToHiddenState hmm startState currState
    let prevCell =
        prevColumn
        |> Array.maxBy pGetToHiddenState
    let pGetToCurrState = pGetToHiddenState prevCell
    {table.[i,l] with
        score = pEmission * pGetToCurrState
        ancestor = Some prevCell}

let rec fillViterbiTable startState hmm coord (table : MarkovDPCell<_,_> [,]) = 
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
            table.[i,l] <- updatedCell hmm startState table currState currEmission i l
        | _, _ -> 
            printfn "Something went very wrong"
    getNextCell (numRows table) (numColumns table) coord
    |> function
        | Some newCoord ->
            fillViterbiTable startState hmm newCoord table
        | None ->
            table

let foo startState hmm cell = 
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
    cell.score * pEnd

let viterbiTraceback startState hmm table = 
    let maxCell = 
        table
        |> getLastColumn
        |> Array.maxBy (foo startState hmm)

    // should always be less than the forward probability
    let viterbiProbability = foo startState hmm maxCell

    let rec loop acc cell = 
        match cell.ancestor with
        | Some ancestor -> loop (cell :: acc) ancestor
        | None -> (cell :: acc)

    let viterbiPath = loop [] maxCell

    viterbiProbability, viterbiPath

// we are currently ignoring transition probabilities to a special end state...
let viterbi (startState : Begin<'State>) (hmm : HMM<'State, 'Emission>) (observations : 'Emission list) = 
    let prob, cellPath = 
        hmm 
        |> Map.toList 
        |> List.map fst
        |> makeDPTable observations 
        |> fillViterbiTable startState hmm (0, 0) 
        |> viterbiTraceback startState hmm
    let path = cellPath |> List.choose (fun cell -> cell.state)
    {probability = prob; path = path}
