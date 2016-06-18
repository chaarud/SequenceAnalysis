module Markov.Viterbi
    
open Types
open DPUtils

type ViterbiComputationConfiguration<'S, 'E when 'S : comparison> = 
    {
        scorer: Probability -> Probability -> Probability
        modelTransformer: HMM<'S, 'E> -> HMM<'S, 'E>
    }

//let logComputationConfiguration = 
//    {
//        scorer = (+)
//    }

let normalComputationConfiguration = 
    {
        scorer = (*)
        modelTransformer = id
    }

type ViterbiResult<'State> = 
    {
        probability : Probability
        path : 'State list
    }

let pGetToHiddenState model currState prevCell = 
    match prevCell.state with
    | Some prevState ->
        let prevInfo = Map.find prevState model.internalState
        let pTransitionToThisState = 
            prevInfo.transitions
            |> List.find (fst >> ((=) (Some currState)))
            |> snd
        pTransitionToThisState * prevCell.score
    | None ->
        model.startState 
        |> List.find (fst >> ((=) (Some currState)))
        |> snd
        //this should give 0, because prevCell.score should be 0
        //TODO can we elmiminate this and get rid of the startState parameter here
        |> (*) prevCell.score 

let updatedCell model table currState currEmission i l (scorer: Probability -> Probability -> Probability) = 
    let prevColumn = getColumnFromTable (l-1) table 
    let pEmission = 
        Map.find currState model.internalState
        |> fun i -> i.emissions
        |> List.find (fst >> ((=) currEmission))
        |> snd
    let pGetToHiddenState = pGetToHiddenState model currState
    let prevCell =
        prevColumn
        |> Array.maxBy pGetToHiddenState
    let pGetToCurrState = pGetToHiddenState prevCell
    {table.[i,l] with
        score = scorer pEmission pGetToCurrState
        ancestor = Some prevCell}

let rec fillViterbiTable model coord (table : MarkovDPCell<_,_> [,]) = 
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
            table.[i,l] <- updatedCell model table currState currEmission i l (*)
        | _, _ -> 
            printfn "Something went very wrong"
    getNextCell (numRows table) (numColumns table) coord
    |> function
        | Some newCoord ->
            fillViterbiTable model newCoord table
        | None ->
            table

let findMostLikelyCell model cell = 
    let pEnd =
        match cell.state with
        | Some lastState ->
            Map.find (lastState) model.internalState
            |> fun info -> info.transitions
            |> List.find (fst >> Option.isNone)
            |> snd
        | None ->
            // This represents the case where there are no observations.
            // The start state transitions directly to the end state.
            // Attempt to find the probability, otherwise return 0.
            List.tryFind (fst >> Option.isNone) model.startState
            |> function
                | Some (None, pBeginToEnd) -> pBeginToEnd
                | _ -> 0.
    cell.score * pEnd

let viterbiTraceback model table = 
    let maxCell = 
        table
        |> getLastColumn
        |> Array.maxBy (findMostLikelyCell model)

    // should always be less than the forward probability
    let viterbiProbability = findMostLikelyCell model maxCell

    let rec loop acc cell = 
        match cell.ancestor with
        | Some ancestor -> loop (cell :: acc) ancestor
        | None -> (cell :: acc)

    let viterbiPath = loop [] maxCell

    viterbiProbability, viterbiPath

// we are currently ignoring transition probabilities to a special end state...
let viterbi (model : HMM<'State, 'Emission>) (observations : 'Emission list) = 
    //(startState : Begin<'State>) (hmm : HMM<'State, 'Emission>) (observations : 'Emission list) = 
    let hmm = model.internalState
    let startState = model.startState
    let prob, cellPath = 
        model.internalState 
        |> Map.toList 
        |> List.map fst
        |> makeDPTable observations 
        |> fillViterbiTable model (0, 0) 
        |> viterbiTraceback model
    let path = cellPath |> List.choose (fun cell -> cell.state)
    {probability = prob; path = path}
