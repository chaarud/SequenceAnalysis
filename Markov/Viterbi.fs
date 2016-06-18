module Markov.Viterbi
    
open Types
open DPUtils

let logProb xs = 
    List.map (fun (a, b) -> (a, log b)) xs

let logHmm hmm = 
    {
        startState = logProb hmm.startState
        internalState = 
            hmm.internalState 
            |> Map.map (fun _key nodeInfo ->
                {
                    emissions = logProb nodeInfo.emissions
                    transitions = logProb nodeInfo.transitions
                })
    }

type ViterbiComputationConfiguration<'S, 'E when 'S : comparison> = 
    {
        topRightScore: Probability
        zerothRowAndColumnScore: Probability
        combinator: Probability -> Probability -> Probability
        modelTransformer: HMM<'S, 'E> -> HMM<'S, 'E>
    }

//TODO fix this odd behavior: if I try to say double(-infinity) I run into issues
let logComputationConfiguration = 
    {
        topRightScore = 0.
        zerothRowAndColumnScore = System.Double.NegativeInfinity //double (-infinity)
        combinator = (+)
        modelTransformer = logHmm
    }

let normalComputationConfiguration = 
    {
        topRightScore = 1.
        zerothRowAndColumnScore = 0.
        combinator = (*)
        modelTransformer = id
    }

type ViterbiResult<'State> = 
    {
        probability : Probability
        path : 'State list
    }

let pGetToHiddenState combinator model currState prevCell = 
    match prevCell.state with
    | Some prevState ->
        let prevInfo = Map.find prevState model.internalState
        let pTransitionToThisState = 
            prevInfo.transitions
            |> List.find (fst >> ((=) (Some currState)))
            |> snd
        combinator prevCell.score pTransitionToThisState
    | None ->
        model.startState 
        |> List.find (fst >> ((=) (Some currState)))
        |> snd
        //this should give 0, because prevCell.score should be 0
        //TODO can this be eliminated
        |> combinator prevCell.score 

let updatedCell combinator model table currState currEmission i l = 
    let prevColumn = getColumnFromTable (l-1) table 
    let pEmission = 
        Map.find currState model.internalState
        |> fun i -> i.emissions
        |> List.find (fst >> ((=) currEmission))
        |> snd
    let pGetToHiddenState = pGetToHiddenState combinator model currState
    let prevCell =
        prevColumn
        |> Array.maxBy pGetToHiddenState
    let pGetToCurrState = pGetToHiddenState prevCell
    {table.[i,l] with
        score = combinator pEmission pGetToCurrState
        ancestor = Some prevCell}

let rec fillViterbiTable config model coord (table : MarkovDPCell<_,_> [,]) = 
    match coord with
    | (0, 0) ->
        table.[0,0] <- {table.[0,0] with score = config.topRightScore}
    | (0, k) ->
        table.[0,k] <- {table.[0,k] with score = config.zerothRowAndColumnScore} 
    | (x, 0) ->
        table.[x,0] <- {table.[x,0] with score = config.zerothRowAndColumnScore}
    | (i, l) ->
        match table.[i,l].state, table.[i,l].emission with
        | Some currState, Some currEmission ->
            table.[i,l] <- updatedCell config.combinator model table currState currEmission i l
        | _, _ -> 
            printfn "Something went very wrong"
    getNextCell (numRows table) (numColumns table) coord
    |> function
        | Some newCoord ->
            fillViterbiTable config model newCoord table
        | None ->
            table

let findMostLikelyCell combinator model cell = 
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
    combinator cell.score pEnd

let viterbiTraceback combinator model table = 
    let maxCell = 
        table
        |> getLastColumn
        |> Array.maxBy (findMostLikelyCell combinator model)

    // should always be less than the forward probability
    let viterbiProbability = findMostLikelyCell combinator model maxCell

    let rec loop acc cell = 
        match cell.ancestor with
        | Some ancestor -> loop (cell :: acc) ancestor
        | None -> (cell :: acc)

    let viterbiPath = loop [] maxCell

    viterbiProbability, viterbiPath

let viterbi (config: ViterbiComputationConfiguration<'State, 'Emission>) (originalModel : HMM<'State, 'Emission>) (observations : 'Emission list) = 
    let model = config.modelTransformer originalModel

    //printfn "model with log probabilities: %A" model

    let tbl = 
        model.internalState 
        |> Map.toList 
        |> List.map fst
        |> makeDPTable observations 
        |> fillViterbiTable config model (0, 0) 

    //printfn "table: \n%A" <| Array2D.map (fun cell -> cell.score) tbl

    let prob, cellPath = 
        tbl
        |> viterbiTraceback config.combinator model

    //printfn "cell path: %A" cellPath

    let path = cellPath |> List.choose (fun cell -> cell.state)
    {probability = prob; path = path}
