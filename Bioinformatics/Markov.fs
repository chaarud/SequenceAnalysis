module Bioinformatics.Markov

//TODO add validation for probability distributions, other invariants
//or reflect it in tests?

// enforce that it's between 0 and 1?
type Probability = float

type NodeInfo<'State, 'Emission> = 
    {
        // enforce these probabilities sum to 1
        emissions : ('Emission * Probability) list
        // enforce these probabilities sum to 1
        transitions : ('State * Probability) list
    }

// each element in 'state should appear at most once
type HMM<'State, 'Emission when 'State : comparison> = Map<'State, NodeInfo<'State, 'Emission>>

type Begin<'State> = ('State * Probability) list

type ViterbiCell<'State, 'Emission> = 
    {
        score : float
        ancestor : ViterbiCell<'State, 'Emission> option
        // None is the begin state
        state : 'State option
        emission : 'Emission option
    }

let makeViterbiTable (states : 'State list) (observations : 'Emission list) = 
    Array2D.zeroCreate<ViterbiCell<'State, 'Emission>> ((List.length states) + 1) ((List.length observations) + 1)
    |> Array2D.mapi (fun idxState idxEmission cell ->
        {cell with
            state = if idxState = 0 then None else Some states.[idxState - 1]
            emission = if idxEmission = 0 then None else Some observations.[idxEmission - 1]
        })

let getNextViterbiCell m n (i, j) = 
    if j+1 <= n
        then Some (i, j+1)
        else 
            if i+1 <= m
                then Some (i+1, 0)
                else None

let pGetToHiddenState hmm startState currState prevCell = 
    match prevCell.state with
    | Some prevState ->
        let prevInfo = Map.find prevState hmm
        let pTransitionToThisState = 
            prevInfo.transitions
            |> List.find (fst >> ((=) currState))
            |> snd
        pTransitionToThisState * prevCell.score
    | None ->
        startState 
        |> List.find (fst >> ((=) currState))
        |> snd

let updatedCell hmm startState table currState currEmission i l = 
    // am I slicing the right way?
    let prevColumn = Array.init (Array2D.length2 table) (fun j -> table.[i-1, j])
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

let rec fillViterbiTable startState hmm coord (table : ViterbiCell<_,_> [,]) = 
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
            table.[i,l] <- updatedCell hmm startState table currState currEmission i l
        | _, _ -> 
            printf "Something went very wrong"
    getNextViterbiCell (Array2D.length1 table) (Array2D.length2 table) coord
    |> function
        | Some newCoord ->
            fillViterbiTable startState hmm newCoord table
        | None ->
            table

let viterbiTraceback table = 
    let L = Array2D.length1 table
    let lastColumn = Array.init (Array2D.length2 table) (fun j -> table.[L, j])
    let maxCell = Array.maxBy (fun cell -> cell.score) lastColumn

    let rec loop acc cell = 
        match cell.ancestor with
        | Some ancestor -> loop (ancestor :: acc) ancestor
        | None -> acc

    loop [] maxCell

let Viterbi (startState : Begin<'State>) (hmm : HMM<'State, 'Emission>) (observations : 'Emission list) = 
    let states : 'State list = hmm |> Map.toList |> List.map fst
    makeViterbiTable states observations 
    |> fillViterbiTable startState hmm (0, 0) 
    |> viterbiTraceback
    |> List.map (fun cell -> cell.state)

let sumForwards (hmm: HMM<_,_>) startState prevColumn currState = 
    prevColumn
    |> Array.sumBy (fun pCell ->
        let pTransition = 
            match pCell.state with
            | Some pState ->
                Map.find pState hmm
                |> fun i -> i.transitions
                |> List.find (fst >> ((=) currState)) 
                |> snd
            | None ->
                startState
                |> List.find (fst >> ((=) currState))
                |> snd
        pCell.score * pTransition)

let updatedForwardCell hmm startState table (currState) currEmission i l = 
    // am I slicing the right way?
    let prevColumn = Array.init (Array2D.length2 table) (fun j -> table.[i-1, j])
    let pEmission = 
        Map.find currState hmm
        |> fun i -> i.emissions
        |> List.find (fst >> ((=) currEmission))
        |> snd
    let sumThing = sumForwards hmm startState prevColumn currState
    {
        table.[i,l] with
            score = pEmission * sumThing
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

let Forward (startState : Begin<'State>) (hmm : HMM<'State, 'Emission>) (observations : 'Emission list) = 
    let states : 'State list = hmm |> Map.toList |> List.map fst
    makeViterbiTable states observations
    |> fillForwardTable startState hmm (0, 0)
