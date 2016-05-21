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

type HmmState<'State> = 
    | Begin
    | End 
    | Internal of 'State

// each element in 'state should appear at most once
type HMM<'State, 'Emission when 'State : comparison> = Map<HmmState<'State>, NodeInfo<'State, 'Emission>>

type ViterbiCell<'State, 'Emission> = 
    {
        score : float
        ancestor : ViterbiCell<'State, 'Emission> option
        // None is the begin state
        state : HmmState<'State>
        emission : 'Emission option
    }

let makeViterbiTable (states : HmmState<'State> list) (observations : 'Emission list) = 
    //(List.length states) - 1 because we don't need the end state
    Array2D.zeroCreate<ViterbiCell<'State, 'Emission>> ((List.length states) - 1) ((List.length observations) + 1)
    |> Array2D.mapi (fun idxState idxEmission cell ->
        {cell with
            state = if idxState = 0 then Begin else states.[idxState - 1]
            emission = if idxEmission = 0 then None else Some observations.[idxEmission - 1]
        })

let getNextViterbiCell m n (i, j) = 
    if j+1 <= n
        then Some (i, j+1)
        else 
            if i+1 <= m
                then Some (i+1, 0)
                else None

let rec fillViterbiTable (hmm : HMM<_, _>) coord (table : ViterbiCell<_,_> [,]) = 
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
        let state = table.[i,l].state
        let emission = table.[i,l].emission
        // am I slicing the right way?
        let prevColumn = Array.init (Array2D.length2 table) (fun j -> table.[i-1, j])
        match state, emission with
        // everything should match here
        | Some s, Some e ->
            let info = Map.find s hmm
            let pEmission = 
                info.emissions
                |> List.find (fst >> ((=) e))
                |> snd

            let pGetToHiddenState currState prevCell = 
                match prevCell.state with
                | Some prevState ->
                    let prevInfo = Map.find prevState hmm
                    let pTransitionToThisState = 
                        prevInfo.transitions
                        |> List.find (fst >> ((=) currState))
                        |> snd
                    pTransitionToThisState * prevCell.score
                | None ->
                    // this is wrong
                    // represent the begin state transition probabilities!
                    1. 

            let prevCell =
                prevColumn
                |> Array.maxBy (pGetToHiddenState s)
            let stuff = pGetToHiddenState s prevCell

            let thisCell = 
                {
                    table.[i,l] with
                        score = pEmission * stuff
                        ancestor = Some prevCell
                }
            table.[i,l] <- thisCell
        | _, _ ->
            // something went wrong
            ()

    getNextViterbiCell (Array2D.length1 table) (Array2D.length2 table) coord
    |> function
        | Some newCoord ->
            fillViterbiTable hmm newCoord table
        | None ->
            table

let viterbiTraceback table = 
    let L = Array2D.length1 table
    let lastColumn = Array.init (Array2D.length2 table) (fun j -> table.[L, j])
    let maxCell = Array.maxBy (fun cell -> cell.score) lastColumn

    let rec loop acc cell = 
        match cell.ancestor with
        | Some ancestor ->
            loop (ancestor :: acc) ancestor
        | None ->
            acc

    loop [] maxCell

let Viterbi (hmm : HMM<'State, 'Emission>) (observations : 'Emission list) = 
    let states : HmmState<'State> list = hmm |> Map.toList |> List.map fst
    makeViterbiTable states observations 
    |> fillViterbiTable hmm (0, 0) 
    |> viterbiTraceback
    |> List.map (fun cell -> cell.state)

