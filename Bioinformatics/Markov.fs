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

(*
The Viterbi Algorithm
*)

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

// we are currently ignoring transition probabilities to a special end state...
let Viterbi (startState : Begin<'State>) (hmm : HMM<'State, 'Emission>) (observations : 'Emission list) = 
    let states : 'State list = hmm |> Map.toList |> List.map fst
    makeViterbiTable states observations 
    |> fillViterbiTable startState hmm (0, 0) 
    |> viterbiTraceback
    |> List.map (fun cell -> cell.state)




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
                |> List.find (fst >> ((=) currState)) 
                |> snd
            | None ->
                startState
                |> List.find (fst >> ((=) currState))
                |> snd
        pCell.score * pTransition)

let updatedForwardCell hmm startState table currState currEmission i l = 
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

let terminate table = 
    let size = Array2D.length2 table
    let finalColumn = Array.init size (fun j -> table.[size, j])
    Array.sumBy (fun cell -> cell.score)

//this ignores transition probabilities to a special end state
let Forward (startState : Begin<'State>) (hmm : HMM<'State, 'Emission>) (observations : 'Emission list) = 
    let states : 'State list = hmm |> Map.toList |> List.map fst
    makeViterbiTable states observations
    |> fillForwardTable startState hmm (0, 0)
    |> terminate

(*
Walk through an HMM for n steps and get an observation
*)
let foldUntil (predicate : 'State -> bool) (folder : 'State -> 'T -> 'State) initialState (xs : 'T list) : 'T option = 

    let rec loop xs innerState outerState = 
        match outerState with
        | None ->
            match xs with
            | x :: xs ->
                if predicate innerState 
                    then (Some x)
                    else (None)
                |> loop xs (folder innerState x)
            | _ -> None
        | Some outerState -> Some outerState

    loop xs initialState None

let getNext (xs : _ list) = 
    let rnd = new System.Random ()
    let random = rnd.NextDouble ()
    xs |> foldUntil ((>) random) (fun state elem -> state + (snd elem)) 0.0

let getNextStateInfo hmm transitions : NodeInfo<_,_> = 
    getNext transitions
    |> Option.get
    |> fst
    |> fun state -> Map.find state hmm

let getEmission emissions = 
    getNext emissions
    |> Option.get
    |> fst

let walkTheChain hmm startState n = 
    let firstStateInfo = getNextStateInfo hmm startState

    let rec loop emissions transitions acc = function
        | i when i <= 0 ->
            List.rev acc
        | i ->
            let emission = getEmission emissions
            let nextStateInfo = getNextStateInfo hmm transitions
            loop nextStateInfo.emissions nextStateInfo.transitions (emission :: acc) (i-1) 

    loop firstStateInfo.emissions firstStateInfo.transitions [] n



(*
Example: The occasionally dishonest casino
*)

type Dice = Fair | Unfair

type DiceRoll = One | Two | Three | Four | Five | Six

let diceRolls = 
    Microsoft.FSharp.Reflection.FSharpType.GetUnionCases typeof<DiceRoll>
    |> Array.map (fun case -> Microsoft.FSharp.Reflection.FSharpValue.MakeUnion(case, [||]) :?> DiceRoll)

let oneSixth = 1./6.

let fairInfo = 
    {
        transitions = [(Unfair, 0.05); (Fair, 0.95)]
        emissions = diceRolls |> Array.toList |> List.map (fun dr -> (dr, oneSixth))
    }

let unfairInfo = 
    {
        transitions = [(Unfair, 0.9); (Fair, 0.1)]
        emissions = [(One, 0.1); (Two, 0.1); (Three, 0.1); (Four, 0.1); (Five, 0.1); (Six, 0.5)]
    }

let exampleHmm : HMM<Dice, DiceRoll> = [(Fair, fairInfo); (Unfair, unfairInfo)] |> Map.ofList

let exampleStartState : Begin<Dice> = [(Unfair, 0.2); (Fair, 0.8)]

(*
Do something
*)
let doSomething () = walkTheChain exampleHmm exampleStartState 100