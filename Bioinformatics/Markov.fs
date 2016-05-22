module Bioinformatics.Markov

//TODO add validation for probability distributions, other invariants
//or reflect it in tests?

// enforce that it's between 0 and 1?
type Probability = double

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
        score : double
        ancestor : ViterbiCell<'State, 'Emission> option
        // None is the begin state
        state : 'State option
        emission : 'Emission option
    }

let makeViterbiTable (states : 'State list) (observations : 'Emission list) = 
    Array2D.zeroCreate<ViterbiCell<'State, 'Emission>> ((List.length states) + 1) ((List.length observations) + 1)
    |> Array2D.mapi (fun idxState idxEmission cell ->
        {
            score = -1.0 // can viterbi generate negative scores? No
            ancestor = None
            state = if idxState = 0 then None else Some states.[idxState - 1]
            emission = if idxEmission = 0 then None else Some observations.[idxEmission - 1]
        })

let getNextViterbiCell m n (i, j) = 
    if j+1 < n
        then Some (i, j+1)
        else 
            if i+1 < m
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
        |> (*) prevCell.score //this will give 0, because prevCell.score should be 0

let updatedCell hmm startState table currState currEmission i l = 
    let prevColumn = Array.init (Array2D.length1 table) (fun j -> table.[j, l-1])
    let pEmission = 
        Map.find currState hmm
        |> fun i -> i.emissions
        |> List.find (fst >> ((=) currEmission))
        |> snd
    let pGetToHiddenState = pGetToHiddenState hmm startState currState
    let prevCell =
        prevColumn
        |> Array.maxBy pGetToHiddenState
//    printfn "=====================\n\nPREVIOUS COLUMN: %A" prevColumn
//    printfn "\n~~~~~~~~~~~~~~~~~~\nPREVIOUS CELL: %A\n\n==========================" prevCell
//    let prevCellFromTable = 
//        let positionInColumn = Array.findIndex ((=) prevCell) prevColumn
//        table.[positionInColumn, l-1]
    let pGetToCurrState = pGetToHiddenState prevCell
    {table.[i,l] with
        score = pEmission * pGetToCurrState
        ancestor = Some prevCell}//FromTable}

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
    let L = Array2D.length2 table
    //Array2Ds are indexed with length1 = length of columns, length2 = length of row
    let lastColumn = Array.init (Array2D.length1 table) (fun j -> table.[j, L-1])
    let maxCell = Array.maxBy (fun cell -> cell.score) lastColumn

//    printfn "starting traceback..."
//    printfn "last column: %A" lastColumn
//    printfn "max cell in last column: %A" maxCell

    let rec loop acc cell = 
        match cell.ancestor with
        | Some ancestor -> loop (cell :: acc) ancestor
        | None -> (cell :: acc)

    loop [] maxCell

// we are currently ignoring transition probabilities to a special end state...
let Viterbi (startState : Begin<'State>) (hmm : HMM<'State, 'Emission>) (observations : 'Emission list) = 
    let states : 'State list = hmm |> Map.toList |> List.map fst
    makeViterbiTable states observations 
//    |> fun table -> 
//        printfn "table.length1: %A" <| Array2D.length1 table
//        printfn "table.length2: %A" <| Array2D.length2 table
//        printfn "table: %A" <| table
//        table
    |> fillViterbiTable startState hmm (0, 0) 
//    |> fun table ->
//        printfn "after filling"
//        printfn "table.scores: %A" <| Array2D.map (fun cell -> cell.score) table
//        printfn "table: %A" <| table
//        table
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
let tryFindWhileFolding (predicate : 'State -> bool) (folder : 'State -> 'T -> 'State) initialState (xs : 'T list) : 'T option = 
    let rec loop innerState xs = 
//        if we didn't want to return an option, we'd have to throw on an empty input
//        match xs with
//        | x :: [] -> x
//        | x :: xs ->
//            let newState = folder innerState x
//            if predicate newState 
//                then x
//                else loop newState xs
//        | [] -> //throw?
        match xs with
        //This would ensure that we would return something always on a non empty list
        //if we can trust that the input list is a probability distribution, this should be OK...
        //| x :: [] -> Some x 
        | x :: xs ->
            let newState = folder innerState x
            if predicate newState 
                then Some x
                else loop newState xs
        | _ -> None

    loop initialState xs

let rnd = new System.Random ()

let getNextTupleFromProbabilityDistribution (xs : _ list) = 
    let random = rnd.NextDouble ()
    // ((>) foo) is confusing - it means (fun x -> foo > x), which is not what we want. It does not mean "greater than foo", it means "foo greater than".
    xs |> tryFindWhileFolding (fun x -> x > random) (fun state elem -> state + (snd elem)) 0.0

let chooseNextEventFromProbabilityDistribution events =
    events
    |> getNextTupleFromProbabilityDistribution 
    |> Option.get //bad, fix this
    |> fst

//let chooseNextEventFromProbabilityDistribution =
//    getNextTupleFromProbabilityDistribution 
//    >> Option.get //bad, fix this
//    >> fst

let getEmission = chooseNextEventFromProbabilityDistribution

let getNextStateInfo hmm transitions = 
    transitions
    |> chooseNextEventFromProbabilityDistribution 
    |> fun state -> Map.find state hmm 
    |> fun info -> info.emissions, info.transitions

let walkTheChain hmm startState n = 
    let rec loop (emissions, transitions) acc = function
        | i when i <= 0 ->
            List.rev acc
        | i ->
            let emission = getEmission emissions
            let nextStateInfo = getNextStateInfo hmm transitions
            loop nextStateInfo (emission :: acc) (i-1) 

    let firstStateInfo = getNextStateInfo hmm startState
    loop firstStateInfo [] n



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

let exampleHmm = [(Fair, fairInfo); (Unfair, unfairInfo)] |> Map.ofList

let exampleStartState : Begin<Dice> = [(Unfair, 0.2); (Fair, 0.8)]

(*
Do something
*)
let decodeOnce () = 
    let steps = 10
    let observation = walkTheChain exampleHmm exampleStartState steps
    printfn "walking for %i steps: %A" steps observation

    let viterbiDecoding = Viterbi exampleStartState exampleHmm observation
    printfn "viterbi decoding of the observation: %A" viterbiDecoding


let doSomething () = 
    [1..10] |> List.iter (fun _ -> decodeOnce ())