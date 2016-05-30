module Bioinformatics.Markov

//unused
//needs to be validated/enforced
//first todo w/reflection for finite discrete countable algebraic data 'T
type Distribution<'T> = 'T -> float

//TODO add validation for probability distributions, other invariants
//or reflect it in property based tests?

// enforce that it's between 0 and 1?
type Probability = double

type NodeInfo<'State, 'Emission> = 
    {
        // enforce these probabilities sum to 1
        emissions : ('Emission * Probability) list
        // enforce these probabilities sum to 1
        transitions : (('State option) * Probability) list
    }

// each element in 'state should appear at most once
type HMM<'State, 'Emission when 'State : comparison> = Map<'State, NodeInfo<'State, 'Emission>>

//'State option because the observations could be empty (ie start -> end) I suppose.
type Begin<'State> = (('State option) * Probability) list

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
            // should the score be an option as well
            score = -1.0 // can viterbi generate negative scores? No
            ancestor = None
            state = if idxState = 0 then None else Some states.[idxState - 1]
            emission = if idxEmission = 0 then None else Some observations.[idxEmission - 1]
        })

//Array2Ds are indexed with length1 = length of columns, length2 = length of row
let getColumnFromTable l table = 
    Array.init (Array2D.length1 table) (fun j -> table.[j, l])

let getLastColumn table = 
    let numColumns = Array2D.length2 table
    getColumnFromTable (numColumns-1) table

//I think this traverses by row...
//let getNextViterbiCell m n (i, j) = 
//    if j+1 < n
//        then Some (i, j+1)
//        else 
//            if i+1 < m
//                then Some (i+1, 0)
//                else None

let getNextViterbiCell m n (i, j) = 
    if i+1 < m
        then Some (i+1, j)
        else
            if j+1 < n
                then Some (0, j+1)
                else None

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
        //can we just elmiminate this entirely, and get rid of the startState parameter here?
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
    let lastColumn = getLastColumn table
    let maxCell = Array.maxBy (fun cell -> cell.score) lastColumn

    let rec loop acc cell = 
        match cell.ancestor with
        | Some ancestor -> loop (cell :: acc) ancestor
        | None -> (cell :: acc)

    loop [] maxCell

// we are currently ignoring transition probabilities to a special end state...
let Viterbi (startState : Begin<'State>) (hmm : HMM<'State, 'Emission>) (observations : 'Emission list) = 
    let states : 'State list = hmm |> Map.toList |> List.map fst
    makeViterbiTable states observations 
    |> fillViterbiTable startState hmm (0, 0) 
    |> viterbiTraceback
    |> List.choose (fun cell -> cell.state)

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

let terminateForward table = 
    //define with composition if it doesn't result in crazy errors
    table
    |> getLastColumn
    |> Array.sumBy (fun cell -> cell.score)

//this ignores transition probabilities to a special end state
let Forward (startState : Begin<'State>) (hmm : HMM<'State, 'Emission>) (observations : 'Emission list) = 
    let states : 'State list = hmm |> Map.toList |> List.map fst
    makeViterbiTable states observations
    |> fillForwardTable startState hmm (0, 0)
    |> terminateForward

(*


The Backward Algorithm


*)
//let Backward (startState : Begin<'State>) (hmm : HMM<'State, 'Emission>) (observations : 'Emission list) = 
//    let states : 'State list = hmm |> Map.toList |> List.map fst
//    makeViterbiTable states observations
//    |> fillBackwardTable startState hmm (0,0)
//    |> terminateBackward

(*


Walk through an HMM for n steps and get an observation


*)
let tryFindWhileFolding (predicate : 'State -> bool) (folder : 'State -> 'T -> 'State) initialState (xs : 'T list) : 'T option = 
    let rec loop innerState xs = 
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
    |> Option.map (fun nextState -> 
        Map.find nextState hmm 
        |> fun info -> info.emissions, info.transitions)

let walkTheChain hmm startState n = 
    let rec loop acc (emissions, transitions) = function
        | i when i <= 0 ->
            List.rev acc
        | i ->
            let emission = getEmission emissions
            getNextStateInfo hmm transitions
            |> function
                | Some nextStateInfo -> loop (emission :: acc) nextStateInfo (i-1) 
                | None -> List.rev acc

    getNextStateInfo hmm startState
    |> Option.fold (fun acc firstState -> loop acc firstState n) []

(*


Example: The occasionally dishonest casino


*)
type Dice = Fair | Unfair

type DiceRoll = One | Two | Three | Four | Five | Six

let unionCases (typ : System.Type) = 
    Microsoft.FSharp.Reflection.FSharpType.GetUnionCases typeof<DiceRoll>
    |> Array.map (fun case -> Microsoft.FSharp.Reflection.FSharpValue.MakeUnion(case, [||]) :?> DiceRoll)

let oneSixth = 1./6.

let fairEmissionDistribution = 
    typeof<DiceRoll> |> unionCases |> Array.toList |> List.map (fun dr -> (dr, oneSixth))

let unfairEmissionDistribution = 
    [(One, 0.1); (Two, 0.1); (Three, 0.1); (Four, 0.1); (Five, 0.1); (Six, 0.5)]

let exampleStartState : Begin<Dice> = [(Some Unfair, 0.2); (Some Fair, 0.8)]

let fairInfo = 
    {
        transitions = [(Some Fair, 0.94); (Some Unfair, 0.05); (None, 0.01)]
        emissions = fairEmissionDistribution
    }

let unfairInfo =
    {
        transitions = [(Some Fair, 0.1); (Some Unfair, 0.89); (None, 0.01)]
        emissions = unfairEmissionDistribution
    }

let exampleHmm = [(Fair, fairInfo); (Unfair, unfairInfo)] |> Map.ofList

(*


Do something


*)
let decodeOnce () = 
    let steps = 10
    let observation = walkTheChain exampleHmm exampleStartState steps
    printfn "walking for %i steps: %A" steps observation

    let viterbiDecoding = Viterbi exampleStartState exampleHmm observation
    printfn "viterbi decoding of the observation: %A" viterbiDecoding

    let forwardProbability = Forward exampleStartState exampleHmm observation
    printfn "forward probability of the observation: %A" forwardProbability

    printfn "\n"


let doSomething () = 
//    decodeOnce ()
    [1..10] |> List.iter (fun _ -> decodeOnce ())