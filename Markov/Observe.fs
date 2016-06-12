module Markov.Observe

open Types

// Generate observations
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

let observe hmm startState n = 
    let rec loop acc (emissions, transitions) = function
        | i when i <= 0 ->
            List.rev acc
        | i ->
            let emission = getEmission emissions
            getNextStateInfo hmm transitions
            |> function
                | Some nextStateInfo -> loop (emission :: acc) nextStateInfo (i-1) 
                | None -> List.rev (emission :: acc)

    getNextStateInfo hmm startState
    |> Option.fold (fun acc firstState -> loop acc firstState n) []
