module Markov.Runner

open Observe
open Viterbi
open Forward
open Backward
open Examples

// Run through an example
let decodeOnce () = 
    let steps = 10
    let observation = observe exampleHmm exampleStartState steps
    printfn "walking for %i steps: %A" steps observation

    let viterbiResult = viterbi exampleStartState exampleHmm observation
    printfn "viterbi decoding of the observation: %A" viterbiResult.path

    let forwardResult = forward exampleStartState exampleHmm observation
    printfn "forward probability of the observation: %A" forwardResult.probability

    let backwardResult = backward exampleStartState exampleHmm observation
    printfn "backward probability of the observation: %A" backwardResult.probability

    printfn "\n"

let doSomething () = 
    //decodeOnce ()
    [1..10] |> List.iter (fun _ -> decodeOnce ())

[<EntryPoint>]
let main argv = 
    0 // return an integer exit code

