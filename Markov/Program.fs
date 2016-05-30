module Markov.Runner

open Observe
open Viterbi
open Forward
open Backward
open Examples

(*


Do something


*)
let decodeOnce () = 
    let steps = 10
    let observation = observe exampleHmm exampleStartState steps
    printfn "walking for %i steps: %A" steps observation

    let viterbiDecoding = viterbi exampleStartState exampleHmm observation
    printfn "viterbi decoding of the observation: %A" viterbiDecoding

    let forwardProbability = forward exampleStartState exampleHmm observation
    printfn "forward probability of the observation: %A" forwardProbability

    printfn "\n"

let doSomething () = 
    //decodeOnce ()
    [1..10] |> List.iter (fun _ -> decodeOnce ())

[<EntryPoint>]
let main argv = 
    0 // return an integer exit code

