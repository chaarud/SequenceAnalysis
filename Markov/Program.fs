module Markov.Runner

open Observe
open Viterbi
open Forward
open Backward
open Examples
open PosteriorProbability

// Run through an example
let decodeOnce () = 
    let steps = 10
    let observation = observe exampleHmm steps
    printfn "walking for %i steps: %A" steps observation

    let viterbiResult = viterbi exampleHmm observation
    printfn "viterbi decoding of the observation     : %A" viterbiResult.path

    let posteriorDecoding = posteriorDecoding exampleStartState exampleHmm observation
    printfn "posterior decoding of the observation   : %A" posteriorDecoding

    printfn "viterbi probability    : %A" viterbiResult.probability

    let forwardResult = forward exampleHmm observation
    printfn "forward probability    : %A" forwardResult.probability

    let backwardResult = backward exampleHmm observation
    printfn "backward probability   : %A" backwardResult.probability


    printfn "\n"

let doSomething () = 
    //decodeOnce ()
    [1..10] |> List.iter (fun _ -> decodeOnce ())

[<EntryPoint>]
let main argv = 
    0 // return an integer exit code

