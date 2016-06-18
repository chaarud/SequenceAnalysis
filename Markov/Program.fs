module Markov.Runner

open Observe
open Viterbi
open Forward
open Backward
open Examples
open PosteriorProbability

let compareViterbis () = 
    let steps = 1000
    let observation = observe exampleHmm steps
    printfn "walking for %i steps: %A" steps observation

    let normalViterbiResult = viterbi normalComputationConfiguration exampleHmm observation
    let logViterbiResult = viterbi logComputationConfiguration exampleHmm observation

    printfn "\n"

    printfn "NORMAL viterbi decoding of the observation     : %A" normalViterbiResult.path
    printfn "LOG    viterbi decoding of the observation     : %A" logViterbiResult.path

    printfn "\n"

    //printfn "NORMAL viterbi probability      : %A" normalViterbiResult.probability
    printfn "ln(normal) viterbi probability    : %A" <| log normalViterbiResult.probability
    printfn "log-space  viterbi probability    : %A" logViterbiResult.probability

let decodeOnce () = 
    let steps = 10
    let observation = observe exampleHmm steps
    printfn "walking for %i steps: %A" steps observation

    let viterbiResult = viterbi normalComputationConfiguration exampleHmm observation
    printfn "viterbi decoding of the observation     : %A" viterbiResult.path

    let posteriorDecoding = posteriorDecoding exampleHmm observation
    printfn "posterior decoding of the observation   : %A" posteriorDecoding

    printfn "viterbi probability    : %A" viterbiResult.probability

    let forwardResult = forward exampleHmm observation
    printfn "forward probability    : %A" forwardResult.probability

    let backwardResult = backward exampleHmm observation
    printfn "backward probability   : %A" backwardResult.probability

    printfn "\n"

let doSomething () = 
    compareViterbis ()
    //decodeOnce ()
    //[1..10] |> List.iter (fun _ -> decodeOnce ())

[<EntryPoint>]
let main argv = 
    0 // return an integer exit code

