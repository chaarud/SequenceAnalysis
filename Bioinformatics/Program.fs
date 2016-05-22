open Bioinformatics.Alignment
open Bioinformatics.Grammar
open Bioinformatics.Markov

[<EntryPoint>]                    
let main argv = 

//    let p : AlignmentParams = 
//        {
//            d = 5
//            s = Map.ofList [(('a', 'b'), 1); (('b', 'a'), 1); (('a', 'a'), 10); (('b', 'b'), 10)]
//        }
//
//    let s1, s2 = "abbababbabababba", "abbabababa"
//    let sw = SmithWaterman p s1 s2
//    printfn "Smith Waterman: %A" sw
//    let nw = NeedlemanWunsch p s1 s2
//    printfn "Needleman Wunsch: %A" nw

//    parseSomething ()

    let foo = [0.09; 0.2; 0.05; 0.13; 0.15; 0.3]
    let thresh = 0.5
    let bar = foldUntil ((>) thresh) (+) 0.0 foo
    printfn "%A" bar

//    let x = Bioinformatics.Markov.doSomething ()
//    printfn "%A" x

    0
