open Bioinformatics.Alignment
open Bioinformatics.Grammar

[<EntryPoint>]                    
let main argv = 

//    let p : AlignmentParams = 
//        {
//            d = 5
//            s = Map.ofList [(('a', 'b'), 1); (('b', 'a'), 1); (('a', 'a'), 10); (('b', 'b'), 10)]
//        }
//
//    let x = NeedlemanWunsch p "abbababbabababba" "abbabababa"
//    printfn "%A" x

    printfn "hello..."
    printfn "%A" (parseSomething())
//    printfn "%A" (getDerivation())
    
    printfn "goodbye"

    0
