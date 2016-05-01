open Bioinformatics.Alignment

[<EntryPoint>]                    
let main argv = 
//    let getNext = getNextCell 10 10
//    let mutable next = getNext (0, 0)
//    let mutable n = 1
//    while next <> None && n < 100 do
//        printfn "%A" next
//        next <- getNext (Option.get next)
//        n <- n+1

    let p : NWParams = 
        {
            d = 5
            s = Map.ofList [(('a', 'b'), 1); (('b', 'a'), 1); (('a', 'a'), 10); (('b', 'b'), 10)]
        }

    let x = NeedlemanWunsch p "abbababbabababba" "abbabababa"
    printfn "%A" x

    0
