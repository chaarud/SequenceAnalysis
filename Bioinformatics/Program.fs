open Bioinformatics.Alignment

[<EntryPoint>]                    
let main argv = 

    let p : NWParams = 
        {
            d = 5
            s = Map.ofList [(('a', 'b'), 1); (('b', 'a'), 1); (('a', 'a'), 10); (('b', 'b'), 10)]
        }

    let x = NeedlemanWunsch p "abbababbabababba" "abbabababa"
    printfn "%A" x

    0
