module Markov.Examples

open Types

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
