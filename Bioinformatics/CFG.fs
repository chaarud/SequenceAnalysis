module Bioinformatics.Grammar

open System.Collections.Generic

//// None represents epsilon. Maybe the empty list could represent epsilon? But this way it's more explicit.
//type TerminalSymbol<'T> = 'T option
//type TerminalSymbols<'T when 'T : comparison> = Set<TerminalSymbol<'T>>
//
//type NonterminalSymbols<'NT when 'NT : comparison> = Set<'NT>

type Symbol<'T, 'NT> =
    | Terminal of 'T
    | Nonterminal of 'NT

type ContextFreeRules<'T, 'NT when 'NT : comparison> = ('NT * Symbol<'T, 'NT> list) list

type CFG<'T, 'NT when 'T : comparison and 'NT : comparison> = 
//    Symbol<'T, 'NT> * ContextFreeRules<'T, 'NT> // how to ensure the start symbol is a Nonterminal?
//    TerminalSymbols<'T> * NonterminalSymbols<'NT> * 'NT * ContextFreeRules<'T, 'NT>
    'NT * ContextFreeRules<'T, 'NT>

type Decision = Accept | Reject

let parse (start, rules) input = 

    let stack = new Stack<Symbol<'a, 'b>> ()
    stack.Push <| Nonterminal start

    let rec loop (stack : Stack<_>) input = 
        if stack.Count = 0 then Accept
        else
            match stack.Pop () with
            | Nonterminal nt -> 
                let productions = 
                    rules
                    |> List.filter (fst>>((=) nt)) 
                    |> List.map snd
                    |> List.map List.rev

                let listifiedStack = stack.ToArray() |> Array.toList

                let matchTerminal = function
                    | Terminal _ -> true
                    | _ -> false

                let matchNonterminal = function   //((<>) matchTerminal)
                    | Nonterminal _ -> true
                    | _ -> false

                let splitWith predicate xs =
                    (List.takeWhile predicate xs, List.skipWhile predicate xs)

                let windowize production = 
                    let rec loop acc production = 
                        match production with
                        | [] -> acc
                        | _ ->
                            let window = List.takeWhile matchTerminal production
                            let remainingList =
                                production 
                                |> List.skipWhile matchTerminal
                                |> List.skipWhile matchNonterminal
                            let acc = window :: acc
                            loop acc remainingList
                    let windows = loop [] production
                    List.rev windows
                        
                let startsWith prefix xs = 
                    let start = List.truncate (List.length prefix) xs
                    start = prefix

                let rec matchWindow input window = 
                    match startsWith window input with
                    | true ->
                        let newInput = List.skip (List.length window) input
                        Some newInput
                    | false -> 
                        if List.length (List.tail input) < List.length window 
                            then None
                            else matchWindow (List.tail input) window

                let predict productions input : (_ option) = 
                    // productions is an already-filtered list of productions
                    // so they all start with the same nonterminal

                    let rec loop input windows = 
                        match windows with
                        | [] ->
                            true
                        | thisWindow :: remainingWindows ->
                            printfn "thiswindow: %A" thisWindow
                            printfn "remainingWindows: %A" remainingWindows
                            let newInput = matchWindow input thisWindow
                            printfn "new input: %A" newInput
                            match newInput with
                            | Some newInput ->
                                loop newInput remainingWindows
                            | None ->
                                false

                    let rec outerLoop productions input = 
                        List.tryFind (fun production ->
                            let windows = windowize production
                            printfn "windows: %A" windows
                            printfn "input from outerLoop : %A" input
                            loop input windows) productions

                    outerLoop productions input

                predict productions input
                |> function
                    | Some production ->
                        // production |> List.rev |> List.iter (stack.Push)
                        production |> List.iter (stack.Push)
                        loop stack input
                    | None -> Reject
                    
            | (Terminal _) as sym -> 
                if sym = List.head input
                    then loop stack (List.tail input)
                    else Reject

    loop stack input

let produceRandom ((start, rules) : CFG<_, _>) =

    let leftmostNonterminal = 
        List.indexed
        >> List.tryPick (function
            | (idx, Nonterminal sym) -> Some (idx, sym)
            | _ -> None)

    let getRandomProduction nonterminal rules =
        let getRandom (l : _ list) =  
            let rnd = System.Random()  
            List.item (rnd.Next (l.Length)) l
        
        rules
        |> List.filter (fst >> ((=) nonterminal))
        |> getRandom
        |> snd

    let rec loop l = 
        match leftmostNonterminal l with
        | Some (idx, symbol) ->
            let producedSymbols = getRandomProduction symbol rules
            let (prefix, suffix) = List.splitAt idx l
            loop <| prefix @ producedSymbols @ (List.tail suffix)
        | None ->
            l

    loop [Nonterminal start]

type GrammaticalStructure = 
    | S | NP | VP | PP | DT | Vi | Vt | NN | IN

type Words = 
    | Sleeps | Saw | Man | Woman | Telescope | With | In | The

let rules =
    [
        (S, [Nonterminal NP; Nonterminal VP]);
        (VP, [Nonterminal Vi]);
        (VP, [Nonterminal Vt; Nonterminal NP]);
        (VP, [Nonterminal VP; Nonterminal PP]);
        (NP, [Nonterminal DT; Nonterminal NN]);
        (NP, [Nonterminal NP; Nonterminal PP]);
        (PP, [Nonterminal IN; Nonterminal NP]);
        (Vi, [Terminal Sleeps]);
        (Vt, [Terminal Saw]);
        (NN, [Terminal Man]);
        (NN, [Terminal Woman]);
        (NN, [Terminal Telescope]);
        (DT, [Terminal The]);
        (IN, [Terminal With]);
        (IN, [Terminal In]);
    ] 

let exampleCFG : CFG<Words, GrammaticalStructure> = S, rules

// may infinite loop - infinite grammatically valid sentences are possible, it seems.
let getDerivation () = produceRandom exampleCFG

let parseSomething () = parse exampleCFG [Terminal The; Terminal Man; Terminal Sleeps]