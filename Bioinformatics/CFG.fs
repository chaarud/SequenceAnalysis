module Bioinformatics.Grammar

open System.Collections.Generic

type Symbol<'T, 'NT> =
    | Terminal of 'T
    | Nonterminal of 'NT

type ContextFreeRules<'T, 'NT> = ('NT * Symbol<'T, 'NT> list) list

type CFG<'T, 'NT> = 'NT * ContextFreeRules<'T, 'NT>

module List = 

    let getRandom (xs : _ list) = 
        let rnd = System.Random()  
        List.item (rnd.Next (xs.Length)) xs

    let splitWith predicate xs =
        (List.takeWhile predicate xs, List.skipWhile predicate xs)

type Decision = Accept | Reject

// Top-down parsing
let parse (start, rules) input = 
    let stack = new Stack<Symbol<'a, 'b>> ()
    stack.Push <| Nonterminal start

    let rec handleNextSymbol (stack : Stack<_>) input = 
        if stack.Count = 0 
            then Accept
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

                let matchNonterminal = matchTerminal >> not

                let windowize production = 
                    let rec loop acc production = 
                        match production with
                        | [] -> acc
                        | _ ->
                            let window, remainingList = List.splitWith matchTerminal production
                            let remainingList = remainingList|> List.skipWhile matchNonterminal
                            loop (window :: acc) remainingList
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

                let tryPickProduction productions input : (_ option) = 
                    // productions is an already-filtered list of LHS productions
                    // so they all have the same nonterminal on the RHS of the rule
                    let rec loop input windows = 
                        match windows with
                        | [] ->
                            true
                        | thisWindow :: remainingWindows ->
                            let newInput = matchWindow input thisWindow
                            match newInput with
                            | Some newInput ->
                                loop newInput remainingWindows
                            | None ->
                                false

                    let rec outerLoop productions input = 
                        List.tryFind (fun production ->
                            let windows = windowize production
                            loop input windows) productions

                    outerLoop productions input

                tryPickProduction productions input
                |> function
                    | Some production ->
                        production |> List.iter (stack.Push)
                        handleNextSymbol stack input
                    | None -> Reject
                    
            | (Terminal _) as sym -> 
                if sym = List.head input
                    then handleNextSymbol stack (List.tail input)
                    else Reject

    handleNextSymbol stack input

// From a CFG, produce a random derivation. This is likely to loop forever.
let produceRandom ((start, rules) : CFG<_, _>) =

    let leftmostNonterminal = 
        List.indexed
        >> List.tryPick (function
            | (idx, Nonterminal sym) -> Some (idx, sym)
            | _ -> None)

    let getRandomProduction nonterminal rules =
        rules
        |> List.filter (fst >> ((=) nonterminal))
        |> List.getRandom
        |> snd

    let rec loop l = 
        match leftmostNonterminal l with
        | Some (idx, symbol) ->
            let producedSymbols = getRandomProduction symbol rules
            let (prefix, suffix) = List.splitAt idx l
            loop <| prefix @ producedSymbols @ (List.tail suffix)
        | None -> l

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

let validSentence = [Terminal The; Terminal Man; Terminal Sleeps]
let invalidSentence = [Terminal The; Terminal Sleeps; Terminal Sleeps]

let parseSomething () = 
    let parser = parse exampleCFG
    printfn "valid: %A" <| parser validSentence
    printfn "invalid: %A" <| parser invalidSentence
    