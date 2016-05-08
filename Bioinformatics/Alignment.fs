module Bioinformatics.Alignment

open Core
open Utils
open Clustering

type Symbol = 
    | Gap
    | Character of char
 
type DPCell = 
    {
        score : int
        ancestor : (int*int) option
    }

type EligibleDirections = 
    | HVD
    | V
    | H
    | Nothing

type AlignmentParams = 
    {
        d : int
        s : Map<char*char, int>
    }
       
let createTable m n = 
    Array2D.zeroCreate<DPCell> (m+1) (n+1)
            
let eligibleDirections = function
    | (0, 0) -> Nothing
    | (i, 0) when i > 0 -> H
    | (0, j) when j > 0 -> V
    | (i, j) when i > 0 && j > 0 -> HVD
  
let getNextCell m n = function
    | (i, j) when i = m && j = n ->
        None
    | (i, j) when i+1 <= m && j-1 >= 0 ->
        Some (i+1, j-1)
    | (i, j) ->
        if i+j+1 <= n 
            then Some (0, i+j+1)
            else
                if i+j+1-n <= m then Some (i+j+1-n, n) else Some (m, n)

let getScore map (l1 : _ list) (l2 : _ list) i j = Map.find (l1.[i-1], l2.[j-1]) map
let getChar (l : _ list) i = l.[i-1]

let rec traceback l1 l2 (table : DPCell [,]) (alignment : (Symbol*Symbol) list) = function
    | Some (i, j) ->
        let cell = table.[i, j]
        let currentPairAlignment = 
            match cell.ancestor with
            | Some (k, l) when (k, l) = (i-1, j) -> 
                (Character <| getChar l1 i, Gap)
            | Some (k, l) when (k, l) = (i, j-1) -> 
                (Gap, Character <| getChar l2 j)
            | Some (k, l) when (k, l) = (i-1, j-1) -> 
                (Character <| getChar l1 i, Character <| getChar l2 j)
            | _ ->
                // This is an invalid state.
                // Should only be reached when we're finished aligning.
                (Gap, Gap)
        traceback l1 l2 table (currentPairAlignment :: alignment) cell.ancestor
    | None ->
        List.tail alignment

// TODO: reduce the parameter passing mess to the helper functions
// TODO: any better way to refactor this?
//       should we further refactor this? (eg, pull out an "aligner" function

let rec fillTable m n scorer (table : DPCell [,]) = function
    | Some (i, j) ->
        let newCell = scorer table (i, j)
        table.[i, j] <- newCell
        fillTable m n scorer table (getNextCell m n (i, j))
    | None ->
        table

let needlemanWunschScorer getScore gapPenalty (table : DPCell [,]) (i, j) = 
    match eligibleDirections (i, j) with
    | Nothing ->
        {score = 0; ancestor = None}
    | H ->
        {score = table.[i-1, j].score - gapPenalty; ancestor = Some (i-1, j)}
    | V -> 
        {score = table.[i, j-1].score - gapPenalty; ancestor = Some (i, j-1)}
    | HVD ->
        let dScore = table.[i-1, j-1].score + (getScore i j)
        let diagonal = {score = dScore; ancestor = Some (i-1, j-1)}
        let hScore = table.[i-1, j].score - gapPenalty
        let horizontal = {score = hScore; ancestor = Some (i-1, j)}
        let vScore = table.[i, j-1].score - gapPenalty
        let vertical = {score = vScore; ancestor = Some (i, j-1)}
        List.maxBy (fun dpcell -> dpcell.score) [diagonal; horizontal; vertical]

let NeedlemanWunsch (p : AlignmentParams) s1 s2 = 
    let l1 = String.toList s1
    let l2 = String.toList s2

    let m = List.length l1
    let n = List.length l2

    let table = createTable m n
    let startCell = Some (0, 0)

    let gapPenalty = p.d
    let mutationScore = getScore p.s l1 l2
    let scorer = needlemanWunschScorer mutationScore gapPenalty

    let dptable = fillTable m n scorer table startCell

    let tracebackStart = Some (m, n)
    traceback l1 l2 table [] tracebackStart

let smithWatermanScorer getScore gapPenalty (table : DPCell [,]) (i, j) = 
    match eligibleDirections (i, j) with
    | Nothing | H | V ->
        {score = 0; ancestor = None}
    | HVD ->
        let dScore = table.[i-1, j-1].score + (getScore i j)
        let diagonal = {score = dScore; ancestor = Some (i-1, j-1)}
        let hScore = table.[i-1, j].score - gapPenalty
        let horizontal = {score = hScore; ancestor = Some (i-1, j)}
        let vScore = table.[i, j-1].score - gapPenalty
        let vertical = {score = vScore; ancestor = Some (i, j-1)}
        let zero = {score = 0; ancestor = None}
        List.maxBy (fun dpcell -> dpcell.score) [diagonal; horizontal; vertical; zero]

let SmithWaterman (p : AlignmentParams) s1 s2 = 
    let l1 = String.toList s1
    let l2 = String.toList s2

    let m = List.length l1
    let n = List.length l2

    let table = createTable m n
    let startCell = Some (0, 0)

    let gapPenalty = p.d
    let mutationScore = getScore p.s l1 l2
    let scorer = smithWatermanScorer mutationScore gapPenalty

    let dptable = fillTable m n scorer table startCell

    let tracebackStart = 
        // Can we make this stateless with folds?
        let mutable maxIndex = (0, 0)
        let mutable maxValue = -1
        [0 .. m] |> List.iter (fun i ->
            [0..n] |> List.iter (fun j ->
                if table.[i, j].score > maxValue 
                    then 
                        maxValue <- table.[i, j].score
                        maxIndex <- (i, j)
                    else ()))
        Some maxIndex

    traceback l1 l2 table [] tracebackStart

