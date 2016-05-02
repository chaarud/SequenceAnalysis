module Bioinformatics.Alignment

open Core
open Utils
open Clustering

type Symbol = 
    | Gap
    | Character of char
 
type NWCell = 
    {
        score : int
        ancestor : (int*int) option
    }

type EligibleDirections = 
    | HVD
    | V
    | H
    | Nothing

type NWParams = 
    {
        d : int
        s : Map<char*char, int>
    }
       
let createTable m n = 
    Array2D.zeroCreate<NWCell> (m+1) (n+1)
            
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

let NeedlemanWunsch (p : NWParams) s1 s2 = 

    // TODO we shouldn't have to index into l1.[i-1] because the lists are 0-indexed and the DP table is effectively 1-indexed.
    // one possible solution is to include a xChar and yChar string in the NWCell record.

    let s = p.s
    let d = p.d

    let l1 = String.toList s1
    let l2 = String.toList s2

    let m = List.length l1
    let n = List.length l2

    let rec fillTable (table : NWCell [,]) = function
        | Some (i, j) ->
            let newCell = 
                match eligibleDirections (i, j) with
                | Nothing ->
                    {score = 0; ancestor = None}
                | H ->
                    {score = table.[i-1, j].score - d; ancestor = Some (i-1, j)}
                | V -> 
                    {score = table.[i, j-1].score - d; ancestor = Some (i, j-1)}
                | HVD ->
                    let diagonalScore = table.[i-1, j-1].score + (Map.find (l1.[i-1], l2.[j-1]) s)
                    let diagonal = {score = diagonalScore; ancestor = Some (i-1, j-1)}
                    let horizontalScore = table.[i-1, j].score - d
                    let horizontal = {score = horizontalScore; ancestor = Some (i-1, j)}
                    let verticalScore = table.[i, j-1].score - d
                    let vertical = {score = verticalScore; ancestor = Some (i, j-1)}
                    List.maxBy (fun nwcell -> nwcell.score) [diagonal; horizontal; vertical]
            table.[i, j] <- newCell
            fillTable table (getNextCell m n (i, j))
        | None ->
            table

    let rec traceback (table : NWCell [,]) (alignment : (Symbol*Symbol) list) = function
        | Some (i, j) ->
            let cell = table.[i, j]
            let currentPairAlignment = 
                match cell.ancestor with
                | Some (k, l) when (k, l) = (i-1, j) -> 
                    (Character l1.[i-1], Gap)
                | Some (k, l) when (k, l) = (i, j-1) -> 
                    (Gap, Character l2.[j-1])
                | Some (k, l) when (k, l) = (i-1, j-1) -> 
                    (Character l1.[i-1], Character l2.[j-1])
                | _ ->
                    // TODO get rid of these (Gap, Gap) pairs
                    (Gap, Gap)
            traceback table (currentPairAlignment :: alignment) cell.ancestor
        | None ->
            alignment

    let table = createTable m n
    let startCell = Some (0, 0)
    let dptable = fillTable table startCell

    let tracebackStart = Some (m, n)
    traceback table [] tracebackStart
