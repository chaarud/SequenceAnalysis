module Markov.DPUtils

type MarkovDPCell<'State, 'Emission> = 
    {
        score : double
        ancestor : MarkovDPCell<'State, 'Emission> option
        // None is the begin state
        state : 'State option
        emission : 'Emission option
    }

let makeDPTable (states : 'State list) (observations : 'Emission list) = 
    Array2D.zeroCreate<MarkovDPCell<'State, 'Emission>> ((List.length states) + 1) ((List.length observations) + 1)
    |> Array2D.mapi (fun idxState idxEmission cell ->
        {
            // should the score be an option as well
            score = -1.0 // can viterbi generate negative scores? No
            ancestor = None
            state = if idxState = 0 then None else Some states.[idxState - 1]
            emission = if idxEmission = 0 then None else Some observations.[idxEmission - 1]
        })

//Array2Ds are indexed with length1 = length of columns, length2 = length of row
let getColumnFromTable l table = 
    Array.init (Array2D.length1 table) (fun j -> table.[j, l])

let getLastColumn table = 
    let numColumns = Array2D.length2 table
    getColumnFromTable (numColumns-1) table

//I think this traverses by row...
//let getNextCell m n (i, j) = 
//    if j+1 < n
//        then Some (i, j+1)
//        else 
//            if i+1 < m
//                then Some (i+1, 0)
//                else None

let getNextCell m n (i, j) = 
    if i+1 < m
        then Some (i+1, j)
        else
            if j+1 < n
                then Some (0, j+1)
                else None
