module Markov.DPUtils

type MarkovDPCell<'State, 'Emission> = 
    {
        score : double
        ancestor : MarkovDPCell<'State, 'Emission> option
        // None is the begin state
        state : 'State option
        emission : 'Emission option
    }

let makeDPTable (observations : 'Emission list) (states : 'State list) = 
    Array2D.zeroCreate<MarkovDPCell<'State, 'Emission>> ((List.length states) + 1) ((List.length observations) + 1)
    |> Array2D.mapi (fun idxState idxEmission cell ->
        {
            // TODO: should the score be an option as well?
            score = -infinity //-1.0 //HACK
            ancestor = None
            state = if idxState = 0 then None else Some states.[idxState - 1]
            emission = if idxEmission = 0 then None else Some observations.[idxEmission - 1]
        })

//Array2Ds are indexed with length1 = length of columns, length2 = length of row
let numColumns = Array2D.length2
let numRows = Array2D.length1
//ie:
let columnSize = Array2D.length1
let rowSize = Array2D.length2

let getColumnFromTable l table = 
    Array.init (columnSize table) (fun j -> table.[j, l])

let getLastColumn table = 
    getColumnFromTable ((numColumns table) - 1) table

// traverse by row
let getNextCellByRow m n (i, j) = 
    if j+1 < n
        then Some (i, j+1)
        else 
            if i+1 < m
                then Some (i+1, 0)
                else None

// traverse by column
let getNextCell m n (i, j) = 
    if i+1 < m
        then Some (i+1, j)
        else
            if j+1 < n
                then Some (0, j+1)
                else None

//TODO test coverage
let getNextBackwardCell m n (i, j) = 
    if i-1 >= 0 
        then Some (i-1, j)
        else
            if j-1 >= 0
                then Some (m-1, j-1)
                else None