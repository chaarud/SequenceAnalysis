module Bioinformatics.Utils

let rec cartesian xs ys = 
    match xs, ys with
    | _, [] -> []
    | [], _ -> []
    | x::xs', _ -> (List.map (fun y -> x, y) ys) @ (cartesian xs' ys)

module String = 
    let toList (s : string) : char list = 
        [ for c in s -> c ]
    let fromList (l : char list) : string =
        let sb = System.Text.StringBuilder(List.length l)
        l |> List.iter (sb.Append >> ignore)
        sb.ToString ()

