module Bioinformatics.Clustering

open Core
open Utils
open Distance

let UPGMA (d : IDistance<Cluster>) (seqs : Sequence list) : Tree<Sequence> = 
    let rec loop map = 
        match Map.toList map with
        | [(cl,tl); (cr, tr)] ->
            Node (tl, tr, (d.distance cl cr).dist/2.)
        | l ->
            let clusters = l |> List.map fst
            let pairwiseDistances = 
                clusters
                |> List.map (fun s1 -> 
                    clusters |> List.map (fun s2 -> d.distance s1 s2))
                |> List.concat
                
            let closestClusters = List.minBy (fun pd -> pd.dist) pairwiseDistances
            let c1, c2, d = closestClusters.elem1, closestClusters.elem2, closestClusters.dist
            let left = Map.find c1 map
            let right = Map.find c2 map
            let newTree = Node (left, right, d/2.) 
            let newCluster = Set.union c1 c2

            map
            |> Map.add newCluster newTree
            |> Map.remove c1
            |> Map.remove c2
            |> loop
            
    let clusters : Cluster list = List.map Set.singleton seqs
    let trees = List.map Leaf seqs
    List.zip clusters trees
    |> Map.ofList
    |> loop

let NeighborJoining (d : IDistance<Tree<_>>) (D : IDistance<Tree<_>>) (seqs : Sequence list) = 

    let rec loop map trees = 
        match Set.toList trees with
        | [t1; t2] ->
            let distances = Map.add (t1, t2) (d.distance t1 t2).dist map
            let tree = Node (t1, t2, 0.) //the float means nothing, remove it
            (tree, distances)
        | _ ->
            let i, j = 
                map
                |> Map.toList
                |> List.minBy snd
                |> fst

            let k = Node (i, j, 0.)

            let d_ij = (d.distance i j).dist
            let d_ik = 0.5 * (D.distance i j).dist
            let d_jk = d_ij - d_ik

            let map = 
                Set.fold (fun map m ->
                    let d_im = (d.distance i m).dist
                    let d_jm = (d.distance j m).dist
                    let d_km = 0.5 * (d_im + d_jm + d_ij)
                    Map.add (k, m) d_km map) map trees
                |> Map.add (i, k) d_ik
                |> Map.add (j, k) d_jk

            trees
            |> Set.remove i
            |> Set.remove j
            |> Set.add k
            |> loop map

    let trees = 
        seqs
        |> List.map Leaf

    let treePairs = cartesian trees trees
    let map = List.fold (fun map (t1,t2) ->
        Map.add (t1, t2) (d.distance t1 t2).dist map) Map.empty treePairs

    loop map (Set.ofList trees)