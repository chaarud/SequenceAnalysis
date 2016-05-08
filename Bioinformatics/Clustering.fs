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
            let newCluster = Set.union c1 c2
            let newTree = Node (left, right, d/2.) 

            map
            |> Map.add newCluster newTree
            |> Map.remove c1
            |> Map.remove c2
            |> loop
            
    let clusters = List.map Set.singleton seqs
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

let NeighborJoining' (d : IDistance<Tree<_>>) avgDistance (seqs : Sequence list) =

    let rec loop L T distances = 
        match Set.toList L with
        | [l1; l2] -> (Node (l1, l2, 0.), distances)
        | _ ->
            let ((i, j), _) = 
                distances
                |> Map.toList
                |> List.filter (fun ((i, j), _) -> (Set.contains i L) && (Set.contains j L))
                |> List.minBy (fun ((i, j), _) -> avgDistance L i j distances)

            let k = Node (i, j, 0.)

            let d_ij = Map.find (i, j) distances
            let d_ik = 0.5 * (avgDistance L i j distances)
            let d_jk = d_ij - d_ik

            let newT = T |> Set.add k

            let newDistances = 
                distances
                |> Map.add (i, k) d_ik
                |> Map.add (k, i) d_ik
                |> Map.add (j, k) d_jk
                |> Map.add (k, j) d_jk
                |> Set.foldBack (fun m distances ->
                    let d_im = Map.find (i, m) distances
                    let d_jm = Map.find (j, m) distances
                    let d_km = 0.5 * (d_im + d_jm + d_ij)
                    distances
                    |> Map.add (k, m) d_km
                    |> Map.add (m, k) d_km) L

            let newL = 
                L
                |> Set.remove i
                |> Set.remove j
                |> Set.add k

            loop newL newT newDistances

    let leaves = seqs |> List.map Leaf
    let T = leaves |> Set.ofList
    let L = T

    // test map.ofList [(1, 3); (1, 4); (1, 3)] etc
    let distances : Map<Tree<_>*Tree<_>, float> = 
        leaves 
        |> List.collect (fun l1 ->
            List.fold (fun map l2 ->
                //when we map over l2 we will add the pair (l2, l1)
                Map.add (l1, l2) (d.distance l1 l2).dist map) Map.empty leaves
            |> Map.toList)
        |> Map.ofList

    loop L T distances

let avgDistances L i j distances = 
    let r i L = 
        let normalization = 1. / ((float <| Set.count L) - 2.)
        let distancesSum = Set.fold (fun sum k -> sum + (Map.find (i, k) distances)) 0. L
        normalization * distancesSum
    let d_ij = Map.find (i, j) distances
    let r_i = r i L
    let r_j = r j L
    d_ij - (r_i + r_j)