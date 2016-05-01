module Bioinformatics.Distance

open Core
open Utils

type PairwiseDistance<'T> = 
    {
        elem1 : 'T
        elem2 : 'T
        dist : float 
    }

type IDistance<'T> = 
    abstract member distance : 'T -> 'T -> PairwiseDistance<'T>

let clusterDistance (sequenceDistance : IDistance<Sequence>) = 
    { new IDistance<Cluster> with
        member this.distance c1 c2 = 
            {
                elem1 = c1
                elem2 = c2
                dist = 
                    cartesian (Set.toList c1) (Set.toList c2)
                    |> List.averageBy (fun (s1, s2) -> (sequenceDistance.distance s1 s2).dist)
            }
    }
    
let sequenceDistanceBuilder f = 
    { new IDistance<Sequence> with
        member this.distance s1 s2 = 
            {
                elem1 = s1
                elem2 = s2
                dist = f s1 s2
            }
    }

let simpleDistanceFn s1 s2 = 
    List.zip (String.toList s1) (String.toList s2)
    |> List.filter (fun (c1, c2) -> c1 = c2)
    |> List.length
    |> float
    |> (/) (float <| String.length s1)

let simpleDistance = sequenceDistanceBuilder simpleDistanceFn

let jukesCantorDistanceGeneric t s1 s2 = 
    let f = simpleDistanceFn s1 s2
    let r = (float <| t - 1) / (float t)
    let rReciprocal = 1. / r
    let n = 1. - (rReciprocal * f)
    - r * (System.Math.Log n)

let jukesCantorDistanceFn = jukesCantorDistanceGeneric 4

let jukesCantorDistance = sequenceDistanceBuilder jukesCantorDistanceFn

let clusterDustance = clusterDistance simpleDistance

let clusterDistanceDGenerator (clusterDistance : IDistance<Cluster>) (leaves : Cluster list) = 
    {
        new IDistance<Cluster> with
            member this.distance c1 c2 = 
                {
                    elem1 = c1
                    elem2 = c2
                    dist = 
                        let d = (clusterDistance.distance c1 c2).dist
                        let rc1 = (1. / ((float <| List.length leaves) - 2.)) * (List.sum (List.map (fun clust -> (clusterDistance.distance c1 clust).dist) leaves))
                        let rc2 = (1. / ((float <| List.length leaves) - 2.)) * (List.sum (List.map (fun clust -> (clusterDistance.distance c1 clust).dist) leaves))
                        d-(rc1+rc2)
                }
    }
