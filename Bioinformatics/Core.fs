module Bioinformatics.Core

type Sequence = string

type Cluster = Set<Sequence>

// TODO remove the float, instead Tree<'T, 'NodeState> should have case | Node of Tree<'T> * Tree<'T> * 'NodeState
type Tree<'T> = 
    | Node of Tree<'T> * Tree<'T> * float
    | Leaf of 'T
