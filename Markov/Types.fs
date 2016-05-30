module Markov.Types

//unused
//needs to be validated/enforced
//first todo w/reflection for finite discrete countable algebraic data 'T
type Distribution<'T> = 'T -> float

//TODO add validation for probability distributions, other invariants
//or reflect it in property based tests?

// enforce that it's between 0 and 1?
type Probability = double

type NodeInfo<'State, 'Emission> = 
    {
        // enforce these probabilities sum to 1
        emissions : ('Emission * Probability) list
        // enforce these probabilities sum to 1
        transitions : (('State option) * Probability) list
    }

// each element in 'state should appear at most once
type HMM<'State, 'Emission when 'State : comparison> = Map<'State, NodeInfo<'State, 'Emission>>

//'State option because the observations could be empty (ie start -> end) I suppose.
type Begin<'State> = (('State option) * Probability) list
