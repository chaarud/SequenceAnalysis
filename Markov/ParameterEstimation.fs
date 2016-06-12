module Markov.ParameterEstimation

open Types

type IEstimator<'State, 'Emission when 'State: comparison> = 
    abstract member estimate : 'State list -> 'Emission list -> 'Emission list list -> (Begin<'State> * HMM<'State, 'Emission>)
