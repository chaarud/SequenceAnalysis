module Markov.ParameterEstimation

open Types

type IEstimator<'State, 'Emission when 'State: comparison> = 
    abstract member estimate : 'State list -> 'Emission list -> 'Emission list list -> HMM<'State, 'Emission>

//let EM<'Latent, 'Parameters> (convergenceThreshold : double) (observations : 'Observation list) (randomParameters: 'Parameters) : 'Parameters = 
//    let newLatent : 'Latent = calculateLatent observations randomParameters
//    let newParameters = calculateParameters observations newLatent
