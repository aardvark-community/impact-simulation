namespace ImpactVisualization

open Aardvark.UI
open Aardvark.UI.Primitives
open FSharp.Data.Adaptive
open Adaptify
open Aardvark.Base
open System.IO

open FSharp.Data.Adaptive

open AardVolume.Model


[<ModelType>]
type Model = 
    {
        text : string
        twoDModel : AardVolume.Model.Model

        modelTrafo : Option<Trafo3d>
        grabTrafo : Option<Trafo3d>
        grabberId : Option<int>

        scalingFactor : float

        sphereProbe : bool 
    }