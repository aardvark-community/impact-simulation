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

        devicesTrafos : HashMap<int, Trafo3d>

        controllerTrafo : Option<Trafo3d>
        heraTrafo : Option<Trafo3d>
        heraToControllerTrafo : Option<Trafo3d>
        grabberId : Option<int>

        scalingFactor : float

        sphereProbe : bool 
    }