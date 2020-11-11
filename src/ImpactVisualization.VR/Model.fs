namespace ImpactVisualization

open Aardvark.UI
open Aardvark.UI.Primitives
open FSharp.Data.Adaptive
open Adaptify
open Aardvark.Base
open System.IO

open FSharp.Data.Adaptive

open AardVolume.Model


type PlaneCorners = 
    {
    c0 : V3d
    c1 : V3d
    c2 : V3d
    c3 : V3d
    }

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
        allowHeraScaling : bool

        sphereControllerTrafo: Option<Trafo3d>
        sphereControllerId: Option<int>
        sphereScalerTrafo : Option<Trafo3d>
        sphereScalerId : Option<int>

        scalingFactorHera : float
        sphereScale : float 
        sphereRadius : float 
        sphereColor : C4b

        rayDeviceId : Option<int>
        ray : Ray3d
        rayColor : C4b

        flatScreen : Quad3d
        screenIntersection : bool

        sphereProbeCreated : bool
        
        clippingPlaneDeviceId : Option<int>
        planeCorners : Quad3d
}