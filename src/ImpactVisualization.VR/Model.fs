namespace ImpactVisualization

open Aardvark.UI
open Aardvark.UI.Primitives
open FSharp.Data.Adaptive
open Adaptify
open Aardvark.Base
open System.IO

open FSharp.Data.Adaptive

open AardVolume.Model


type ControllerMode = 
    | Probe = 0
    | Ray = 1
    | Clipping = 2

[<ModelType>]
type Probe = {
    center : V3d
    radius : float

    [<NonAdaptive>]
    id : string
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

        tvQuad : Quad3d
        screenIntersection : bool
        hitPoint : V3d
        screenHitPoint : V2d 

        currentProbe : Option<Probe>
        currentProbeManipulated : bool 
        allProbes : HashMap<string, Probe>
        intersectionControllerId : Option<int>
        probeIntersectionId : Option<string>
        
        clippingPlaneDeviceTrafo : Option<Trafo3d>
        clippingPlaneDeviceId : Option<int>
        planeCorners : Quad3d

        controllerMenuOpen : bool
        menuControllerTrafo : Option<Trafo3d>
        menuControllerId : Option<int>
        controllerMode : ControllerMode

        touchPadCurrPosX : float
}