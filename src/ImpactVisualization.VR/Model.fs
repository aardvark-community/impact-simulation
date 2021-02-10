namespace ImpactVisualization

open Aardvark.UI
open Aardvark.UI.Primitives
open FSharp.Data.Adaptive
open Adaptify
open Aardvark.Base
open System.IO

open FSharp.Data.Adaptive

open AardVolume.Model
open Aardvark.Cef


type ControllerMode = 
    | Probe = 0
    | Ray = 1
    | Clipping = 2

[<ModelType>]
type Probe = {
    
    center          : V3d
    radius          : float
    centerRelToHera : V3d
    radiusRelToHera : float
    insideHera      : bool
    currStatistics  : string

    [<NonAdaptive>]
    id : string
}

[<ModelType>]
type Model = 
    {
        text : string
        twoDModel : AardVolume.Model.Model

        //Controllers
        devicesTrafos : HashMap<int, Trafo3d>

        //Hera
        controllerTrafo : Option<Trafo3d>
        heraTrafo : Option<Trafo3d>
        heraToControllerTrafo : Option<Trafo3d>
        grabberId : Option<int>
        allowHeraScaling : bool
        heraBox : Box3d
        heraTransformations : Trafo3d

        //Sphere translation and scaling
        sphereControllerTrafo: Option<Trafo3d>
        sphereControllerId: Option<int>
        sphereScalerTrafo : Option<Trafo3d>
        sphereScalerId : Option<int>

        //Scaling factors
        scalingFactorHera : float
        sphereScale : float 
        lastSphereScale : float
        sphereRadius : float 
        sphereColor : C4b

        //Ray
        rayDeviceId : Option<int>
        ray : Ray3d
        rayColor : C4b

        [<NonAdaptive>]
        rayTriggerClicked : bool

        //TV
        tvQuad : Quad3d
        screenIntersection : bool
        hitPoint : V3d
        screenHitPoint : V2d 
        screenCoordsHitPos : PixelPosition
        clickPosition : Option<V2d>

        //Probes manipulation
        currentProbe : Option<Probe>
        currentProbeManipulated : bool 
        allProbes : HashMap<string, Probe>
        intersectionControllerId : Option<int>
        deletionControllerId : Option<int>
        probeIntersectionId : Option<string>
        lastFilterProbe : Option<Probe>

        statistics : string
        
        //Controller clipping plane
        clippingPlaneDeviceTrafo : Option<Trafo3d>
        clippingPlaneDeviceId : Option<int>
        planeCorners : Quad3d

        //Controller menu
        controllerMenuOpen : bool
        menuControllerTrafo : Option<Trafo3d>
        menuControllerId : Option<int>
        controllerMode : ControllerMode

        //Touchpad
        currTouchPadPos : V2d
        touchpadDeviceId : Option<int>
        touchpadDeviceTrafo : Option<Trafo3d>
        touchpadTexture : string 
}