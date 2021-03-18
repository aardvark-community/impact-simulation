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
    | NoneMode = 3

type ProbeType = 
    | Sphere = 0 
    | Box = 1
    | Line = 2

type Attribute = 
    | Energy = 0
    | CubicRoot = 1
    | Strain = 2
    | AlphaJutzi = 3
    | Pressure = 4
    | Density = 6
    | NoAttribute = 7

[<ModelType>]
type Probe = {
    
    center          : V3d
    radius          : float
    centerRelToHera : V3d
    radiusRelToHera : float
    insideHera      : bool
    currStatistics  : string
    currHistogram   : Option<ITexture>
    showBillboard   : bool 

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
        controllerTrafo : Trafo3d
        heraTrafo : Trafo3d
        heraToControllerTrafo : Trafo3d
        grabberId : Option<int>
        allowHeraScaling : bool
        heraBox : Box3d
        heraTransformations : Trafo3d

        //Sphere translation and scaling
        sphereControllerTrafo: Trafo3d
        sphereControllerId: Option<int>
        sphereScalerTrafo : Trafo3d
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
        clippingPlaneDeviceTrafo : Trafo3d
        clippingPlaneDeviceId : Option<int>
        planeCorners : Quad3d

        //Controller menu
        controllerMenuOpen : bool
        menuControllerTrafo : Trafo3d
        menuControllerId : Option<int>
        controllerMode : ControllerMode
        menuLevel : int 
        attribute : RenderValue

        //Touchpad
        currTouchPadPos : V2d
        touchpadDeviceId : Option<int>
        touchpadDeviceTrafo : Trafo3d
        touchpadTexture : ITexture 
        lastTouchpadModeTexture : ITexture
        textureDeviceTrafo : Trafo3d
        showTexture : bool

        contrScreenTexture : ITexture
        lastContrScreenModeTexture : ITexture
}