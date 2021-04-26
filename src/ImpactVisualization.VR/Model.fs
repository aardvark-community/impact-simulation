namespace ImpactVisualization

open Aardvark.UI
open Aardvark.UI.Primitives
open FSharp.Data.Adaptive
open Adaptify
open Aardvark.Base
open System.IO

open FSharp.Data.Adaptive
open FSharpx.Collections

open AardVolume.Model
open Aardvark.Cef


type ControllerMode = 
    | Probe = 0
    | Ray = 1
    | Clipping = 2
    | Analyze = 3
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

type BillboardType = 
    | Statistic = 0
    | Histogram = 1

[<ModelType>]
type Probe = {
    color           : C4b
    center          : V3d
    radius          : float
    centerRelToHera : V3d
    radiusRelToHera : float
    insideHera      : bool
    showStatistics  : bool 
    currHistogram   : Option<PixImage>
    showHistogram   : bool
    currBillboard   : BillboardType
    currAttribute   : RenderValue
    allData         : HashMap<RenderValue, (float[] * string)>
    

    [<NonAdaptive>]
    id : string
}

type BoxPlot = {
    attribute : RenderValue
    trafo     : Trafo3d

    [<NonAdaptive>]
    id : string
}

type ThreeDMessage = Nop

type Message =
    | ThreeD of ThreeDMessage
    | TwoD of AardVolume.Message 
    | Nop
    | StartVr
    | SetController of int
    | GrabHera of int
    | UngrabHera of int
    | ScaleHera of int * float
    | MoveController of int * Trafo3d
    | ToggleMainMenu of int
    | OpenMainMenu of int
    | ChangeTouchpadPos of int * V2d
    | ChangeMainControllerMode of int
    | ChangeBillboard of int
    | SelectGlobalAttribute of int
    | SelectProbeAttribute of int
    | ActivateControllerMode of int
    | CreateProbe of int // of int * Trafo3d
    | CreateRay of int //of int * Trafo3d
    | CreateClipping of int //of int * Trafo3d
    | SelectBoxPlotProbes of int 
    | PlaceBoxPlot of int
    | ChangeBoxPlotAttribute of int 
    | DeleteBoxPlot of int
    | TakeBoxPlot of int 
    | ScaleProbe of int
    | StopProbeScale of int 
    | DeleteProbe of int 
    | DeleteClippingPlane of int
    | DeactivateControllerMode of int
    | TouchDevice of int
    | UntouchDevice of int 
    | MouseClick
    | SetTexture of PixImage * Probe
    | ResetHera 

[<ModelType>]
type Model = 
    {
        text : string
        twoDModel : AardVolume.Model.Model
        threads : ThreadPool<Message>
        firstHistogram : bool

        //Controllers
        devicesTrafos : HashMap<int, Trafo3d>
        mainControllerId : Option<int> 
        secondControllerId : Option<int>

        mainControllerTrafo : Trafo3d
        secondControllerTrafo : Trafo3d

        //Hera
        heraTrafo : Trafo3d
        heraToControllerTrafo : Trafo3d
        lastHeraScaleTrafo : Trafo3d
        grabbingHera : bool
        heraBox : Box3d
        heraTransformations : Trafo3d

        holdingSphere : bool 
        scalingSphere : bool

        //Scaling factors
        scalingFactorHera : float
        sphereScale : float 
        lastSphereScale : float
        sphereRadius : float 

        //Ray
        ray : Ray3d
        rayActive : bool
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
        currentProbeManipulated : bool 
        allProbes : HashMap<string, Probe>
        mainContrProbeIntersectionId : Option<string> 
        secondContrProbeIntersectionId : Option<string>
        lastFilterProbe : Option<Probe>
        lastFilterProbeId : Option<string>
        newProbePlaced : bool
        existingProbeModified : bool

        //Box Plot
        boxPlotProbes : PersistentHashMap<string, float[]>
        currBoxPlotAttribSet : bool
        currBoxPlotAttrib : RenderValue
        showCurrBoxPlot : bool 
        currBoxPlot : Option<BoxPlot>
        allPlacedBoxPlots : HashMap<string, BoxPlot>
                        
        //Controller clipping plane
        holdClipping : bool
        clippingActive : bool
        interesctingClippingPlane : bool
        clippingColor : C4b
        planeCorners : Quad3d

        //Main controller menu
        mainMenuOpen : bool
        controllerMode : ControllerMode
        menuLevel : int 

        //Second controller menu
        secondMenuOpen : bool
        attribute : RenderValue

        //Touchpad
        currMainTouchPadPos : V2d
        currSecondTouchPadPos : V2d
        mainTouching : bool
        secondTouching : bool

        mainTouchpadTexture : ITexture 
        secondTouchpadTexture : ITexture 
        showMainTexture : bool
        showSecondTexture : bool

        lastTouchpadModeTexture : ITexture

        mainContrScreenTexture : ITexture
        secondContrScreenTexture : ITexture
        lastContrScreenModeTexture : ITexture
}