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
    | NoneMode = 4

type AnalyzeMode = 
    | Region = 0
    | Time = 1

type AnimationFlow = 
    | Playing = 0
    | Paused = 1

type PlaybackMode = 
    | AnimationFlow = 0
    | Forward = 1
    | Backward = 2
    | Stop = 3
    | Screenshot = 4
    | None = 5

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
    numberId        : int
    currSelected    : bool 
    selected        : bool
    timesSelected   : int 
    timeAnalyze     : bool
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
    allData         : HashMap<RenderValue, (float[] * string)> []
    
    [<NonAdaptive>]
    id : string
}

[<ModelType>]
type BoxPlot = {
    isRegion        : bool
    attribute       : RenderValue
    trafo           : Trafo3d
    positions       : Quad3d
    texture         : PixImage
    data            : HashMap<int, float[]>
    probeIds        : HashMap<int, Probe>
    screenPos       : V2d []
    probesPositions : (string * V3d) []

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
    | ToggleBillboardVisibility of int
    | GrabHera of int
    | UngrabHera of int
    | ScaleHera of int * float
    | GrabTv of int 
    | UngrabTv of int
    | ScaleTv of int * float
    | MoveController of int * Trafo3d
    | ToggleMainMenu of int
    | OpenMainMenu of int
    | ChangeTouchpadPos of int * V2d
    | ChangeMainControllerMode of int
    | ChangeAnalyzeMode of int
    | ChangeAnimationPlayback of int
    | ChangeBillboard of int
    | SelectGlobalAttribute of int
    | SelectProbeAttribute of int
    | ActivateControllerMode of int
    | CreateProbe of int // of int * Trafo3d
    | CreateRay of int //of int * Trafo3d
    | CreateClipping of int //of int * Trafo3d
    | SelectBoxPlotProbes of int 
    | SelectBoxPlotProbeTime of int
    | PlaceBoxPlot of int
    | ChangeBoxPlotAttribute of int 
    | DeleteBoxPlot of int
    | TakeBoxPlot of int 
    | LeaveBoxPlot of int 
    | CopyBoxPlot of int
    | ScaleProbe of int
    | StopProbeScale of int 
    | DeleteProbe of int 
    | DeleteClippingPlane of int
    | DeactivateControllerMode of int
    | TouchDevice of int
    | UntouchDevice of int 
    | MouseClick
    | SetTexture of PixImage * Probe
    | ResetHera of int

[<ModelType>]
type Model = 
    {
        text : string
        twoDModel : AardVolume.Model.Model
        threads : ThreadPool<Message>
        firstHistogram : bool
        showBillboard : bool

        playbackMode : PlaybackMode
        currAnimationFlow : AnimationFlow

        //Controllers
        devicesTrafos : HashMap<int, Trafo3d>
        mainControllerId : Option<int> 
        secondControllerId : Option<int>
        mainContrSignTexture : ITexture

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
        scalingFactorTv : float
        lastTvScaleTrafo : Trafo3d
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
        tvTrafo : Trafo3d
        tvToControllerTrafo : Trafo3d
        grabbingTV : bool
        tvTransformations : Trafo3d
        screenIntersection : bool
        hitPoint : V3d
        screenHitPoint : V2d 
        screenCoordsHitPos : PixelPosition
        clickPosition : Option<V2d>

        //Probes manipulation
        createProbe : bool
        currentProbeManipulated : bool 
        allProbes : HashMap<string, Probe>
        mainContrProbeIntersectionId : Option<string> 
        secondContrProbeIntersectionId : Option<string>
        lastFilterProbe : Option<Probe>
        lastFilterProbeId : Option<string>
        lastDeletedProbe : string
        lastModifiedProbeIntId : int
        newProbePlaced : bool
        existingProbeModified : bool

        //Box Plot Region
        boxPlotProbes : HashMap<int, float[]>
        lastProbeId : int
        currBoxPlotAttribSet : bool
        currBoxPlotAttrib : RenderValue
        showCurrBoxPlot : bool 
        currBoxPlot : Option<BoxPlot>
        allPlacedBoxPlots : HashMap<string, BoxPlot>
        mainContrBoxPlotIntersectionId : Option<string>
        secondContrBoxPlotIntersectionId : Option<string>
        movingBoxPlot : bool
        takenBoxPlot : Option<BoxPlot>
        allCurrSelectedProbesIds : HashMap<int, Probe>
        selectedProbesPositions : (string * V3d) []

        //Box Plot Time
        currProbeAnalyzeTime : Option<Probe>
        boxPlotFrames : HashMap<int, float[]>
        framesOrder : seq<int>

                        
        //Controller clipping plane
        holdClipping : bool
        clippingActive : bool
        interesctingClippingPlane : bool
        clippingColor : C4b
        planeCorners : Quad3d

        //Main controller menu
        mainMenuOpen : bool
        controllerMode : ControllerMode
        analyzeMode : AnalyzeMode
        menuLevel : int 

        //Second controller menu
        secondMenuOpen : bool
        attribute : RenderValue

        //Touchpad
        currMainTouchPadPos : V2d
        currSecondTouchPadPos : V2d
        mainTouching : bool
        secondTouching : bool
        mainUntouched : bool

        mainTouchpadTexture : ITexture 
        secondTouchpadTexture : ITexture 
        showMainTexture : bool
        showSecondTexture : bool

        lastTouchpadModeTexture : ITexture

        mainContrScreenTexture : ITexture
        secondContrScreenTexture : ITexture
        lastContrScreenModeTexture : ITexture
}