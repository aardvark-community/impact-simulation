namespace AardVolume.Model

open System
open Aardvark.Base
open Aardvark.UI
open Aardvark.UI.Primitives
open FSharp.Data.Adaptive
open Adaptify

type Primitive =
    | Box
    | Sphere


type Vis = 
    | HeraSimVis
    | VolumeVis


type RenderValue = 
    | Energy = 0
    | CubicRoot = 1
    | Strain = 2
    | AlphaJutzi = 3
    | Pressure = 4


type Axis = 
    {
    minX : aval<float>;
    maxX : aval<float>;
    minY : aval<float>;
    maxY : aval<float>;
    minZ : aval<float>;
    maxZ : aval<float>;
    }

type Slider = 
    {
    slideX : aval<float>
    slideY : aval<float>
    slideZ : aval<float>
    }

[<ModelType>]
type Model =
    {
        currentModel    : Primitive
        cameraState     : CameraControllerState
        frame : int
        vis : Vis

        pointSize : float
        playAnimation : bool
        renderValue : RenderValue
        colorMaps : HashMap<string, string>
        currentMap : string
        minX : float
        maxX : float
        minY : float
        maxY : float
        minZ : float
        maxZ : float 
        slideX : float
        slideY : float
        slideZ : float

        count : NumericInput
        data : list<float>

    }