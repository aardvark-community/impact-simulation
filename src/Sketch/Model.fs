namespace AardVolume.Model

open System
open Aardvark.Base
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
    }