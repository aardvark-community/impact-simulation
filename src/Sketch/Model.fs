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



[<ModelType>]
type Model =
    {
        currentModel    : Primitive
        cameraState     : CameraControllerState
        frame : int
        vis : Vis

        pointSize : float
    }