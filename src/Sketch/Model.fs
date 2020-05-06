namespace AardVolume.Model

open Aardvark.UI
open Aardvark.UI.Primitives
open FSharp.Data.Adaptive
open Adaptify

type RenderValue = 
    | Energy = 0
    | CubicRoot = 1
    | Strain = 2
    | AlphaJutzi = 3
    | Pressure = 4

type DomainRange = 
    {
    minX : aval<float>;
    maxX : aval<float>;
    minY : aval<float>;
    maxY : aval<float>;
    minZ : aval<float>;
    maxZ : aval<float>;
    }

type ClippingPlane = 
    {
    clippingPlaneX : aval<float>
    clippingPlaneY : aval<float>
    clippingPlaneZ : aval<float>
    }

type EnergyPoints = 
    {
    energyPoints1 : list<float>
    energyPoints2 : list<float>
    }


[<ModelType>]
type Model =
    {
        cameraState : CameraControllerState
        frame : int

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
        clippingPlaneX : float
        clippingPlaneY : float
        clippingPlaneZ : float

        count : NumericInput
        data : list<float>

        //filter : option<Box3d>
        //filtered : list<float> 
    }