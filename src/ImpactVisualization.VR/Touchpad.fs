namespace Touchpad

open System
open System.IO
open Aardvark.Base
open Aardvark.Base.Rendering
open FSharp.Data.Adaptive
open Aardvark.SceneGraph
open AardVolume.Model

module TouchpadShaders = 
    
    open FShade

    type Vertex = 
        {
            [<PointCoord>] // gl_PointCoord
            pointCoord : V2d

            [<FragCoord>] 
            fc : V4d

            [<Color>]
            color : V4d

            [<TexCoordAttribute>]
            uv : V2d
        }

    let fragmentSh (v : Vertex) = 
        fragment {
            let c = v.uv * 2.0 - V2d.II
            let f = Vec.dot c c
            if f > 1.0 then discard() // round points magic
            return v.color
        }
