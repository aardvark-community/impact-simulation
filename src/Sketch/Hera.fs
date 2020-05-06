module Hera

open System
open System.IO
open Aardvark.Base
open Aardvark.Base.Rendering
open FSharp.Data.Adaptive
open Aardvark.SceneGraph
open AardVolume.Model

module Shaders = 

    open FShade

    type Vertex = 
        {
            [<Position>] // gl_Position
            pos : V4d

            [<WorldPosition>]
            wp : V4d

            [<PointSize>] // gl_PointSize
            pointSize : float

            [<PointCoord>] // gl_PointCoord
            pointCoord : V2d

            [<Color>]
            pointColor : V4d

            [<Semantic("Velocity")>] 
            velocity : V3d

            [<Semantic("Energy")>]
            energy : float
            
            [<Semantic("CubicRootOfDamage")>]
            cubicRoot : float

            [<Semantic("LocalStrain")>]
            localStrain : float

            [<Semantic("AlphaJutzi")>]
            alphaJutzi : float

            [<Semantic("Pressure")>]
            pressure : float
        }

    let transfer = 
        sampler2d {
            texture uniform?TransferFunction
            filter Filter.MinMagLinear
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
        }

    let vs (v : Vertex) =
        vertex {
            let color = 
                let renderValue = uniform?RenderValue
                let value renderValue = 
                    match renderValue with
                    | RenderValue.Energy -> v.energy
                    | RenderValue.CubicRoot -> v.cubicRoot
                    | RenderValue.Strain -> v.localStrain * 10000.0
                    | RenderValue.AlphaJutzi -> v.alphaJutzi
                    | RenderValue.Pressure -> v.pressure
                    | _ -> v.energy
                let linearCoord = value renderValue          
                let transferFunc = transfer.SampleLevel(V2d(linearCoord, 0.0), 0.0)
                transferFunc
            return 
                { v with
                    pointSize = uniform?PointSize
                    pointColor = color
                }
        }            
      
    let internal pointSprite (p : Point<Vertex>) = 
        point {
            let wp = p.Value.wp

            let minX = uniform?MinX
            let maxX = uniform?MaxX
            let minY = uniform?MinY
            let maxY = uniform?MaxY
            let minZ = uniform?MinZ
            let maxZ = uniform?MaxZ

            let planeX = uniform?ClippingPlaneX
            let planeY = uniform?ClippingPlaneY
            let planeZ = uniform?ClippingPlaneZ

            if (wp.X >= minX && wp.X <= maxX && wp.Y >= minY && wp.Y <= maxY && wp.Z >= minZ && wp.Z <= maxZ) &&
                (wp.X <= planeX && wp.Y <= planeY && wp.Z <= planeZ) then
                yield p.Value
        }
        
    let fs (v : Vertex) = 
        fragment {
            let c = v.pointCoord * 2.0 - V2d.II
            let f = Vec.dot c c - 1.0
            if f > 0.0 then discard() // round points magic

           // if v.wp.X >= 0.0 then discard()

            //return V4d((v.velocity * 0.5 + V3d.Half).XYZ,1.0) // color according to velocity
            //return V4d(V3d(v.cubicRoots), 1.0) // color according to cubic Roots
            let color = V4d(V3d(v.pointColor), 0.9)
            return color
        }
 
type Frame = 
    {
        positions   : V3f[]
        vertices    : IBuffer
        velocities  : IBuffer
        energies    : IBuffer
        cubicRoots  : IBuffer
        strains     : IBuffer
        alphaJutzis : IBuffer
        pressures   : IBuffer
    }

let createAnimatedSg (frame : aval<int>) (pointSize : aval<float>) (renderValue : aval<RenderValue>) (tfPath : aval<string>) (domainRange : DomainRange) (clippingPlane : ClippingPlane) (frames : Frame[], bb : Box3f, vertexCount : int)  = 
    let dci = DrawCallInfo(vertexCount, InstanceCount = 1)

    let currentBuffers = frame |> AVal.map (fun i -> frames.[i % frames.Length]) 

    let vertices    = currentBuffers |> AVal.map (fun f -> f.vertices)
    let velocities  = currentBuffers |> AVal.map (fun f -> f.velocities)
    let energies    = currentBuffers |> AVal.map (fun f -> f.energies)
    let cubicRoots  = currentBuffers |> AVal.map (fun f -> f.cubicRoots)
    let strains     = currentBuffers |> AVal.map (fun f -> f.strains)
    let alphaJutzis = currentBuffers |> AVal.map (fun f -> f.alphaJutzis)
    let pressures   = currentBuffers |> AVal.map (fun f -> f.pressures)

    let texture = tfPath |> AVal.map (fun p -> FileTexture(p, TextureParams.empty) :> ITexture )

    Sg.render IndexedGeometryMode.PointList dci
    // complex, can also handly dynamic vertex data
    |> Sg.vertexBuffer DefaultSemantic.Positions (BufferView(vertices, typeof<V3f>))
    |> Sg.vertexBuffer (Sym.ofString "Velocity")  (BufferView(velocities, typeof<V3f>))
    |> Sg.vertexBuffer (Sym.ofString "Energy") (BufferView(energies, typeof<float32>))
    |> Sg.vertexBuffer (Sym.ofString "CubicRootOfDamage") (BufferView(cubicRoots, typeof<float32>))
    |> Sg.vertexBuffer (Sym.ofString "LocalStrain") (BufferView(strains, typeof<float32>))
    |> Sg.vertexBuffer (Sym.ofString "AlphaJutzi") (BufferView(alphaJutzis, typeof<float32>))
    |> Sg.vertexBuffer (Sym.ofString "Pressure") (BufferView(pressures, typeof<float32>))
    |> Sg.shader {  
            do! DefaultSurfaces.trafo
            do! Shaders.pointSprite
            do! Shaders.vs
            do! Shaders.fs
        }
    |> Sg.uniform "PointSize" pointSize
    |> Sg.uniform "TransferFunction" texture
    |> Sg.uniform "RenderValue" renderValue
    |> Sg.uniform "MinX" domainRange.minX
    |> Sg.uniform "MaxX" domainRange.maxX
    |> Sg.uniform "MinY" domainRange.minY
    |> Sg.uniform "MaxY" domainRange.maxY
    |> Sg.uniform "MinZ" domainRange.minZ
    |> Sg.uniform "MaxZ" domainRange.maxZ
    |> Sg.uniform "ClippingPlaneX" clippingPlane.clippingPlaneX
    |> Sg.uniform "ClippingPlaneY" clippingPlane.clippingPlaneY
    |> Sg.uniform "ClippingPlaneZ" clippingPlane.clippingPlaneZ
 







   