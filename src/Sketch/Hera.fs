module Hera

open System
open System.IO
open Aardvark.Base
open Aardvark.Base.Rendering
open FSharp.Data.Adaptive
open Aardvark.SceneGraph
open AardVolume.Model

//[<ReflectedDefinition>]
//let isInsideBox (filter : Box3f) (pos : V3f) =
//    let min = filter.Min
//    let max = filter.Max

//    if (min.X <= pos.X && pos.X <= max.X && min.Y <= pos.Y && pos.Y <= max.Y && min.Z <= pos.Z && pos.Z <= max.Z) then
//        true
//    else
//        false

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
                let filter : Box3f = uniform?Filter
                let dataRange : Range = uniform?DataRange
                let colorValue = uniform?Color
                let value renderValue = 
                    match renderValue with
                    | RenderValue.Energy -> v.energy
                    | RenderValue.CubicRoot -> v.cubicRoot
                    | RenderValue.Strain -> v.localStrain
                    | RenderValue.AlphaJutzi -> v.alphaJutzi
                    | RenderValue.Pressure -> v.pressure
                    | _ -> v.energy
                let linearCoord = value renderValue          
                let transferFunc = transfer.SampleLevel(V2d(linearCoord, 0.0), 0.0)

                let pos = V3f(v.wp)

                let min = filter.Min
                let max = filter.Max

                let minRange = dataRange.min
                let maxRange = dataRange.max

                if (min.X <= pos.X && pos.X <= max.X && min.Y <= pos.Y && pos.Y <= max.Y && min.Z <= pos.Z && pos.Z <= max.Z && minRange <= linearCoord && linearCoord <= maxRange) then
                    transferFunc
                else
                    colorValue
            return 
                { v with
                    pointSize = uniform?PointSize
                    pointColor = color
                }
        }            
      
    let internal pointSprite (p : Point<Vertex>) = 
        point {
            let wp = p.Value.wp
            let dm : DomainRange = uniform?DomainRange
            let plane = uniform?ClippingPlane

            if (wp.X >= dm.x.min && wp.X <= dm.x.max && wp.Y >= dm.y.min && wp.Y <= dm.y.max && wp.Z >= dm.z.min && wp.Z <= dm.z.max) &&
                (wp.X <= plane.x && wp.Y <= plane.y && wp.Z <= plane.z) then
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
            let color = V4d(V3d(v.pointColor), 1.0)
            return color
        }

let createAnimatedSg (frame : aval<int>) (pointSize : aval<float>) (renderValue : aval<RenderValue>) (tfPath : aval<string>) (domainRange : aval<DomainRange>) (clippingPlane : aval<ClippingPlane>) (filter : aval<option<Box3f>>) (dataRange : aval<Range>) (colorValue : aval<C4b>) (frames : Frame[], bb : Box3f, vertexCount : int)  = 
    let dci = DrawCallInfo(vertexCount, InstanceCount = 1)

    let filterNew = filter |> AVal.map (fun f -> match f with
                                                 | Some i -> i
                                                 | None -> Box3f.Infinite)

    let currentBuffers = frame |> AVal.map (fun i -> frames.[i % frames.Length]) 

    //this conversion is actually not really neccessary
    let color = colorValue |> AVal.map (fun c -> C4d c) |> AVal.map (fun x -> V4d(x.R, x.G, x.B, x.A))

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
    |> Sg.uniform "DomainRange" domainRange
    |> Sg.uniform "ClippingPlane" clippingPlane
    |> Sg.uniform "Filter" filterNew
    |> Sg.uniform "DataRange" dataRange
    |> Sg.uniform "Color" color
  