module Hera

open System
open System.IO
open Aardvark.Base
open Aardvark.Base.Rendering
open FSharp.Data.Adaptive
open Aardvark.SceneGraph

module Array = 
    open System.Collections.Generic

    let choosei (f : int -> 'a -> Option<'b>) (xs : array<'a>) : array<'b> =
        let r = List<'b>(xs.Length)
        xs |> Array.iteri (fun i e -> 
            match f i e with
            | Some v -> r.Add v 
            | None -> ()
        )
        r.ToArray()


module Shaders = 

    open Aardvark.Base.Rendering.Effects
    open FShade

    type Vertex = 
        {
            [<Position>] // gl_Position
            pos : V4d

            [<PointSize>] // gl_PointSize
            pointSize : float

            [<PointCoord>] // gl_PointCoord
            pointCoord : V2d

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
            return 
                { v with
                    pointSize = uniform?PointSize
                }
        }

    let fs (v : Vertex) = 
        fragment {
            let c = v.pointCoord * 2.0 - V2d.II
            let f = Vec.dot c c - 1.0
            if f > 0.0 then discard() // round points magic

            let transferWidth = 5.0
            let value = v.energy

            let linearCoord = value;
            let logCoord = log (transferWidth * value + 1.0)

            let color = transfer.SampleLevel(V2d(linearCoord, 0.0), 0.0)

            //return V4d((v.velocity * 0.5 + V3d.Half).XYZ,1.0) // color according to velocity
            //return V4d(V3d(v.cubicRoots), 1.0) // color according to cubic Roots
            return color
        }


let private serializer = MBrace.FsPickler.FsPickler.CreateBinarySerializer()
 

type Frame = 
    {
        positions : V3f[]
        vertices : IBuffer
        velocities : IBuffer
        energies : IBuffer
        cubicRoots : IBuffer
        strains : IBuffer
        alphaJutzis : IBuffer
        pressures : IBuffer
    }

    //not used atm !!!
let loadOldCacheFiles (runtime : IRuntime) (dir : string) (frames : int) = 
    Log.startTimed "loading hera data"
    let files = Directory.EnumerateFiles dir |> Seq.atMost frames  |> Seq.toArray 
    let mutable bb = Box3f.Invalid
    let mutable vertexCount = 0
    let buffers = 
        files 
        |> Array.choosei (fun i f -> 
                if f.EndsWith("_vs") then   
                    None
                else
                    Report.Progress(float i / float files.Length)
                    let d = File.readAllBytes f
                    let vertices : array<V3f> = serializer.UnPickle d
                    if i = 0 then 
                        bb <- Box3f(vertices |> Seq.map V3f)
                        vertexCount <- vertices.Length


                    let vs = sprintf "%s_vs" f
                    let d = File.readAllBytes vs
                    let velocities : array<V3f> = serializer.UnPickle d

                    Some 
                        {
                            vertices = runtime.PrepareBuffer (ArrayBuffer vertices)   :> IBuffer
                            velocities = runtime.PrepareBuffer (ArrayBuffer velocities) :> IBuffer
                            energies = runtime.PrepareBuffer (ArrayBuffer velocities) :> IBuffer;
                            cubicRoots = runtime.PrepareBuffer (ArrayBuffer velocities) :> IBuffer;
                            strains = runtime.PrepareBuffer (ArrayBuffer velocities) :> IBuffer;
                            alphaJutzis = runtime.PrepareBuffer (ArrayBuffer velocities) :> IBuffer;
                            pressures = runtime.PrepareBuffer (ArrayBuffer velocities) :> IBuffer;
                            positions = vertices
                        }
        )

    Log.stop()
    buffers, bb, vertexCount

let texture = 
    let path = @"..\..\..\data\transfer\map-26.png"
    FileTexture(path, TextureParams.empty) :> ITexture

let createAnimatedSg  (frame : aval<int>) (pointSize : aval<float>) (frames : Frame[], bb : Box3f, vertexCount : int)  = 
    let dci = DrawCallInfo(vertexCount, InstanceCount = 1)

    let currentBuffers = 
        frame |> AVal.map (fun i -> 
            let frame = frames.[i % frames.Length]
            frame.vertices, frame.velocities, frame.energies, frame.cubicRoots, frame.strains, frame.alphaJutzis, frame.pressures
        )

    let vertices = currentBuffers |> AVal.map (fun (a, _, _, _, _, _, _) -> a)
    let velocities = currentBuffers |> AVal.map (fun (_, b, _, _, _, _, _) -> b)
    let energies = currentBuffers |> AVal.map (fun (_, _, c, _, _, _, _) -> c)
    let cubicRoots = currentBuffers |> AVal.map (fun (_, _, _, d, _, _, _) -> d)
    let strains = currentBuffers |> AVal.map (fun (_, _, _, _, e, _, _) -> e)
    let alphaJutzis = currentBuffers |> AVal.map (fun (_, _, _, _, _, f, _) -> f)
    let pressures = currentBuffers |> AVal.map (fun (_, _, _, _, _, _, g) -> g)

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
            do! Shaders.vs
            do! Shaders.fs
        }
    |> Sg.uniform "PointSize" pointSize
    |> Sg.uniform "TransferFunction" (AVal.constant texture)