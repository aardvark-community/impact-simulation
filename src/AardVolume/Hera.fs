﻿module Hera

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
            [<Position>]
            pos : V4d

            [<PointSize>]
            pointSize : float

            [<PointCoord>]
            pointCoord : V2d

            [<Semantic("Velocity")>]
            velocity : V3d
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
            if f > 0.0 then discard()
            return V4d((v.velocity * 0.5 + V3d.Half).XYZ,1.0)
        }


let private serializer = MBrace.FsPickler.FsPickler.CreateBinarySerializer()
 

type Frame = 
    {
        positions : V3f[]
        vertices : IBuffer
        velocities : IBuffer
    }

let loadOldCacheFiles (runtime : IRuntime) (dir : string) (frames : int) = 
    Log.startTimed "loading hera data"
    let files = Directory.EnumerateFiles dir |> Seq.atMost frames  |> Seq.toArray 
    let mutable bb = Box3d.Invalid
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
                        bb <- Box3d(vertices |> Seq.map V3d)
                        vertexCount <- vertices.Length


                    let vs = sprintf "%s_vs" f
                    let d = File.readAllBytes vs
                    let velocities : array<V3f> = serializer.UnPickle(d)

                    Some 
                        {
                            vertices = runtime.PrepareBuffer (ArrayBuffer vertices)   :> IBuffer
                            velocities = runtime.PrepareBuffer (ArrayBuffer velocities) :> IBuffer
                            positions = vertices
                        }
        )

    Log.stop()
    buffers, bb, vertexCount


let createAnimatedSg  (frame : aval<int>) (frames : Frame[], bb : Box3d, vertexCount : int) = 
    let dci = DrawCallInfo(vertexCount, InstanceCount = 1)

    let currentBuffers = 
        frame |> AVal.map (fun i -> 
            let frame = frames.[i % frames.Length]
            frame.vertices, frame.velocities
        )
    let vertices = AVal.map fst currentBuffers
    let velocities = AVal.map snd currentBuffers

    Sg.render IndexedGeometryMode.PointList dci
    |> Sg.vertexBuffer DefaultSemantic.Positions (BufferView(vertices, typeof<V3f>))
    |> Sg.vertexBuffer (Sym.ofString "Velocity")  (BufferView(velocities, typeof<V3f>))
    |> Sg.shader {  
            do! DefaultSurfaces.trafo
            do! Shaders.vs
            do! Shaders.fs
        }
    |> Sg.uniform "PointSize" (AVal.constant 8.0)