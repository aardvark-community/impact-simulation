namespace AardVolume

open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.SceneGraph
open Aardvark.Application
open System.IO

open FSharp.Data.Adaptive

[<ReflectedDefinition>]
module Shader =
    open FShade

    let volumeTexture =
        sampler3d {
            texture uniform?VolumeTexture
            filter Filter.MinMagLinear
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
            addressW WrapMode.Clamp
        }

    let pickRay (p : V2d) =
        let pn = uniform.ViewProjTrafoInv * V4d(p.X, p.Y, 0.0, 1.0)
        let nearPlanePoint = pn.XYZ / pn.W
        Vec.normalize nearPlanePoint

    type Vertex =
        {
            [<Position>]
            pos : V4d

            [<Semantic("RayDirection")>]
            dir : V3d

            [<Semantic("CubeCoord")>]
            cubeCoord : V3d

            
            [<Semantic("CamModel")>]
            camModel : V3d
        }

    let vertex (v : Vertex) =
        vertex {
            let cameraInModel = uniform.ModelTrafoInv.TransformPos uniform.CameraLocation
            let wp = uniform.ModelTrafo * v.pos
            return {
                pos = uniform.ViewProjTrafo * wp
                dir = v.pos.XYZ - cameraInModel
                cubeCoord = v.pos.XYZ
                camModel = cameraInModel
            }
        }

    // worst practice yet simple volume shader
    let fragment (v : Vertex) =
        fragment {
            let size = volumeTexture.Size
            let mutable color = V3d.Zero
                
            let mutable sampleLocation = v.cubeCoord

            let steps = 100

            let dir = -Vec.normalize v.dir / float steps
            let mutable stop = false

            do
                while sampleLocation.X >= 0.0 && sampleLocation.X <= 1.0 && sampleLocation.Y >= 0.0 && sampleLocation.Y <= 1.0 && sampleLocation.Z >= 0.0 && sampleLocation.Z <= 1.0 && not stop do
                    color <- color + V3d.III * volumeTexture.SampleLevel(sampleLocation, 0.0).X
                    sampleLocation <- sampleLocation + dir
                    let viewPos = uniform.ModelViewProjTrafo * V4d(sampleLocation,1.0)
                    if viewPos.Z / viewPos.W > 1.0 then stop <- true // stop looking backwards

            return V4d(2.0 * color / float steps, 1.0)
        }

module SimpleVolumeRenderer = 

    let createSg (runtime : IRuntime) (folder : string) = 
        Log.startTimed "creating volume sg"
        let files = Directory.GetFiles folder

        let images = files |> Array.map (fun p -> PixImage.Create(p).ToPixImage<byte>(Col.Format.Gray))

        let s2d = images.[0].Size
        let volume = PixVolume<byte>(s2d.X, s2d.Y, files.Length, 1)
        for layer in 0 .. images.Length - 1 do
            volume.Tensor4.SubImageAtZ(int64 layer).Set(images.[layer].Volume) |> ignore

        let texture = PixTexture3d(volume, false) :> ITexture
        let texture = runtime.PrepareTexture(texture) :> ITexture |> AVal.constant

        let size = V3d volume.Size / float volume.Size.NormMax

        let sg = 
            Sg.box' C4b.Red (Box3d(-size, size))
            |> Sg.uniform "VolumeTexture" texture
            |> Sg.shader {
                    do! Shader.vertex
                    do! Shader.fragment
                }
            |> Sg.cullMode (AVal.constant CullMode.Front)

        Log.stop()

        sg

    let main argv = 
    
        Aardvark.Init()

        let win = window {
            backend Backend.GL
            display Display.Mono
            debug true
            verbosity DebugVerbosity.Warning
            samples 1
        }

        let sg = createSg win.Runtime @"F:\notebooks\hechtkopfsalamander male - Copy"

        win.Scene <- sg
        win.Run()

        0
