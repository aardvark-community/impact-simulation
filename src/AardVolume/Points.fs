﻿module DSLAwesomness.Points

open System

open Aardvark.Rendering.Vulkan
open Aardvark.Base
open Aardvark.Application.Slim
open Aardvark.Base.Rendering
open Aardvark.SceneGraph
open Aardvark.Application

open FSharp.Data.Adaptive


module Shaders = 
    open FShade

    type Vertex = 
        {
            [<Position>]
            pos : V4d

            [<PointSize>]
            pointSize : float

            [<PointCoord>]
            pointCoord : V2d

            [<Color>]
            color : V4d
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
            return V4d((v.color).XYZ,1.0)
        }

    let fs2 (v : Vertex) = 
        fragment {
            let c = v.pointCoord * 2.0 - V2d.II
            let f = Vec.dot c c - 1.0
            //if f > 0.0 then discard()
            return V4d.IIII
        }


let runSingleFrame args =
    Aardvark.Init()

    let vertices,velocities,bb = Parser.parseFile @"D:\volumes\univie\r80_p0_m500_v6000_mbasalt_a1.0_1M\impact.0400"
    
    
    //let vertices,velocities,bb = Parser.parseFile @"D:\volumes\univie\r80_p0_m500_v6000_mbasalt_a4.0_1M\impact.0400"
    //let vertices,velocities,bb = Parser.parseFile @"D:\volumes\univie\r80_p45_m500_v6000_mbasalt_a1.25_1M\impact.0400"

    let app = new OpenGlApplication()


    let win = app.CreateGameWindow(4)
    //let win = new Aardvark.Application.Slim.GameWindow(app.Runtime, false, 4, true)

    let t = Trafo3d.Translation -bb.Center * Trafo3d.Scale (20.0 / bb.Size.NormMax)
    let initialCam = CameraView.lookAt (V3d.III * 30.0) V3d.Zero V3d.OOI
    let c = DefaultCameraController.control win.Mouse win.Keyboard win.Time initialCam

    let f = win.Sizes |> AVal.map (fun s -> Frustum.perspective 60.0 0.01 100.0 (float s.X / float s.Y))


    let sw = System.Diagnostics.Stopwatch.StartNew()
    let vertices = 
        win.Time |> AVal.map (fun _ -> 
            let t = (sw.Elapsed.TotalSeconds % 5.0 ) * 1.5
            (vertices, velocities) ||> Array.map2 (fun p v -> 
                p + t * v |> V3f
            )
    )

    let eyeSeparation = V3d(-0.04, 0.0, 0.0)

    let stereoViews =
        let half = eyeSeparation * 0.5
        c  |> AVal.map (fun v -> 
            let t = CameraView.viewTrafo v
            [|
                t * Trafo3d.Translation(-half)
                t * Trafo3d.Translation(half)
            |]
        )

    let stereoProjs =
        win.Sizes 
        // construct a standard perspective frustum (60 degrees horizontal field of view,
        // near plane 0.1, far plane 50.0 and aspect ratio x/y.
        |> AVal.map (fun s -> 
            let ac = 30.0
            let ao = 30.0
            let near = 0.01
            let far = 10.0
            let aspect = float s.X / float s.Y
            let sc = tan (Conversion.RadiansFromDegrees ac) * near
            let so = tan (Conversion.RadiansFromDegrees ao) * near
            let sv = tan (0.5 * Conversion.RadiansFromDegrees (ac + ao)) * near

            let leftEye = { left = -sc; right = +so; bottom = -sv / aspect; top = +sv / aspect; near = near; far = far; isOrtho = false }
            let rightEye = { left = -so; right = +sc; bottom = -sv / aspect; top = +sv / aspect; near = near; far = far; isOrtho = false }
            [|
                Frustum.perspective 90.0 0.01 100.0 aspect   |> Frustum.projTrafo
                Frustum.perspective 90.0 0.01 100.0 aspect   |> Frustum.projTrafo
                //Frustum.projTrafo leftEye
                //Frustum.projTrafo rightEye
            |]
        )

    let mutable blend = 
        BlendMode(true)

    blend.AlphaOperation <- BlendOperation.Add
    blend.Operation <- BlendOperation.Add
    blend.SourceAlphaFactor <- BlendFactor.One
    blend.DestinationAlphaFactor <- BlendFactor.One
    blend.SourceFactor <- BlendFactor.One
    blend.DestinationFactor <- BlendFactor.One

    win.RenderTask <-
        Sg.draw IndexedGeometryMode.PointList
        |> Sg.vertexAttribute DefaultSemantic.Positions vertices
        |> Sg.vertexAttribute' DefaultSemantic.Colors (velocities |> Array.map ( fun v -> ((v.Normalized + V3d.III) * 0.5) |> V3f ))
        |> Sg.shader {  
             do! DefaultSurfaces.trafo
             do! Shaders.vs
             do! Shaders.fs
             //do! DefaultSurfaces.pointSprite
             //do! DefaultSurfaces.pointSpriteFragment
           }
        |> Sg.uniform "PointSize" (AVal.constant 8.0)

        |> Sg.transform t
        |> Sg.viewTrafo (c |> AVal.map CameraView.viewTrafo)
        |> Sg.projTrafo (f |> AVal.map Frustum.projTrafo)
        |> Sg.uniform "ViewTrafo" stereoViews
        |> Sg.uniform "ProjTrafo" stereoProjs
        //|> Sg.blendMode (Mod.constant blend)
        //|> Sg.depthTest (Mod.constant DepthTestMode.None)
        |> Sg.compile app.Runtime win.FramebufferSignature

    win.Run()

    0

open System.IO
open System.Runtime.InteropServices
open MBrace.FsPickler
open MBrace.FsPickler.Combinators
open System.Threading.Tasks

let run args =
    Aardvark.Init()

    let p = MBrace.FsPickler.FsPickler.CreateBinarySerializer()

    let app = new OpenGlApplication()

    // creates a new game window with samples = 8 (not showing it). neeeds to be disposed.

    let mutable bb = Box3d.Invalid

    let files = Directory.EnumerateFiles(@"F:\vrvis\sim") |> Seq.skip 200 |> Seq.atMost 10  |> Seq.toArray 
    let buffers = 
        files 
        |> Array.choosei (fun i f -> 
                if f.EndsWith("_vs") then   
                    None
                else
                    Report.Progress(float i / float files.Length)
                    let d = File.readAllBytes f
                    let vertices : array<V3f> = p.UnPickle(d)
                    if i = 0 then 
                        bb <- Box3d(vertices |> Seq.map V3d)


                    let vs = sprintf "%s_vs" f
                    let d = File.readAllBytes vs
                    let velocities : array<V3f> = p.UnPickle(d)
                    let colors = velocities |> Array.map ( fun v -> ((v.Normalized + V3f.III) * 0.5f) )

                    Some (app.Runtime.PrepareBuffer (ArrayBuffer vertices) :> IBuffer, app.Runtime.PrepareBuffer (ArrayBuffer colors) :> IBuffer, vertices)
        )


    let bb : Box3d = Box3d.FromCenterAndSize(V3d.Zero,V3d.III)

    let win = app.CreateGameWindow() // new Aardvark.Application.Slim.GameWindow(app.Runtime, false, 4, true)

    let t = Trafo3d.Translation -bb.Center * Trafo3d.Scale (20.0 / bb.Size.NormMax)
    let initialCam = CameraView.lookAt (V3d.III * 30.0) V3d.Zero V3d.OOI
    let c = DefaultCameraController.control win.Mouse win.Keyboard win.Time initialCam

    let f = win.Sizes |> AVal.map (fun s -> Frustum.perspective 60.0 0.01 100.0 (float s.X / float s.Y))


    let sw = System.Diagnostics.Stopwatch.StartNew()
    let frameDt = 0.05
    let currentBuffers = 
        win.Time |> AVal.map (fun _ -> 
            let t = (sw.Elapsed.TotalSeconds / frameDt) |> int
            let i = t % buffers.Length
            Log.line "frame: %d" i
            let vertices, colors, array = buffers.[i]
            vertices, colors
    )
    let vertices = AVal.map fst currentBuffers
    let colors = AVal.map snd currentBuffers

    let eyeSeparation = V3d(-0.04, 0.0, 0.0)

    let stereoViews =
        let half = eyeSeparation * 0.5
        c  |> AVal.map (fun v -> 
            let t = CameraView.viewTrafo v
            [|
                t * Trafo3d.Translation(-half)
                t * Trafo3d.Translation(half)
            |]
        )

    let stereoProjs =
        win.Sizes 
        // construct a standard perspective frustum (60 degrees horizontal field of view,
        // near plane 0.1, far plane 50.0 and aspect ratio x/y.
        |> AVal.map (fun s -> 
            let ac = 30.0
            let ao = 30.0
            let near = 0.01
            let far = 10.0
            let aspect = float s.X / float s.Y
            let sc = tan (Conversion.RadiansFromDegrees ac) * near
            let so = tan (Conversion.RadiansFromDegrees ao) * near
            let sv = tan (0.5 * Conversion.RadiansFromDegrees (ac + ao)) * near

            let leftEye = { left = -sc; right = +so; bottom = -sv / aspect; top = +sv / aspect; near = near; far = far; isOrtho = false }
            let rightEye = { left = -so; right = +sc; bottom = -sv / aspect; top = +sv / aspect; near = near; far = far; isOrtho = false }
            [|
                Frustum.perspective 90.0 0.01 100.0 aspect   |> Frustum.projTrafo
                Frustum.perspective 90.0 0.01 100.0 aspect   |> Frustum.projTrafo
                //Frustum.projTrafo leftEye
                //Frustum.projTrafo rightEye
            |]
        )

    let mutable blend = 
        BlendMode(true)

    blend.AlphaOperation <- BlendOperation.Add
    blend.Operation <- BlendOperation.Add
    blend.SourceAlphaFactor <- BlendFactor.One
    blend.DestinationAlphaFactor <- BlendFactor.One
    blend.SourceFactor <- BlendFactor.One
    blend.DestinationFactor <- BlendFactor.One

    let (p,v,xs) = buffers.[0]
    let count = xs.Length
    let mutable dci = DrawCallInfo(count)
    dci.InstanceCount <- 1

    win.RenderTask <-
        Sg.render IndexedGeometryMode.PointList dci
        |> Sg.vertexBuffer DefaultSemantic.Positions (BufferView(vertices, typeof<V3f>))
        |> Sg.vertexBuffer DefaultSemantic.Colors  (BufferView(colors, typeof<V3f>))
        |> Sg.shader {  
             do! DefaultSurfaces.trafo
             //do! DefaultSurfaces.constantColor C4f.White
             do! Shaders.vs
             do! Shaders.fs
             //do! DefaultSurfaces.pointSprite
             //do! DefaultSurfaces.pointSpriteFragment
           }
        |> Sg.uniform "PointSize" (AVal.constant 8.0)
        |> Sg.viewTrafo (c |> AVal.map CameraView.viewTrafo)
        |> Sg.projTrafo (f |> AVal.map Frustum.projTrafo)
        |> Sg.uniform "ViewTrafo" stereoViews
        |> Sg.uniform "ProjTrafo" stereoProjs
        //|> Sg.blendMode (Mod.constant blend)
        //|> Sg.depthTest (Mod.constant DepthTestMode.None)
        |> Sg.compile app.Runtime win.FramebufferSignature

    win.Run()

    0
