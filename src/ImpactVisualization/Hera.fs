namespace Hera

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

    //let vs (v : Vertex) =
    //    vertex {
    //        let color = 
    //            let renderValue = uniform?RenderValue
    //            let filter : Box3f = uniform?Filter
    //            let dataRange : Range = uniform?DataRange
    //            let colorValue = uniform?Color
    //            let value renderValue = 
    //                match renderValue with
    //                | RenderValue.Energy -> v.energy
    //                | RenderValue.CubicRoot -> v.cubicRoot
    //                | RenderValue.Strain -> v.localStrain
    //                | RenderValue.AlphaJutzi -> v.alphaJutzi
    //                | RenderValue.Pressure -> v.pressure
    //                | _ -> v.energy
    //            let linearCoord = value renderValue          
    //            let transferFunc = transfer.SampleLevel(V2d(linearCoord, 0.0), 0.0)

    //            let pos = V3f(v.wp)

    //            let min = filter.Min
    //            let max = filter.Max

    //            let minRange = dataRange.min
    //            let maxRange = dataRange.max

    //            if (min.X <= pos.X && pos.X <= max.X && min.Y <= pos.Y && pos.Y <= max.Y && min.Z <= pos.Z && pos.Z <= max.Z && minRange <= linearCoord && linearCoord <= maxRange) then
    //                transferFunc
    //            else
    //                colorValue
    //        return 
    //            { v with
    //                pointSize = uniform?PointSize
    //                pointColor = color
    //            }
    //    }            
      
    let internal pointSprite (p : Point<Vertex>) = 
        point {
            let wp = p.Value.wp

            let dm : DomainRange = uniform?DomainRange
            let dataRange : Range = uniform?DataRange
            let filter : Box3f = uniform?Filter
            let filters : Filters = uniform?CurrFilters
            let discardPoints = uniform?DiscardPoints
            let renderValue = uniform?RenderValue
            let colorValue = uniform?Color


            let value renderValue = 
                match renderValue with
                | RenderValue.Energy -> p.Value.energy
                | RenderValue.CubicRoot -> p.Value.cubicRoot
                | RenderValue.Strain -> p.Value.localStrain
                | RenderValue.AlphaJutzi -> p.Value.alphaJutzi
                | RenderValue.Pressure -> p.Value.pressure
                | _ -> p.Value.energy
            let linearCoord = value renderValue         
            let transferFunc = transfer.SampleLevel(V2d(linearCoord, 0.0), 0.0)

            let notDiscardByFilters = 
                    (p.Value.energy >= filters.filterEnergy.min && p.Value.energy <= filters.filterEnergy.max) &&
                    (p.Value.cubicRoot >= filters.filterCubicRoot.min && p.Value.cubicRoot <= filters.filterCubicRoot.max) &&
                    (p.Value.localStrain >= filters.filterStrain.min && p.Value.localStrain <= filters.filterStrain.max) &&
                    (p.Value.alphaJutzi >= filters.filterAlphaJutzi.min && p.Value.alphaJutzi <= filters.filterAlphaJutzi.max) &&
                    (p.Value.pressure >= filters.filterPressure.min && p.Value.pressure <= filters.filterPressure.max) 

            let min = filter.Min
            let max = filter.Max

            let pos = V3f(wp)

            let minRange = dataRange.min
            let maxRange = dataRange.max

            let plane = uniform?ClippingPlane


            let color = 
                if (notDiscardByFilters && min.X <= pos.X && pos.X <= max.X && min.Y <= pos.Y && pos.Y <= max.Y && min.Z <= pos.Z && pos.Z <= max.Z && minRange <= linearCoord && linearCoord <= maxRange) then
                    transferFunc
                else
                    colorValue

            if (wp.X >= dm.x.min && wp.X <= dm.x.max && wp.Y >= dm.y.min && wp.Y <= dm.y.max && wp.Z >= dm.z.min && wp.Z <= dm.z.max) &&
                (wp.X <= plane.x && wp.Y <= plane.y && wp.Z <= plane.z) then
                if (discardPoints) then
                    if (notDiscardByFilters && min.X <= pos.X && pos.X <= max.X && min.Y <= pos.Y && pos.Y <= max.Y && min.Z <= pos.Z && pos.Z <= max.Z && minRange <= linearCoord && linearCoord <= maxRange) then
                        yield  { p.Value with 
                                    pointColor = color
                                    pointSize = uniform?PointSize}
                else
                    yield { p.Value with 
                                pointColor = color
                                pointSize = uniform?PointSize}
        }
        
    let fs (v : Vertex) = 
        fragment {
            let c = v.pointCoord * 2.0 - V2d.II
            let f = Vec.dot c c - 1.0
            if f > 0.0 then discard() // round points magic

            let alpha : float = uniform?Alpha
            //return V4d((v.velocity * 0.5 + V3d.Half).XYZ,1.0) // color according to velocity
            //return V4d(V3d(v.cubicRoots), 1.0) // color according to cubic Roots
            let color = V4d(v.pointColor.XYZ, alpha)
            return color
        }


module Hera =

    open Aardvark.Base.Sorting
    open FSharp.Data.Adaptive.Operators

    module Compute = 

        open FShade

        [<LocalSize(X = 64)>]
        let transform (view : M44d) (cnt : int) (src : V4d[]) (dst : V4d[]) =
            compute {
                let id = getGlobalId().X 
                if id < cnt then
                    let viewPos = view * src.[id]
                    dst.[id] <- viewPos
            }

    let createAnimatedSg (frame : aval<int>) (pointSize : aval<float>) (discardPoints : aval<bool>)  
                         (renderValue : aval<RenderValue>) (tfPath : aval<string>) 
                         (domainRange : aval<DomainRange>) (clippingPlane : aval<ClippingPlane>) 
                         (filter : aval<option<Box3f>>) (currFilters : aval<Filters>) 
                         (dataRange : aval<Range>) (colorValue : aval<C4b>) 
                         (cameraView : aval<CameraView>)
                         (runtime : IRuntime)
                         (frames : Frame[], bb : Box3f, vertexCount : int)  = 
        let dci = DrawCallInfo(vertexCount, InstanceCount = 1)


        let mutable mode = BlendMode(true)
        mode.Enabled <- true
        mode.Operation <- BlendOperation.Add
        mode.AlphaOperation <- BlendOperation.Add
        mode.SourceFactor <- BlendFactor.SourceAlpha
        mode.DestinationFactor <- BlendFactor.InvSourceAlpha
        mode.SourceAlphaFactor <- BlendFactor.One
        mode.DestinationAlphaFactor <- BlendFactor.InvSourceAlpha

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


        //let cnt = frames.[0].positions.Length

        //let idx = frame |> AVal.map (fun f -> Array.init frames.[f].positions.Length id) 

        //let bi = new BitonicSorter<V4f>(runtime, <@ fun (l : V4f) (r : V4f) -> l.Z <= r.Z @>)
        //let sorter = bi.NewInstance(cnt)


        //let input = runtime.CreateComputeShader(Compute.transform)
        //let inputBinding = runtime.NewInputBinding(input)
        //let target = runtime.CreateBuffer<V4f>(cnt)
        //let perm = runtime.CreateBuffer<int>(cnt)
        //inputBinding.["dst"] <- target.Buffer
        //inputBinding.["cnt"] <- cnt
        //let sw = System.Diagnostics.Stopwatch()
        //let mutable iter = 0
        //let run (src : IBackendBuffer) (vp : Trafo3d) = 
        //    sw.Start()
        //    inputBinding.["src"] <- src
        //    inputBinding.["view"] <- vp.Forward
        //    inputBinding.Flush()
        //    runtime.Run [
        //        ComputeCommand.Bind(input)
        //        ComputeCommand.SetInput inputBinding
        //        ComputeCommand.Dispatch(ceilDiv cnt 64)
        //        ComputeCommand.Sync(target.Buffer)
        //    ]
        //    sorter.Run(target, perm)
        //    sw.Stop()

        //    if iter >= 100 then  
        //        iter <- 0
        //        printfn "%A" (sw.Elapsed.MicroTime / 100.0)
        //        sw.Reset()

        //    iter <- iter + 1

        //    perm.Buffer :> IBuffer



        //let idx = 
        //    (cameraView, frame, idx) |||> AVal.map3 (fun c frame idx -> 
        //        let positions = frames.[frame].positions
        //        let cmp = System.Collections.Generic.Comparer<float32>.Default

        //        //let idx = positions.CreatePermutationSmoothSort(fun a b ->  cmp.Compare((a- V3f c.Location).LengthSquared , (b - V3f c.Location).LengthSquared))
        //        Log.startTimed "sorting: %A" positions.Length
        //        idx |> Array.sortInPlaceBy (fun idx -> -((positions.[idx] - V3f c.Location).LengthSquared))

        //        Log.stop()
        //        //positions |> Array.sortBy (fun p -> -(p - V3f c.Location).LengthSquared)
        //        idx |> Array.copy
        //    )

        //let idx = 
        //    (cameraView, frame) ||> AVal.map2 (fun c frame -> 
        //        let vp = c.ViewTrafo
        //        let positions = frames.[frame].positions
        //        run (frames.[frame].vertices |> unbox) vp
        //    )

        let texture = tfPath |> AVal.map (fun p -> FileTexture(p, TextureParams.empty) :> ITexture )

        //let index b (c : ISg)  = Sg.VertexIndexApplicator(BufferView(b, typeof<int>), c) :> ISg

        Sg.render IndexedGeometryMode.PointList dci
        // complex, can also handly dynamic vertex data
        //|> index idx
        //|> Sg.vertexAttribute DefaultSemantic.Positions vertices
        |> Sg.vertexBuffer DefaultSemantic.Positions (BufferView(vertices, typeof<V4f>))
        |> Sg.vertexBuffer (Sym.ofString "Velocity")  (BufferView(velocities, typeof<V3f>))
        |> Sg.vertexBuffer (Sym.ofString "Energy") (BufferView(energies, typeof<float32>))
        |> Sg.vertexBuffer (Sym.ofString "CubicRootOfDamage") (BufferView(cubicRoots, typeof<float32>))
        |> Sg.vertexBuffer (Sym.ofString "LocalStrain") (BufferView(strains, typeof<float32>))
        |> Sg.vertexBuffer (Sym.ofString "AlphaJutzi") (BufferView(alphaJutzis, typeof<float32>))
        |> Sg.vertexBuffer (Sym.ofString "Pressure") (BufferView(pressures, typeof<float32>))
        |> Sg.shader {  
                do! DefaultSurfaces.trafo
                do! Shaders.pointSprite
               // do! Shaders.vs
                do! Shaders.fs
            }
        |> Sg.uniform "PointSize" pointSize
        |> Sg.uniform "DiscardPoints" discardPoints
        |> Sg.uniform "TransferFunction" texture
        |> Sg.uniform "RenderValue" renderValue
        |> Sg.uniform "DomainRange" domainRange
        |> Sg.uniform "ClippingPlane" clippingPlane
        |> Sg.uniform "Filter" filterNew
        |> Sg.uniform "CurrFilters" currFilters
        |> Sg.uniform "DataRange" dataRange
        |> Sg.uniform "Color" color
        |> Sg.uniform "Alpha" ~~0.01
        //|> Sg.depthTest ~~DepthTestMode.None
        //|> Sg.blendMode ~~mode
  