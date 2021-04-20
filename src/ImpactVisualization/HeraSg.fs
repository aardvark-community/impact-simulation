namespace HeraSg

open System
open System.IO
open Aardvark.Base
open Aardvark.Base.Rendering
open FSharp.Data.Adaptive
open Aardvark.SceneGraph
open AardVolume.Model
open System.Runtime.InteropServices
open System.Threading

module Shaders = 

    open FShade

    type Vertex = 
        {
            [<Position>] // gl_Position
            pos : V4d

            [<WorldPosition>]
            wp : V4d

            [<Normal>]
            normal : V3d

            [<Depth(DepthWriteMode.OnlyGreater)>]
            depth : float

            [<Semantic("DepthRange"); Interpolation(InterpolationMode.Flat)>] 
            depthRange : float

            [<PointSize>] // gl_PointSize
            pointSize : float

            [<Semantic("PointPixelSize")>] ps : float

            [<Semantic("linearCoord")>]
            linearCoord : float

            [<PointCoord>] // gl_PointCoord
            pointCoord : V2d

            [<FragCoord>] 
            fc : V4d

            [<Color>]
            pointColor : V4d

            [<Semantic("Velocity")>] 
            velocity : V3d

            [<Semantic("Mass")>]
            mass : float

            [<Semantic("Density")>]
            density : float

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

    type UniformScope with
        member x.SpheresInfos : V4f[] = uniform?StorageBuffer?SpheresInfos

    //let pointInSphere (point : V3d) (sphere : Sphere3d) = 
    //    let pos = sphere.Center
    //    let r = sphere.Radius
    //    (point.X - pos.X) ** 2.0 + (point.Y - pos.Y) ** 2.0 + (point.Z - pos.Z) ** 2.0 <= r ** 2.0

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

    let vs (v : Vertex) = 
        vertex {
            let mv = uniform.ViewTrafo
            let vp = mv * v.wp

            let s = uniform.PointSize / V2d uniform.ViewportSize
            let opp = uniform.ProjTrafo * vp
            let pp0 = opp.XYZ / opp.W

            let vpx = 
                let temp = uniform.ProjTrafoInv * V4d(pp0 + V3d(s.X, 0.0, 0.0), 1.0)
                temp.XYZ / temp.W
              
            let vpy = 
                let temp = uniform.ProjTrafoInv * V4d(pp0 + V3d(0.0, s.Y, 0.0), 1.0)
                temp.XYZ / temp.W

            let worldRadius =
                0.5 * (Vec.length (vp.XYZ - vpx) + Vec.length (vp.XYZ - vpy))

            let vpz = vp + V4d(0.0, 0.0, worldRadius, 0.0)
            let ppz = uniform.ProjTrafo * vpz
            let ppz = ppz.XYZ / ppz.W

            let depthRange = abs (ppz.Z - pp0.Z)

            let pixelDist = 
                uniform.PointSize
 
            let pixelDist = 
                if ppz.Z < -1.0 then -1.0
                else pixelDist

            return { v with ps = floor pixelDist; pointSize = pixelDist; pos = V4d(ppz, 1.0); depthRange = depthRange; }

        }
    

    let internal pointSprite (p : Point<Vertex>) = 
        point {
            let pos = p.Value.pos
            let modM = uniform.ModelTrafo
            let wp  = p.Value.wp //modM * pos

            let dm : DomainRange = uniform?DomainRange
            let dataRange : Range = uniform?DataRange
            let filter : Box3f = uniform?Filter
            let filters : Filters = uniform?CurrFilters
            let discardPoints = uniform?DiscardPoints
            let renderValue = uniform?RenderValue
            let colorValue = uniform?Color
            let normalizeData = uniform?NormalizeData
            let outliersRange : Range = uniform?OutliersRange 

         

            let value renderValue = 
                match renderValue with
                | RenderValue.Energy -> p.Value.energy
                | RenderValue.CubicRoot -> p.Value.cubicRoot
                | RenderValue.Strain -> p.Value.localStrain
                | RenderValue.AlphaJutzi -> p.Value.alphaJutzi
                | RenderValue.Pressure -> p.Value.pressure
                | RenderValue.Mass -> p.Value.mass
                | RenderValue.Density -> p.Value.density
                | _ -> p.Value.energy

            let currValue = value renderValue  
            let linearCoord = if normalizeData then (currValue - dataRange.min) / (dataRange.max - dataRange.min) else currValue

            //let temp = (currValue - dataRange.min) / (dataRange.max - dataRange.min) // normalized values!!! 
            //let linearCoord = Math.Pow(temp, 1.0/2.0)
            //let transferFunc = transfer.SampleLevel(V2d(currValue, 0.0), 0.0)
            //let linearCoord = value renderValue
            
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
            let minRange = if normalizeData then 0.0 else dataRange.min
            let maxRange = if normalizeData then 1.0 else dataRange.max
            let isInsideMinMaxRange = min.X <= pos.X && pos.X <= max.X && min.Y <= pos.Y && pos.Y <= max.Y && min.Z <= pos.Z && pos.Z <= max.Z

            let minOutlier = outliersRange.min
            let maxOutlier = outliersRange.max
            let isInsideOutlierRange = minOutlier <= currValue && currValue <= maxOutlier

            let plane = uniform?ClippingPlane

            let isInAllRanges = notDiscardByFilters && isInsideMinMaxRange && minRange <= linearCoord && linearCoord <= maxRange && isInsideOutlierRange

            let color = 
                if (isInAllRanges) then
                    transferFunc
                else
                    colorValue

            if (wp.X >= dm.x.min && wp.X <= dm.x.max && wp.Y >= dm.y.min && wp.Y <= dm.y.max && wp.Z >= dm.z.min && wp.Z <= dm.z.max) &&
                (wp.X <= plane.x && wp.Y <= plane.y && wp.Z <= plane.z) then
                if (discardPoints) then
                    if (isInAllRanges) then
                        yield  { p.Value with 
                                    pointColor = color
                                    pointSize = uniform?PointSize
                                    linearCoord = linearCoord // give linearCoord to fs or decide alpha here - in fs it might be more flexible - one could fade out points
                               }
                else
                    yield { p.Value with 
                                pointColor = color
                                pointSize = uniform?PointSize
                                linearCoord = linearCoord
                          }
        }
      
    let internal pointSpriteVr (p : Point<Vertex>) = 
        point {
            let wp = p.Value.wp
            let pPos = V3d(wp)

            let modelMatrixInv = uniform.ModelTrafoInv
            let wpInv = modelMatrixInv * wp
            let posInv = V3f(wpInv)

            let modelMatrix = uniform.ModelTrafo

            let dm : DomainRange = uniform?DomainRange
            let dataRange : Range = uniform?DataRange
            let filterBox : Box3f = uniform?Filter
            let boxIsSet : bool = uniform?BoxSet
            let filters : Filters = uniform?CurrFilters
            let discardPoints = uniform?DiscardPoints
            let renderValue = uniform?RenderValue
            let colorValue = uniform?Color
            let normalizeData = uniform?NormalizeData
            let outliersRange : Range = uniform?OutliersRange 
            let plane = uniform?ClippingPlane
            let pSize = uniform?PointSize

            //VR Variables only
            let controllerPlane : Plane3d = uniform?ControllerClippingPlane
            let sphereProbe : Sphere3d = uniform?SphereProbe
            let probesLength = uniform?SpheresLength
            let allProbes : V4f[] = uniform.SpheresInfos

            let value renderValue = 
                match renderValue with
                | RenderValue.Energy -> p.Value.energy
                | RenderValue.CubicRoot -> p.Value.cubicRoot
                | RenderValue.Strain -> p.Value.localStrain
                | RenderValue.AlphaJutzi -> p.Value.alphaJutzi
                | RenderValue.Pressure -> p.Value.pressure
                | RenderValue.Mass -> p.Value.mass
                | RenderValue.Density -> p.Value.density
                | _ -> p.Value.energy
            
            let currValue = value renderValue  
            let linearCoord = if normalizeData then (currValue - dataRange.min) / (dataRange.max - dataRange.min) else currValue
            
            let transferFunc = transfer.SampleLevel(V2d(linearCoord, 0.0), 0.0)

            let notDiscardByFilters = 
                (p.Value.energy >= filters.filterEnergy.min && p.Value.energy <= filters.filterEnergy.max) &&
                (p.Value.cubicRoot >= filters.filterCubicRoot.min && p.Value.cubicRoot <= filters.filterCubicRoot.max) &&
                (p.Value.localStrain >= filters.filterStrain.min && p.Value.localStrain <= filters.filterStrain.max) &&
                (p.Value.alphaJutzi >= filters.filterAlphaJutzi.min && p.Value.alphaJutzi <= filters.filterAlphaJutzi.max) &&
                (p.Value.pressure >= filters.filterPressure.min && p.Value.pressure <= filters.filterPressure.max) 

            let min = filterBox.Min
            let max = filterBox.Max
            let isInsideBoxFilter = min.X <= posInv.X && posInv.X <= max.X && min.Y <= posInv.Y && posInv.Y <= max.Y && min.Z <= posInv.Z && posInv.Z <= max.Z

            let minOutlier = outliersRange.min
            let maxOutlier = outliersRange.max
            let isInsideOutlierRange = minOutlier <= currValue && currValue <= maxOutlier

            //let minRange = dataRange.min
            //let maxRange = dataRange.max
            let isInAllRanges = notDiscardByFilters && isInsideBoxFilter && isInsideOutlierRange //&& minRange <= linearCoord && linearCoord <= maxRange

            // DISCARD EVALUATIONS 
            let pointInDomainRange = wpInv.X >= dm.x.min && wpInv.X <= dm.x.max && wpInv.Y >= dm.y.min && wpInv.Y <= dm.y.max && wpInv.Z >= dm.z.min && wpInv.Z <= dm.z.max
            let pointInsidePlanes = wpInv.X <= plane.x && wpInv.Y <= plane.y && wpInv.Z <= plane.z
            let isOutsideControllerPlane = 
                if controllerPlane.Normal = V3d.Zero then
                    true
                else 
                    Vec.Dot(controllerPlane.Normal, pPos) - controllerPlane.Distance >= 0.0
            let discardByRanges = if isInAllRanges then false else discardPoints

            // POINT IN CURRENT PROBE 
            let spPos = sphereProbe.Center
            let r = sphereProbe.Radius
            let isInsideCurrProbe = 
                if r < 0.0 then 
                    false
                else 
                    (pPos.X - spPos.X) ** 2.0 + (pPos.Y - spPos.Y) ** 2.0 + (pPos.Z - spPos.Z) ** 2.0 <= r ** 2.0

            // POINT IN ANY OF THE PLACED PROBES
            let pointInAnyProbe = 
                let mutable isInsideAProbe = false
                for i in 0 .. probesLength - 1 do 
                    let currProbe = allProbes.[i]
                    let pos = currProbe.XYZ |> V3d
                    let radius = currProbe.W |> float
                    if (pPos.X - pos.X) ** 2.0 + (pPos.Y - pos.Y) ** 2.0 + (pPos.Z - pos.Z) ** 2.0 <= radius ** 2.0 then 
                        isInsideAProbe <- true
                isInsideAProbe

            // RESULTING COLOR
            let color = 
                if probesLength <= 0 && r < 0.0 then 
                    if isInAllRanges then transferFunc else colorValue 
                else
                    if pointInAnyProbe || isInsideCurrProbe then transferFunc else V4d(transferFunc.XYZ/2.0, 1.0)

            if pointInDomainRange && pointInsidePlanes && isOutsideControllerPlane && not discardByRanges then
                yield  { p.Value with 
                            pointColor = color
                            pointSize = pSize;
                            linearCoord = linearCoord // give linearCoord to fs or decide alpha here - in fs it might be more flexible - one could fade out points
                       }
        }

    let pointSpriteFragment (v : Vertex) = 
          fragment {
             let mutable cc = v.pointCoord
             let c = v.pointCoord * 2.0 - V2d.II
             let f = Vec.dot c c - 1.0
             if f > 0.0 then discard()
     
             let t = 1.0 - sqrt (-f)
             let depth = v.fc.Z
             let outDepth = depth + v.depthRange * t

             let c = c * (1.0 + 2.0 / v.ps)
             let f = Vec.dot c c 
             let diffuse = sqrt (max 0.0 (1.0 - f))
             let color = V3d.III * diffuse

             return { v with pointColor = V4d(color, 1.0); depth = outDepth }
          }
        
    let fs (v : Vertex) = 
        fragment {
            let c = v.pointCoord * 2.0 - V2d.II
            let f = Vec.dot c c - 1.0
            if f > 0.0 then discard() // round points magic

            let ambient = V4d(0.4, 0.375, 0.35, 1.0)
            let diffuse = 0.6
            let specular = 0.3

            let modelMatrix = uniform.ModelTrafo
            let viewMatrixTF : Trafo3d = uniform?ViewTrafo
            let viewVector : V3d = uniform?ViewVector
            let viewMatrix = viewMatrixTF.Forward
            let viewMatrixInv = viewMatrixTF.Backward
            let projectionMatrix = uniform.ProjTrafo
            let viewProjMatrix = uniform.ViewProjTrafo
            let modelMatrixInv = uniform.ModelTrafoInv
    
            let shading = uniform?EnableShading
            //let reconstructNormal = uniform?ReconstructNormal
            let reconstructDepth : bool = uniform?ReconstructDepth
            let pointRadius : float = uniform?PointRadius

            //Normal reconstruction
            let distToCenter_squared = Vec.dot c c 
            let normalVec = V3d(c, Math.Sqrt(1.0 - distToCenter_squared))
            let normal_normalized = normalVec |> Vec.normalize  // IN SCREEN SPACE!??!!??!!!!!
            let normal =  V4d(normal_normalized, 1.0)



            //let norm = if reconstructNormal then normalVec_normalized else v.normal

            //BLINN-PHONG LIGHTING MODEL FROM MY LECTURE
            let lightPos = V4d(0.0, 100.0, 0.0, 1.0)
            let spherePos_view = viewMatrix * v.wp
            let posOnSphere = spherePos_view + V4d(normal_normalized * pointRadius, 1.0)// Radius is different for 2D and VR !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            //Depth reconstruction
            let posOnSphere_screen = projectionMatrix * posOnSphere
            let ndc_depth = posOnSphere_screen.Z / posOnSphere_screen.W

            
            let t = 1.0 - sqrt (-f)
            let depth = v.fc.Z
            let outDepth = depth + v.depthRange * t

            let dep = if reconstructDepth then outDepth else depth

            let lightPos_view = viewMatrix * lightPos
            //let lightPos = if reconstructNormal then lightPos_view else V4d(lp, 1.0)
            //let posSphere = if reconstructNormal then posOnSphere else v.wp
            let lightDir = Vec.normalize(lightPos_view.XYZ - posOnSphere.XYZ)
            //let viewDir = Vec.normalize(-posOnSphere.XYZ)
            
            //let vVec = viewMatrix.C2.XYZ |> Vec.normalize

            let viewVec_view = viewMatrix * V4d(viewVector, 1.0) |> Vec.normalize |> V3d

            let c = c * (1.0 + 2.0 / v.ps)
            let f = Vec.dot c c 

            //Diffuse term
            let diff =
                let value = 1.0 - f //Vec.dot normal_normalized lightDir
                max 0.0 value

            //Speculat term 
            //let halfwayDir = Vec.normalize(lightDir + viewDir)
            //let spec = Math.Pow(Math.Max(Vec.Dot(normal_normalized, halfwayDir), 0.0), 128.0)

            // Final Color
            let color = V4d(v.pointColor.XYZ, 1.0)
            let fragmentColor = (ambient * color) + (diffuse * diff * color) //+ (specular * spec * color)
            let finalColor = if shading then fragmentColor else color

            let alpha : float = uniform?Alpha
            //return V4d((v.velocity * 0.5 + V3d.Half).XYZ,1.0) // color according to velocity
            //return V4d(V3d(v.cubicRoots), 1.0) // color according to cubic Roots
            //let color = V4d(v.pointColor.XYZ, 1.0)
            //return V4d(v.pointColor.XYZ * l, 1.0)

            return {v with 
                        pointColor = finalColor
                        //pointColor = V4d(c.XYZ,0.05 * v.linearCoord) // proper transfer function needed here for transparency
                        depth = dep}
        }


module HeraSg =

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

    // utilities - for rendering - no outside use.
    let private composeBackwards =
        // check composition scheme if this is really smartest
        let mutable mode = BlendMode(true)
        mode.Enabled <- true
        mode.Operation <- BlendOperation.Add
        mode.AlphaOperation <- BlendOperation.Add
        mode.SourceFactor <- BlendFactor.SourceAlpha
        mode.DestinationFactor <- BlendFactor.InvSourceAlpha
        mode.SourceAlphaFactor <- BlendFactor.One
        mode.DestinationAlphaFactor <- BlendFactor.InvSourceAlpha
        mode


    let private createIndex (vertexCount : int) (positions : aval<V3f[]>) (cameraLocation : aval<V3d>) = 
        let currentBuffer = cval (Array.init vertexCount id)
        let sorter () = 
            while true do
                // get current positiosn and camera location
                let cameraLocation = cameraLocation.GetValue() |> V3f
                let positions = positions.GetValue()

                // compute distances
                let distances = positions |> Array.map (fun (p : V3f) -> (cameraLocation - p).LengthSquared)
                // create permutation array
                let indices = distances.CreatePermutationQuickSortDescending()
                // update index buffer
                transact (fun _ -> currentBuffer.Value <- indices)

        let sortThread = Thread(sorter)
        // no clean shudown implemented (TODO), thus, we use background thread which goes away on shutdown
        sortThread.IsBackground <- true
        sortThread.Priority <- ThreadPriority.Lowest // should not be too invasive
        sortThread.Start()

        currentBuffer :> aval<_>

    let transparencyModes (transparencyEnabled : aval<bool>) (sg : ISg) =
        let depthWrite = transparencyEnabled |> AVal.map not // enabled depth write if no transparancy wanted
        let blendMode = transparencyEnabled |> AVal.map (function true -> composeBackwards | _ -> BlendMode.None) // blending only if transparency enabled
        sg |> Sg.depthMask depthWrite |> Sg.blendMode blendMode

    let createAnimatedSg (frame : aval<int>) (pointSize : aval<float>) (discardPoints : aval<bool>) 
                         (normalizeData : aval<bool>) (enableShading : aval<bool>)
                         (reconstructNormal : aval<bool>) (reconstructDepth : aval<bool>)
                         (lowerOutliers : aval<bool>) (higherOutliers : aval<bool>) (outliersRange : aval<Range>)
                         (renderValue : aval<RenderValue>) (tfPath : aval<string>) 
                         (domainRange : aval<DomainRange>) (clippingPlane : aval<ClippingPlane>) 
                         (filterBox : aval<option<Box3f>>)
                         (currFilters : aval<Filters>) 
                         (dataRange : aval<Range>) (colorValue : aval<C4b>) 
                         (cameraView : aval<CameraView>) (viewVector : aval<V3d>)
                         (runtime : IRuntime)
                         (frames : Frame[])  = 

        let vertexCount = frames.[0].positions.Length       
        let dci = DrawCallInfo(vertexCount, InstanceCount = 1)

        let viewTrafo = cameraView |> AVal.map (fun cV -> cV.ViewTrafo.Forward)
        let pointRadius = AVal.constant 1.0

        //let filterNew = filter |> AVal.map (fun f -> Box3f f)
        let filterNew = filterBox |> AVal.map (fun f -> match f with 
                                                        | Some i -> i
                                                        | None -> Box3f.Infinite)

        let currentBuffers = frame |> AVal.map (fun i -> frames.[i].preparedFrame)
        let color = colorValue |> AVal.map (fun c -> C4d c) |> AVal.map (fun x -> V4d(x.R, x.G, x.B, x.A))
        let texture = tfPath |> AVal.map (fun p -> FileTexture(p, TextureParams.empty) :> ITexture )

        let positions   = currentBuffers |> AVal.map (fun f -> f.positionsBuffer)
        let normals     = currentBuffers |> AVal.map (fun f -> f.normalsBuffer)
        let velocities  = currentBuffers |> AVal.map (fun f -> f.velocitiesBuffer)
        let masses      = currentBuffers |> AVal.map (fun f -> f.massesBuffer)
        let densities   = currentBuffers |> AVal.map (fun f -> f.densitiesBuffer)
        let energies    = currentBuffers |> AVal.map (fun f -> f.energiesBuffer)
        let cubicRoots  = currentBuffers |> AVal.map (fun f -> f.cubicRootsBuffer)
        let strains     = currentBuffers |> AVal.map (fun f -> f.strainsBuffer)
        let alphaJutzis = currentBuffers |> AVal.map (fun f -> f.alphaJutzisBuffer)
        let pressures   = currentBuffers |> AVal.map (fun f -> f.pressuresBuffer)

        //Sg.draw IndexedGeometryMode.PointList
        //|> Sg.vertexAttribute DefaultSemantic.Positions (currFrame |> AVal.map (fun f -> f.positions))
        //|> Sg.vertexAttribute DefaultSemantic.Normals (currFrame |> AVal.map (fun f -> f.normals))
        //|> Sg.vertexAttribute (Sym.ofString "Velocity") (currFrame |> AVal.map (fun f -> f.velocities))
        //|> Sg.vertexAttribute (Sym.ofString "Energy") (currFrame |> AVal.map (fun f -> Array.map float32 f.energies))
        //|> Sg.vertexAttribute (Sym.ofString "CubicRootOfDamage") (currFrame |> AVal.map (fun f -> Array.map float32 f.cubicRoots))
        //|> Sg.vertexAttribute (Sym.ofString "LocalStrain") (currFrame |> AVal.map (fun f -> Array.map float32 f.strains))
        //|> Sg.vertexAttribute (Sym.ofString "AlphaJutzi") (currFrame |> AVal.map (fun f -> Array.map float32 f.alphaJutzis))
        //|> Sg.vertexAttribute (Sym.ofString "Pressure") (currFrame |> AVal.map (fun f -> Array.map float32 f.pressures))


        // creating an index does not harm - any ways.
        let index = createIndex vertexCount (frame |> AVal.map (fun i -> frames.[i].positions)) (cameraView |> AVal.map CameraView.location) 
        let transparencyEnabled = AVal.constant false


        Sg.render IndexedGeometryMode.PointList dci 
        |> Sg.index index
        |> Sg.vertexBuffer DefaultSemantic.Positions (BufferView(positions, typeof<V4f>))
        |> Sg.vertexBuffer DefaultSemantic.Normals (BufferView(normals, typeof<V3f>))
        |> Sg.vertexBuffer (Sym.ofString "Velocity") (BufferView(velocities, typeof<V3f>))
        |> Sg.vertexBuffer (Sym.ofString "Mass") (BufferView(masses, typeof<float32>))
        |> Sg.vertexBuffer (Sym.ofString "Density") (BufferView(densities, typeof<float32>))
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
        |> Sg.uniform "DiscardPoints" discardPoints
        |> Sg.uniform "NormalizeData" normalizeData
        |> Sg.uniform "EnableShading" enableShading
        |> Sg.uniform "ReconstructNormal" reconstructNormal
        |> Sg.uniform "ReconstructDepth" reconstructDepth
        |> Sg.uniform "OutliersRange" outliersRange
        |> Sg.uniform "PointRadius" pointRadius
        |> Sg.uniform "ViewMatrix" viewTrafo
        |> Sg.uniform "ViewVector" viewVector
        |> Sg.uniform "TransferFunction" texture
        |> Sg.uniform "RenderValue" renderValue
        |> Sg.uniform "DomainRange" domainRange
        |> Sg.uniform "ClippingPlane" clippingPlane
        |> Sg.uniform "Filter" filterNew 
        |> Sg.uniform "CurrFilters" currFilters
        |> Sg.uniform "DataRange" dataRange
        |> Sg.uniform "Color" color
        |> Sg.uniform "Alpha" ~~0.01
        |> transparencyModes transparencyEnabled // apply transparency dependent attributes
  
    //TODO: Create a function containing repetitive code
    let createAnimatedVrSg (frame : aval<int>) (pointSize : aval<float>) (discardPoints : aval<bool>) 
                           (normalizeData : aval<bool>) (enableShading : aval<bool>) 
                           (reconstructNormal : aval<bool>) (reconstructDepth : aval<bool>)
                           (lowerOutliers : aval<bool>) (higherOutliers : aval<bool>) (outliersRange : aval<Range>)
                           (pointRadius : aval<float>)
                           (renderValue : aval<RenderValue>) (tfPath : aval<string>) 
                           (domainRange : aval<DomainRange>) (clippingPlane : aval<ClippingPlane>) 
                           (contrClippingPlane : aval<Plane3d>)
                           (filterBox : aval<option<Box3f>>) (sphereProbe : aval<Sphere3d>) (allSpheres : aval<V4f[]>) (spheresLength : aval<int>)
                           (currFilters : aval<Filters>) 
                           (dataRange : aval<Range>) (colorValue : aval<C4b>) 
                           (cameraView : aval<CameraView>) (viewTrafoVR : aval<Trafo3d>) (viewVector : aval<V3d>)
                           (runtime : IRuntime)
                           (frames : Frame[])  = 

        let vertexCount = frames.[0].positions.Length       
        let dci = DrawCallInfo(vertexCount, InstanceCount = 1)

        let filterBoxNew = filterBox |> AVal.map (fun f -> match f with
                                                            | Some b -> b
                                                            | None -> Box3f.Infinite)

        let boxIsSet = filterBox |> AVal.map (fun f -> match f with 
                                                        | Some i -> true
                                                        | None -> false)

        //let viewTrafo = viewTrafoVR |> AVal.map (fun vT -> vT.Forward)                                                        
                                                            
       // let currFrame = frame |> AVal.map (fun i -> frames.[i])
        let currentBuffers = frame |> AVal.map (fun i -> frames.[i].preparedFrame)
        let color = colorValue |> AVal.map (fun c -> C4d c) |> AVal.map (fun x -> V4d(x.R, x.G, x.B, x.A))
        let texture = tfPath |> AVal.map (fun p -> FileTexture(p, TextureParams.empty) :> ITexture )

        let positions   = currentBuffers |> AVal.map (fun f -> f.positionsBuffer)
        let normals     = currentBuffers |> AVal.map (fun f -> f.normalsBuffer)
        let velocities  = currentBuffers |> AVal.map (fun f -> f.velocitiesBuffer)
        let masses      = currentBuffers |> AVal.map (fun f -> f.massesBuffer)
        let densities   = currentBuffers |> AVal.map (fun f -> f.densitiesBuffer)
        let energies    = currentBuffers |> AVal.map (fun f -> f.energiesBuffer)
        let cubicRoots  = currentBuffers |> AVal.map (fun f -> f.cubicRootsBuffer)
        let strains     = currentBuffers |> AVal.map (fun f -> f.strainsBuffer)
        let alphaJutzis = currentBuffers |> AVal.map (fun f -> f.alphaJutzisBuffer)
        let pressures   = currentBuffers |> AVal.map (fun f -> f.pressuresBuffer)

        //Sg.draw IndexedGeometryMode.PointList
        //|> Sg.vertexAttribute DefaultSemantic.Positions (currFrame |> AVal.map (fun f -> f.positions))
        //|> Sg.vertexAttribute DefaultSemantic.Normals (currFrame |> AVal.map (fun f -> f.normals))
        //|> Sg.vertexAttribute (Sym.ofString "Velocity") (currFrame |> AVal.map (fun f -> f.velocities))
        //|> Sg.vertexAttribute (Sym.ofString "Energy") (currFrame |> AVal.map (fun f -> Array.map float32 f.energies))
        //|> Sg.vertexAttribute (Sym.ofString "CubicRootOfDamage") (currFrame |> AVal.map (fun f -> Array.map float32 f.cubicRoots))
        //|> Sg.vertexAttribute (Sym.ofString "LocalStrain") (currFrame |> AVal.map (fun f -> Array.map float32 f.strains))
        //|> Sg.vertexAttribute (Sym.ofString "AlphaJutzi") (currFrame |> AVal.map (fun f -> Array.map float32 f.alphaJutzis))
        //|> Sg.vertexAttribute (Sym.ofString "Pressure") (currFrame |> AVal.map (fun f -> Array.map float32 f.pressures))

        // creating an index does not harm - any ways.
        let index = createIndex vertexCount (frame |> AVal.map (fun i -> frames.[i].positions)) (cameraView |> AVal.map CameraView.location) 
        let transparencyEnabled = AVal.constant false

        Sg.render IndexedGeometryMode.PointList dci 
        |> Sg.index index
        |> Sg.vertexBuffer DefaultSemantic.Positions (BufferView(positions, typeof<V4f>))
        |> Sg.vertexBuffer DefaultSemantic.Normals (BufferView(normals, typeof<V3f>))
        |> Sg.vertexBuffer (Sym.ofString "Velocity") (BufferView(velocities, typeof<V3f>))
        |> Sg.vertexBuffer (Sym.ofString "Mass") (BufferView(masses, typeof<float32>))
        |> Sg.vertexBuffer (Sym.ofString "Density") (BufferView(densities, typeof<float32>))
        |> Sg.vertexBuffer (Sym.ofString "Energy") (BufferView(energies, typeof<float32>))
        |> Sg.vertexBuffer (Sym.ofString "CubicRootOfDamage") (BufferView(cubicRoots, typeof<float32>))
        |> Sg.vertexBuffer (Sym.ofString "LocalStrain") (BufferView(strains, typeof<float32>))
        |> Sg.vertexBuffer (Sym.ofString "AlphaJutzi") (BufferView(alphaJutzis, typeof<float32>))
        |> Sg.vertexBuffer (Sym.ofString "Pressure") (BufferView(pressures, typeof<float32>))
        |> Sg.shader {  
            do! DefaultSurfaces.trafo
            do! Shaders.pointSpriteVr
            // do! Shaders.vs
            do! Shaders.fs
        }
        |> Sg.uniform "PointSize" pointSize
        |> Sg.uniform "DiscardPoints" discardPoints
        |> Sg.uniform "NormalizeData" normalizeData
        |> Sg.uniform "EnableShading" enableShading
        |> Sg.uniform "ReconstructNormal" reconstructNormal
        |> Sg.uniform "ReconstructDepth" reconstructDepth
        |> Sg.uniform "OutliersRange" outliersRange
        |> Sg.uniform "PointRadius" pointRadius
        |> Sg.uniform "ViewMatrix" viewTrafoVR
        |> Sg.uniform "ViewVector" viewVector
        |> Sg.uniform "TransferFunction" texture
        |> Sg.uniform "RenderValue" renderValue
        |> Sg.uniform "DomainRange" domainRange
        |> Sg.uniform "ClippingPlane" clippingPlane
        |> Sg.uniform "ControllerClippingPlane" contrClippingPlane
        |> Sg.uniform "Filter" filterBoxNew 
        |> Sg.uniform "BoxSet" boxIsSet
        |> Sg.uniform "CurrFilters" currFilters
        |> Sg.uniform "DataRange" dataRange
        |> Sg.uniform "Color" color
        |> Sg.uniform "SphereProbe" sphereProbe  
        |> Sg.uniform "SpheresInfos" allSpheres
        |> Sg.uniform "SpheresLength" spheresLength
        |> transparencyModes transparencyEnabled // apply transparency dependent attributes
  