namespace HeraSg

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

            [<Normal>]
            normal : V3d

            [<Depth(DepthWriteMode.OnlyLess)>]
            depth : float

            [<PointSize>] // gl_PointSize
            pointSize : float

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
                                    pointSize = uniform?PointSize}
                else
                    yield { p.Value with 
                                pointColor = color
                                pointSize = uniform?PointSize}
        }
      
    let internal pointSpriteVr (p : Point<Vertex>) = 
        point {
            let wp = p.Value.wp

            let modelMatrixInv = uniform.ModelTrafoInv

            let wpInv = modelMatrixInv * wp

            let modelMatrix = uniform.ModelTrafo

            let dm : DomainRange = uniform?DomainRange
            let dataRange : Range = uniform?DataRange
            let filterBox : Box3f = uniform?Filter
            let boxIsSet : bool = uniform?BoxSet
            let sphereProbe : Sphere3d = uniform?SphereProbe
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
            
            let transferFunc = transfer.SampleLevel(V2d(linearCoord, 0.0), 0.0)

            let notDiscardByFilters = 
                    (p.Value.energy >= filters.filterEnergy.min && p.Value.energy <= filters.filterEnergy.max) &&
                    (p.Value.cubicRoot >= filters.filterCubicRoot.min && p.Value.cubicRoot <= filters.filterCubicRoot.max) &&
                    (p.Value.localStrain >= filters.filterStrain.min && p.Value.localStrain <= filters.filterStrain.max) &&
                    (p.Value.alphaJutzi >= filters.filterAlphaJutzi.min && p.Value.alphaJutzi <= filters.filterAlphaJutzi.max) &&
                    (p.Value.pressure >= filters.filterPressure.min && p.Value.pressure <= filters.filterPressure.max) 

            //let min = if boxIsSet then V3f(modelMatrix.TransformPos(V3d(filterBox.Min))) else filterBox.Min
            //let max = if boxIsSet then V3f(modelMatrix.TransformPos(V3d(filterBox.Max))) else filterBox.Max
            //let min = if temp then filterBox.Min else V3f(modelMatrix.TransformPos(V3d(filterBox.Min)))
            let min = filterBox.Min
            let max = filterBox.Max

            let pos = V3f(wpInv)
            let pPos = V3d(wp)

            let spPos = sphereProbe.Center
            let radius = sphereProbe.Radius

            let isNotInsideSphere = 
                if radius < 0.0 then 
                    true
                else 
                    (pPos.X - spPos.X) ** 2.0 + (pPos.Y - spPos.Y) ** 2.0 + (pPos.Z - spPos.Z) ** 2.0 <= radius ** 2.0

            let minRange = dataRange.min
            let maxRange = dataRange.max
            let isInsideMinMaxRange = min.X <= pos.X && pos.X <= max.X && min.Y <= pos.Y && pos.Y <= max.Y && min.Z <= pos.Z && pos.Z <= max.Z

            let minOutlier = outliersRange.min
            let maxOutlier = outliersRange.max
            let isInsideOutlierRange = minOutlier <= currValue && currValue <= maxOutlier

            let plane = uniform?ClippingPlane

            let pSize = uniform?PointSize

            let controllerPlane : Plane3d = uniform?ControllerClippingPlane

            let isOutsideControllerPlane = 
                if controllerPlane.Normal = V3d.Zero then
                    true
                else 
                    Vec.Dot(controllerPlane.Normal, pPos) - controllerPlane.Distance >= 0.0

            let isInAllRanges = notDiscardByFilters && isInsideMinMaxRange && isNotInsideSphere && isInsideOutlierRange //&& minRange <= linearCoord && linearCoord <= maxRange

            let color = if (isInAllRanges) then transferFunc else colorValue
            let size = if (isNotInsideSphere) then (pSize + 8.0) else pSize

            if (wpInv.X >= dm.x.min && wpInv.X <= dm.x.max && wpInv.Y >= dm.y.min && wpInv.Y <= dm.y.max && wpInv.Z >= dm.z.min && wpInv.Z <= dm.z.max) &&
                (wpInv.X <= plane.x && wpInv.Y <= plane.y && wpInv.Z <= plane.z) && isOutsideControllerPlane then
                if (discardPoints) then
                    if (isInAllRanges) then
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

            let ambient = V4d(0.4, 0.375, 0.35, 1.0)
            let diffuse = 0.6
            let specular = 0.3

            let modelMatrix = uniform.ModelTrafo
            let viewMatrix : M44d = uniform?ViewTrafo
            let projectionMatrix = uniform.ProjTrafo
            let modelMatrixInv = uniform.ModelTrafoInv
    
            let shading = uniform?EnableShading
            let reconstructNormal = uniform?ReconstructNormal
            let reconstructDepth : bool = uniform?ReconstructDepth
            let pointRadius : float = uniform?PointRadius

            //Normal reconstruction
            let distToCenter_squared = Vec.dot c c 
            let normalVec = V3d(c, Math.Sqrt(1.0 - distToCenter_squared))
            let normalVec_normalized = normalVec |> Vec.normalize // IN VIEW SPACE!!!!!!!!

            let norm = if reconstructNormal then normalVec_normalized else v.normal

            //BLINN-PHONG LIGHTING MODEL FROM MY LECTURE
            let lp = V3d(0.0, 40.0, 0.0)
            let spherePos_view = viewMatrix * v.wp
            let posOnSphere = spherePos_view + V4d(norm, 1.0) * pointRadius // TODO: not sure if the radius is always one

            //Depth reconstruction
            let posOnSphere_screen = projectionMatrix * posOnSphere
            let ndc_depth = posOnSphere_screen.Z / posOnSphere_screen.W
            let dep = if reconstructDepth then ((ndc_depth + 1.0) / 2.0) else v.fc.Z

            let lightPos_view = viewMatrix * V4d(lp, 1.0)
            let lightPos = if reconstructNormal then lightPos_view else V4d(lp, 1.0)
            let posSphere = if reconstructNormal then posOnSphere else v.wp
            let lightDir = Vec.normalize(lightPos.XYZ - posSphere.XYZ)
            let viewDir = Vec.normalize(-posSphere.XYZ)
            
            //Diffuse term
            let diff =
                let value = Vec.dot norm lightDir
                if reconstructNormal then (max 0.0 value) else (abs value)

            //Speculat term 
            let halfwayDir = Vec.normalize(lightDir + viewDir)
            let spec = Math.Pow(Math.Max(Vec.Dot(norm, halfwayDir), 0.0), 128.0)

            // Final Color
            let color = V4d(v.pointColor.XYZ, 1.0)
            let fragmentColor = (ambient * color) + (diffuse * diff * color) //+ (specular * spec * color)
            let c = if shading then fragmentColor else color

            let alpha : float = uniform?Alpha
            //return V4d((v.velocity * 0.5 + V3d.Half).XYZ,1.0) // color according to velocity
            //return V4d(V3d(v.cubicRoots), 1.0) // color according to cubic Roots
            //let color = V4d(v.pointColor.XYZ, 1.0)
            //return V4d(v.pointColor.XYZ * l, 1.0)

            return {v with 
                        pointColor = c
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

    let createAnimatedSg (frame : aval<int>) (pointSize : aval<float>) (discardPoints : aval<bool>) 
                         (normalizeData : aval<bool>) (enableShading : aval<bool>)
                         (reconstructNormal : aval<bool>) (reconstructDepth : aval<bool>)
                         (lowerOutliers : aval<bool>) (higherOutliers : aval<bool>) (outliersRange : aval<Range>)
                         (renderValue : aval<RenderValue>) (tfPath : aval<string>) 
                         (domainRange : aval<DomainRange>) (clippingPlane : aval<ClippingPlane>) 
                         (filterBox : aval<option<Box3f>>)
                         (currFilters : aval<Filters>) 
                         (dataRange : aval<Range>) (colorValue : aval<C4b>) 
                         (cameraView : aval<CameraView>)
                         (runtime : IRuntime)
                         (frames : Frame[])  = 

        let vertexCount = frames.[0].positions.Length       
        let dci = DrawCallInfo(vertexCount, InstanceCount = 1)

        let mutable mode = BlendMode(true)
        mode.Enabled <- true
        mode.Operation <- BlendOperation.Add
        mode.AlphaOperation <- BlendOperation.Add
        mode.SourceFactor <- BlendFactor.SourceAlpha
        mode.DestinationFactor <- BlendFactor.InvSourceAlpha
        mode.SourceAlphaFactor <- BlendFactor.One
        mode.DestinationAlphaFactor <- BlendFactor.InvSourceAlpha

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


        Sg.render IndexedGeometryMode.PointList dci 
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
        |> Sg.uniform "ViewMatrix" viewTrafo
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
  
    //TODO: Create a function containing repetitive code
    let createAnimatedVrSg (frame : aval<int>) (pointSize : aval<float>) (discardPoints : aval<bool>) 
                           (normalizeData : aval<bool>) (enableShading : aval<bool>) 
                           (reconstructNormal : aval<bool>) (reconstructDepth : aval<bool>)
                           (lowerOutliers : aval<bool>) (higherOutliers : aval<bool>) (outliersRange : aval<Range>)
                           (pointRadius : aval<float>)
                           (renderValue : aval<RenderValue>) (tfPath : aval<string>) 
                           (domainRange : aval<DomainRange>) (clippingPlane : aval<ClippingPlane>) 
                           (contrClippingPlane : aval<Plane3d>)
                           (filterBox : aval<option<Box3f>>) (sphereProbe : aval<Sphere3d>)
                           (currFilters : aval<Filters>) 
                           (dataRange : aval<Range>) (colorValue : aval<C4b>) 
                           (cameraView : aval<CameraView>) (viewTrafoVR : aval<Trafo3d>)
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

        let viewTrafo = viewTrafoVR |> AVal.map (fun vT -> vT.Forward)                                                        
                                                            
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

        Sg.render IndexedGeometryMode.PointList dci 
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
        |> Sg.uniform "ViewMatrix" viewTrafo
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