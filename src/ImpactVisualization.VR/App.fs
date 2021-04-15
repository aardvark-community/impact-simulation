namespace ImpactVisualization

open System
open System.IO
open System.IO.Compression
open System.IO.MemoryMappedFiles
open Aardvark.Base
open Aardvark.Rendering.Text
open Aardvark.Vr
open Aardvark.SceneGraph
open Aardvark.SceneGraph.IO
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.UI.Generic
open Aardvark.Application
open Aardvark.Application.OpenVR
open Adaptify.FSharp.Core

open FSharp.Data.Adaptive

open AardVolume.Model
open AardVolume.App
open ImpactVisualization
open ImpactVisualization.AppUpdate
open Aardvark.Cef
open Touchpad
 
module Demo =
    open Aardvark.UI.Primitives
    open Aardvark.Base.Rendering

    let combinedTrafo (trafos : aval<Trafo3d []>) = 
        trafos |> AVal.map (fun trafos ->
        let left = trafos.[0]
        let right = trafos.[1]
        let combined = (left.Forward + right.Forward)/2.0
        let combinedInv = (left.Backward + right.Backward)/2.0
        Trafo3d(combined, combinedInv))

    let threads (model : Model) =
        let thread2d = AardVolume.App.threads model.twoDModel |> ThreadPool.map TwoD
        let threadsVr = model.threads
        ThreadPool.union thread2d threadsVr
        
    let input (msg : VrMessage) =
        match msg with
        | VrMessage.PressButton(controllerId, buttonId) ->
           // printf "press button: %A " (controllerId, buttonId)
            match buttonId with 
            | 1 -> [ResetHera]
            | 2 -> [GrabHera controllerId]
            | _ -> []
        | VrMessage.UnpressButton(controllerId, buttonId) ->
            match buttonId with 
            | 2 -> [UngrabHera controllerId]
            | _ -> []
        | VrMessage.Press(controllerId, buttonId) ->
            match buttonId with 
            | 0 -> [ToggleMainMenu controllerId; OpenMainMenu controllerId; OpenProbeAttributeMenu controllerId; ]
            | 1 -> [ActivateControllerMode controllerId; ScaleProbe controllerId; DeleteProbe controllerId]
            | _ -> []
        | VrMessage.Unpress(controllerId, buttonId) ->
            match buttonId with 
            | 1 ->  [DeactivateControllerMode controllerId; StopProbeScale controllerId]
            | _ -> []
        | VrMessage.Touch(controllerId, buttonId) ->
            //match buttonId with 
            //| 0 -> [TouchDevice controllerId]
            //| _ -> []
            []
        | VrMessage.Untouch(controllerId, buttonId) ->
            //match buttonId with 
            //| 0 -> [UntouchDevice controllerId]
            //| _ -> []
            []
        | VrMessage.ValueChange(controllerId, buttonId, value) ->
            match buttonId with 
            | 0 ->  [ChangeTouchpadPos (controllerId, value); 
                     ScaleHera (controllerId, value.X); 
                     ChangeMainControllerMode controllerId; 
                     SelectGlobalAttribute controllerId;
                     SelectProbeAttribute controllerId;
                     ChangeBillboard controllerId
                     ]
            | _ -> []
        | VrMessage.UpdatePose(controllerId, pose) ->
            if pose.isValid then [MoveController (controllerId, pose.deviceToWorld); SetController controllerId] else []
        | _ -> []

    let ui (runtime : IRuntime) (data : Frame[]) (info : VrSystemInfo) (m : AdaptiveModel) : DomNode<Message> = // 2D UI
        div [] [AardVolume.App.view runtime data m.twoDModel |> UI.map TwoD]

    let vr (runtime : IRuntime) (client : Browser) (histogramClient : Browser) (viewTrafo : aval<Trafo3d>) (data : Frame[]) (info : VrSystemInfo) (m : AdaptiveModel) : ISg<Message> = // HMD Graphics
       
        let pass0 = RenderPass.main
        let pass1 = RenderPass.after "pass1" RenderPassOrder.Arbitrary pass0 
        let pass2 = RenderPass.after "pass2" RenderPassOrder.Arbitrary pass1   
        let pass3 = RenderPass.after "pass3" RenderPassOrder.Arbitrary pass2   
        let pass4 = RenderPass.after "pass4" RenderPassOrder.Arbitrary pass3   


        let mutable mode = BlendMode(true)
        mode.Enabled <- true
        mode.Operation <- BlendOperation.Add
        mode.AlphaOperation <- BlendOperation.Add
        mode.SourceFactor <- BlendFactor.SourceAlpha
        mode.DestinationFactor <- BlendFactor.InvSourceAlpha
        mode.SourceAlphaFactor <- BlendFactor.One
        mode.DestinationAlphaFactor <- BlendFactor.InvSourceAlpha

        let textScreenSg = 
            Loader.Assimp.load (Path.combine [__SOURCE_DIRECTORY__; "..";"..";"models";"controllerText";"text.obj"])
            |> Sg.adapter
            |> Sg.transform (Trafo3d.Scale(1.0, 1.0, -1.0))
            |> Sg.transform (Trafo3d.RotationXInDegrees(90.0))
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.diffuseTexture
                do! DefaultSurfaces.normalMap
                do! DefaultSurfaces.simpleLighting
            }
            |> Sg.pass pass0

        let pPositions =  AVal.constant  [|V3f(-1.588, -0.5, 0.31); V3f(1.588, -0.5, 0.31); V3f(1.588, 0.5, 0.31); V3f(-1.588, 0.5, 0.31)|]

        let textPlaneSg = 
            Sg.draw IndexedGeometryMode.TriangleList
            |> Sg.vertexAttribute DefaultSemantic.Positions pPositions
            |> Sg.vertexAttribute DefaultSemantic.Normals (AVal.constant [| V3f.OOI; V3f.OOI; V3f.OOI; V3f.OOI |])
            |> Sg.vertexAttribute DefaultSemantic.DiffuseColorCoordinates  (AVal.constant  [| V2f.OO; V2f.IO; V2f.II; V2f.OI |])
            |> Sg.index (AVal.constant [|0;1;2; 0;2;3|])
            |> Sg.translate 0.0 -0.0006 0.0
            |> Sg.scale 0.022
            |> Sg.diffuseTexture m.contrScreenTexture
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.diffuseTexture
            }

        let texturePositions =  AVal.constant  [|V3f(-1.0, -1.0, 0.0); V3f(1.0, -1.0, 0.0); V3f(1.0, 1.0, 0.0); V3f(-1.0, 1.0, 0.0)|]

        let touchpadPlaneSg showTexture texture =  
            Sg.draw IndexedGeometryMode.TriangleList
            |> Sg.vertexAttribute DefaultSemantic.Positions texturePositions
            |> Sg.vertexAttribute DefaultSemantic.Normals (AVal.constant [| V3f.OOI; V3f.OOI; V3f.OOI; V3f.OOI |])
            |> Sg.vertexAttribute DefaultSemantic.DiffuseColorCoordinates  (AVal.constant  [| V2f.OO; V2f.IO; V2f.II; V2f.OI |])
            |> Sg.index (AVal.constant [|0;1;2; 0;2;3|])
            |> Sg.scale 0.0205
            |> Sg.transform (Trafo3d.RotationXInDegrees(6.5))
            |> Sg.translate 0.0 -0.05 0.0052
            |> Sg.onOff showTexture
            |> Sg.diffuseTexture texture
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.diffuseTexture
                do! TouchpadShaders.fragmentSh
            }

        let mainTouchpadPlaneSg = touchpadPlaneSg m.showMainTexture m.mainTouchpadTexture

        let secondTouchpadPlaneSg = touchpadPlaneSg m.showSecondTexture m.secondTouchpadTexture

        //let touchingMain = m.touchpadDeviceId |> AVal.map (fun id -> id.IsSome)

        let touchpadPosMain = m.currMainTouchPadPos |> AVal.map (fun pos -> let z = tan (6.5 * (Math.PI / 180.0))
                                                                            V3d(pos, z * (pos.Y + 1.0)) * 0.019)

        let touchpadPosSecond = m.currSecondTouchPadPos |> AVal.map (fun pos -> let z = tan (6.5 * (Math.PI / 180.0))
                                                                                V3d(pos, z * (pos.Y + 1.0)) * 0.019)

        let touchpadSphereSg = 
            Sg.sphere' 9 C4b.LightGreen 0.002
            |> Sg.noEvents
            |> Sg.translate 0.0 -0.05 0.0035 // translation so that it is in the middle of the touchpad

        let mainTouchpadSphereSg = 
            touchpadSphereSg
            |> Sg.translate' touchpadPosMain
            |> Sg.trafo m.mainControllerTrafo
            |> Sg.onOff m.mainTouching

        let secondTouchpadSphereSg = 
            touchpadSphereSg
            |> Sg.translate' touchpadPosSecond
            |> Sg.trafo m.secondControllerTrafo
            |> Sg.onOff m.secondTouching

        //let textPlaneSg = 
        //    Loader.Assimp.load (Path.combine [__SOURCE_DIRECTORY__; "..";"..";"models";"controllerText";"plane.obj"])
        //    |> Sg.adapter
        //    |> Sg.vertexAttribute DefaultSemantic.Normals (AVal.constant [| V3f.OOI; V3f.OOI; V3f.OOI; V3f.OOI |])
        //    |> Sg.vertexAttribute DefaultSemantic.DiffuseColorCoordinates  (AVal.constant  [| V2f.OO; V2f.IO; V2f.II; V2f.OI |])
        //    |> Sg.index (AVal.constant [|0;1;2; 0;2;3|])
        //    |> Sg.transform (Trafo3d.Scale(1.0, 1.0, -1.0))
        //    |> Sg.transform (Trafo3d.RotationXInDegrees(90.0))
        //    |> Sg.diffuseTexture DefaultTextures.checkerboard
        //    |> Sg.shader {
        //        do! DefaultSurfaces.trafo
        //        do! DefaultSurfaces.diffuseTexture
        //    }
        //    |> Sg.pass pass0

        let deviceSgs = 
            info.state.devices |> AMap.toASet |> ASet.chooseA (fun (_,d) ->
                //printf "Device Type: %A, %A \n" d.kind d.id
                //printf "Device Vibrate: %A \n" d.startVibrate  
                let touchpadSg  = 
                    (d.id, m.mainControllerId, m.secondControllerId) |||> AVal.map3 (fun id mId sId ->
                        match mId with
                        | Some i when i = id -> mainTouchpadPlaneSg
                        | _ ->
                            match sId with
                            | Some i when i = id -> secondTouchpadPlaneSg
                            | _ -> Sg.empty)
                    |> Sg.dynamic
                d.model |> AVal.map (fun m ->
                    match m.Value with
                    | Some sg -> 
                        sg 
                        |> Sg.noEvents 
                        |> Sg.andAlso textScreenSg
                        |> Sg.andAlso textPlaneSg
                        |> Sg.andAlso touchpadSg
                        |> Sg.trafo d.pose.deviceToWorld
                        |> Sg.onOff d.pose.isValid
                        |> Some
                    | None -> 
                        None 
                )
            )
            |> Sg.set
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.diffuseTexture
                do! DefaultSurfaces.simpleLighting
            }
            |> Sg.pass pass0

        let sphereTrafo = 
            AVal.map2 (fun sphereContrTrafo (sphereScale : float) -> 
                Trafo3d(Scale3d(sphereScale)) * sphereContrTrafo
                ) m.mainControllerTrafo m.sphereScale

        let currentSphereProbeSg = 
            Sg.sphere 6 m.sphereColor m.sphereRadius
            |> Sg.noEvents
            |> Sg.trafo sphereTrafo
            |> Sg.onOff m.currentProbeManipulated
            |> Sg.fillMode (FillMode.Line |> AVal.constant)
           // |> Sg.cullMode (CullMode.Front |> AVal.constant)
           // |> Sg.blendMode (AVal.constant mode)
            |> Sg.pass pass4

        let cfg : Aardvark.Rendering.Text.TextConfig = 
            {
                font              = Font "Calibri"
                color             = C4b.White
                align             = TextAlignment.Center
                flipViewDependent = true
                renderStyle       = RenderStyle.Normal
        }
        
        let probeHistogramPositions = AVal.constant  [|V3f(-1.77, -1.0, 0.0); V3f(1.77, -1.0, 0.0); V3f(1.77, 1.0, 0.0); V3f(-1.77, 1.0, 0.0)|]

        let createStatisticsSg (p : AdaptiveProbe) = 
            let text = 
                 let allData = p.allData |> AMap.toAVal
                 (allData, p.currAttribute) ||> AVal.map2 (fun data attrib -> 
                    let array, stats = data.Item attrib
                    stats)
            let statisticsScaleTrafo = 
                (m.sphereRadius, p.radius) ||> AVal.map2 (fun r radius -> 
                    let sphereScale = radius / r
                    let statScale = 0.04 * sphereScale
                    Trafo3d(Scale3d(statScale)))
            let showStatistics = 
                (p.showStatistics, p.currBillboard) ||> AVal.map2 (fun showStats billboardType ->
                    let showBillboard = 
                        match billboardType with 
                        | BillboardType.Statistic -> true
                        | _ -> false
                    showStats && showBillboard)
            text
            |> AVal.map cfg.Layout
            |> Sg.shapeWithBackground C4b.Gray10 Border2d.None
            |> Sg.noEvents
            |> Sg.trafo statisticsScaleTrafo
            |> Sg.trafo (p.radius |> AVal.map (fun r -> Trafo3d.Translation(0.0, (r * 0.75), 0.0)))
            |> Sg.transform (Trafo3d.FromOrthoNormalBasis(V3d.IOO,-V3d.OIO, V3d.OOI))
            |> Sg.myBillboard viewTrafo
            |> Sg.applyRuntime runtime
            |> Sg.noEvents
            |> Sg.trafo (p.center |> AVal.map (fun center -> Trafo3d.Translation(center)))
            |> Sg.trafo (p.radius |> AVal.map (fun r -> Trafo3d.Translation(0.0, 0.0, (r * 1.8))))
            |> Sg.onOff showStatistics
            |> Sg.blendMode (AVal.constant mode)
            |> Sg.pass pass3

        let createHistogramSg (p : AdaptiveProbe) = 
            let histogramScaleTrafo = 
                (m.sphereRadius, p.radius) ||> AVal.map2 (fun r radius -> 
                    let sphereScale = radius / r
                    let statScale = 0.12 * sphereScale
                    Trafo3d(Scale3d(statScale)))
            let showCurrHistogram = 
                (p.showHistogram, p.currHistogram, p.currBillboard) |||> AVal.map3 (fun showHisto currHisto billboardType -> 
                    let showBillboard = 
                        match billboardType with 
                        | BillboardType.Histogram -> true
                        | _ -> false
                    showHisto && currHisto.IsSome && showBillboard)
            let texture = 
                p.currHistogram 
                |> AVal.map (fun histo ->
                    let pixImage = 
                        match histo with 
                        | Some tex -> tex
                        | None -> DefaultTextures.blackPix :> PixImage
                    convertPixImageToITexture pixImage)
            Sg.draw IndexedGeometryMode.TriangleList
            |> Sg.vertexAttribute DefaultSemantic.Positions probeHistogramPositions
            |> Sg.vertexAttribute DefaultSemantic.Normals (AVal.constant [| V3f.OOI; V3f.OOI; V3f.OOI; V3f.OOI |])
            |> Sg.vertexAttribute DefaultSemantic.DiffuseColorCoordinates  (AVal.constant  [| V2f.OO; V2f.IO; V2f.II; V2f.OI |])
            |> Sg.index (AVal.constant [|0;1;2; 0;2;3|]) 
            |> Sg.trafo histogramScaleTrafo
            |> Sg.transform (Trafo3d.FromOrthoNormalBasis(V3d.IOO,-V3d.OIO, V3d.OOI))
            |> Sg.myBillboard viewTrafo
            |> Sg.applyRuntime runtime
            |> Sg.noEvents
            |> Sg.trafo (p.center |> AVal.map (fun center -> Trafo3d.Translation(center)))
            |> Sg.trafo (p.radius |> AVal.map (fun r -> Trafo3d.Translation(0.0, 0.0, (r * 1.6))))
            |> Sg.onOff showCurrHistogram
            |> Sg.diffuseTexture texture
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.simpleLighting
                do! DefaultSurfaces.diffuseTexture
            }
            |> Sg.blendMode (AVal.constant mode)
            |> Sg.pass pass2

        let showBillboard = m.grabbingHera |> AVal.map (fun grabbing -> not grabbing)

        //TODO: Probably causing overhead due to the complexity, especially when grabbing hera
        let probesSgs = 
            m.allProbes |> AMap.toASet |> ASet.choose (fun (key, probe) ->
                let color =
                    AVal.map3 (fun mainId secondId probeIsInsideHera ->
                        match secondId with 
                        | Some i when i = key -> C4b.Red
                        | _ ->
                            match mainId with 
                            | Some i when i = key -> C4b.LightGreen
                            | _ -> if probeIsInsideHera then C4b.Blue else C4b.White
                    ) m.mainContrProbeIntersectionId m.secondContrProbeIntersectionId probe.insideHera
                let statisticsSg = createStatisticsSg probe
                let probeHistogramSg = createHistogramSg probe
                let billboardSg = 
                    statisticsSg
                    |> Sg.andAlso probeHistogramSg
                    |> Sg.onOff showBillboard
                Sg.sphere 6 color probe.radiusRelToHera
                |> Sg.noEvents
                |> Sg.trafo (probe.centerRelToHera |> AVal.map (fun center -> Trafo3d.Translation(center)))
                |> Sg.trafo m.heraTransformations // so that it moves with hera!!!
                |> Sg.fillMode (FillMode.Line |> AVal.constant)
                |> Sg.andAlso billboardSg
                |> Some
            ) 
            |> Sg.set
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.simpleLighting
            }
            //|> Sg.cullMode (CullMode.Front |> AVal.constant)
            //|> Sg.depthTest (AVal.constant DepthTestMode.Always)
            //|> Sg.blendMode (AVal.constant mode)
            |> Sg.pass pass2

        let contrClippingPlane = m.planeCorners |> AVal.map (fun c -> Plane3d(c.P0, c.P1, c.P2))

        let probeScale = 
            AVal.map3 (fun currProbMan currScale lastScale ->
                if currProbMan then currScale else lastScale
                ) m.currentProbeManipulated m.sphereScale m.lastSphereScale

        let sphereScaleAndRadius = 
            AVal.map2 (fun (scale : float) (radius : float) -> 
                    V2d(scale, radius)
                ) probeScale m.sphereRadius

        let sphereProbe = 
            AVal.map3 (fun (manipulated : bool) (trafo : Trafo3d) (scaleAndRadius : V2d) -> 
                let scale = scaleAndRadius.X
                let initRadius = scaleAndRadius.Y
                if (not manipulated) then 
                    Sphere3d.Invalid
                else 
                    let spherePos = trafo.Forward.TransformPos(V3d.OOO)
                    let sphereRadius : float = initRadius * scale 
                    Sphere3d(spherePos, sphereRadius)
                ) m.currentProbeManipulated m.mainControllerTrafo sphereScaleAndRadius

        let allPlacedSpheres = 
            m.allProbes
            |> AMap.mapA (fun key probe ->
                probe.Current |> AVal.map (fun p -> 
                    V4f(p.center, p.radius))                 
                )
            |> AMap.toASetValues
            |> ASet.toAList
            |> AList.toAVal
            |> AVal.map (fun value -> value.AsArray)

        let spheresLength =
            m.allProbes
            |> AMap.toAVal
            |> AVal.map (fun probes -> probes.Count)

        //let sphereProbe = Sphere3d.Invalid |> AVal.constant

        let heraSg = 
            let model = m
            let m = m.twoDModel
            data
            |> HeraSg.HeraSg.createAnimatedVrSg 
                m.frame m.pointSize m.discardPoints m.normalizeData 
                m.enableShading m.reconstructNormal m.reconstructDepth 
                m.lowerOutliers m.higherOutliers m.outliersRange
                model.scalingFactorHera
                m.renderValue m.currentMap m.domainRange m.clippingPlane contrClippingPlane 
                m.boxFilter sphereProbe allPlacedSpheres spheresLength
                m.currFilters m.dataRange m.colorValue.c 
                m.cameraState.view viewTrafo
                runtime
            |> Sg.noEvents
            |> Sg.trafo model.heraTransformations
            |> Sg.pass pass0

        let planeSg positions color fillmode blendmode renderPass =
            Sg.draw IndexedGeometryMode.TriangleList
            |> Sg.vertexAttribute DefaultSemantic.Positions positions
            |> Sg.vertexAttribute DefaultSemantic.Normals (AVal.constant [| V3f.OOI; V3f.OOI; V3f.OOI; V3f.OOI |])
            |> Sg.vertexAttribute DefaultSemantic.DiffuseColorCoordinates  (AVal.constant  [| V2f.OO; V2f.IO; V2f.II; V2f.OI |])
            |> Sg.index (AVal.constant [|0;1;2; 0;2;3|])
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.simpleLighting
                do! DefaultSurfaces.constantColor color
            }
            |> Sg.fillMode (fillmode |> AVal.constant)
            |> Sg.blendMode blendmode
            |> Sg.pass renderPass

        let planePositions = m.planeCorners |> AVal.map (fun q -> [|q.P0.ToV3f(); q.P1.ToV3f(); q.P2.ToV3f(); q.P3.ToV3f()|])

        let clipPlaneSg = planeSg planePositions (C4f(1.0,1.0,0.1,0.2)) FillMode.Fill (AVal.constant mode) pass4

        let tvSg = 
            Loader.Assimp.load (Path.combine [__SOURCE_DIRECTORY__; "..";"..";"models";"tv";"tv.obj"])
            |> Sg.adapter
            |> Sg.transform (Trafo3d.Scale(1.0, 1.0, -1.0))
            |> Sg.trafo (tvTrafo |> AVal.constant)
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.diffuseTexture
                do! DefaultSurfaces.normalMap
                do! DefaultSurfaces.simpleLighting
            }
            |> Sg.pass pass0
        
        let tvPosSphereSg = 
            Sg.sphere' 9 C4b.LightGreen 0.02
            |> Sg.noEvents
            |> Sg.translate' m.hitPoint
            |> Sg.onOff m.screenIntersection

        // TODO: X and Y must be swapped for some reason !! Find why??!!
        let message = 
            m.screenHitPoint |> AVal.map (fun p ->
                "X: " + p.Y.ToString() + "\n" +
                "Y: " + p.X.ToString() + "\n")

        let billboardSg = 
            Sg.markdown MarkdownConfig.light message
            |> Sg.billboard
            |> Sg.noEvents
            |> Sg.scale 0.1
            |> Sg.trafo (Trafo3d.RotationEulerInDegrees(90.0, 0.0, -90.0) |> AVal.constant)
            |> Sg.translate 2.5 2.0 0.5

        let contrOrientation = m.mainControllerTrafo |> AVal.map (fun t -> t.GetOrthoNormalOrientation()) 
        let controllerTypeMenuOpen = AVal.map2 (fun menuOpen level -> menuOpen && level = 1) m.mainMenuOpen m.menuLevel

        let controllerSg path rotX rotY rotZ scTrafo contrPos = 
            Loader.Assimp.load (Path.combine path)
            |> Sg.adapter
            |> Sg.transform (Trafo3d.Scale(1.0, 1.0, -1.0))
            |> Sg.transform (Trafo3d.RotationEulerInDegrees(rotX, rotY, rotZ))
            |> Sg.trafo scTrafo 
            |> Sg.trafo contrOrientation
            |> Sg.translate' contrPos
            |> Sg.onOff controllerTypeMenuOpen
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.diffuseTexture
                do! DefaultSurfaces.simpleLighting
            }
            |> Sg.pass pass0

        let controllerPos position = m.mainControllerTrafo |> AVal.map (fun t -> t.Forward.TransformPos(position)) 

        let probeContrPos = controllerPos (V3d(-0.12, 0.11, 0.0))
        let rayContrPos = controllerPos (V3d(0.0, 0.15, 0.0))
        let clippingContrPos = controllerPos (V3d(0.12, 0.11, 0.0))

        let scaleTrafo mode = 
            m.controllerMode |> AVal.map (fun m ->
                if m = mode then Trafo3d (Scale3d(0.7)) else Trafo3d (Scale3d(0.5)))

        let probeScaleTrafo = scaleTrafo ControllerMode.Probe
        let rayScaleTrafo = scaleTrafo ControllerMode.Ray
        let clippingScaleTrafo = scaleTrafo ControllerMode.Clipping

        let probeContrSg = 
            let path = [__SOURCE_DIRECTORY__; "..";"..";"models";"menuControllers";"probe";"probe.obj"]
            controllerSg path 90.0 0.0 30.0 probeScaleTrafo probeContrPos
           
        let laserContrSg = 
            let path = [__SOURCE_DIRECTORY__; "..";"..";"models";"menuControllers";"laser";"laser.obj"]
            controllerSg path 90.0 0.0 0.0 rayScaleTrafo rayContrPos

        let clippingContrSg = 
            let path = [__SOURCE_DIRECTORY__; "..";"..";"models";"menuControllers";"clipping";"clipping.obj"]
            controllerSg path 90.0 0.0 -30.0 clippingScaleTrafo clippingContrPos

        let lines = m.ray |> AVal.map (fun r -> [|Line3d(r.Origin, r.Direction)|]) 

        let raySg =
            lines
            |> Sg.lines m.rayColor
            |> Sg.noEvents
            |> Sg.uniform "LineWidth" (AVal.constant 5)
            |> Sg.effect [
                toEffect DefaultSurfaces.trafo
                toEffect DefaultSurfaces.vertexColor
                toEffect DefaultSurfaces.thickLine
                ]
            |> Sg.depthTest (AVal.constant DepthTestMode.LessOrEqual)
            |> Sg.pass pass1

        let quadPositions = m.tvQuad |> AVal.map (fun q -> [|q.P0.ToV3f(); q.P1.ToV3f(); q.P2.ToV3f(); q.P3.ToV3f()|])

        let browserSg = 
            Sg.draw IndexedGeometryMode.TriangleList
            |> Sg.vertexAttribute DefaultSemantic.Positions quadPositions
            |> Sg.vertexAttribute DefaultSemantic.Normals (AVal.constant [| V3f.OOI; V3f.OOI; V3f.OOI; V3f.OOI |])
            |> Sg.vertexAttribute DefaultSemantic.DiffuseColorCoordinates  (AVal.constant  [| V2f.OO; V2f.OI; V2f.II; V2f.IO |])
            |> Sg.index (AVal.constant [|0;1;2; 0;2;3|])
            |> Sg.diffuseTexture client.Texture 
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.diffuseTexture
            }  

        let currentBox = 
            m.twoDModel.boxFilter |> AVal.map (fun b ->
                match b with 
                | Some box -> box.BoundingBox3d
                | None -> Box3d.Infinite
                )

        let boxSg = 
            Sg.box m.twoDModel.boxColor currentBox
            |> Sg.noEvents
            |> Sg.trafo m.heraTransformations
            |> Sg.fillMode (FillMode.Line |> AVal.constant)




         
        //let unusedParts = 
        
        //    let quadSg = planeSg quadPositions C4f.Gray10 FillMode.Fill (AVal.constant BlendMode.None) pass0

        //    let statisticsSg = 
        //        Sg.textWithConfig cfg (AVal.constant "Hello, User! \n Flip the text \n Float 5.505050 \n Not working ;(")
        //        |> Sg.noEvents
        //        |> Sg.transform (Trafo3d.FromOrthoNormalBasis(V3d.IOO,-V3d.OIO, V3d.OOI))
        //        |> Sg.scale 0.1

        //    let statisticsSg = 
        //        Sg.textWithBackground (Font "Calibri") C4b.White C4b.Blue Border2d.None ( AVal.constant "Hello, User! \n Flip the text \n Float 5.505050 \n Not working ;(")
        //        |> Sg.noEvents
        //        |> Sg.scale 0.1
        //        |> Sg.transform (Trafo3d.FromOrthoNormalBasis(V3d.IOO,-V3d.OIO, V3d.OOI))
        //        |> Sg.billboard
        //        |> Sg.noEvents

        //        |> Sg.transform (Trafo3d.RotationZInDegrees(180.0))
        //    //let statisticsSg2 = 
        //    //    Sg.textWithBackground (Font "Calibri") C4b.White C4b.Black Border2d.None text
        //    //    |> Sg.noEvents
        //    //    |> Sg.trafo statisticsScaleTrafo
        //    //    |> Sg.translate 0.0 (p.radius * 0.75) 0.0
        //    //    |> Sg.transform (Trafo3d.FromOrthoNormalBasis(V3d.IOO,-V3d.OIO, V3d.OOI))
        //    //    |> Sg.myBillboard viewTrafo
        //    //    |> Sg.applyRuntime runtime
        //    //    |> Sg.noEvents
        //    //    |> Sg.transform (Trafo3d.Translation(p.center))
        //    //    |> Sg.translate 0.0 0.0 (p.radius * 1.8)
        //    //    |> Sg.pass pass2
        
        //    let world = 
        //        Sg.textWithConfig TextConfig.Default m.text
        //        |> Sg.noEvents
        //        |> Sg.andAlso deviceSgs
        //        |> Sg.pass pass0

        //    let heraBBox = 
        //        Sg.box (AVal.constant C4b.White) m.twoDModel.currHeraBBox
        //        |> Sg.noEvents
        //        |> Sg.trafo m.heraTransformations
        //        |> Sg.fillMode (FillMode.Line |> AVal.constant)

        //    let t = 
        //        Sg.textWithConfig ({ TextConfig.Default with renderStyle = RenderStyle.Normal })  (AVal.constant "hello world\nsuperstar")
        //        |> Sg.noEvents
        //        |> Sg.scale 0.08
        //        |> Sg.transform (Trafo3d.FromOrthoNormalBasis(V3d.IOO,-V3d.OIO, V3d.OOI))
        //        |> Sg.myBillboard viewTrafo
        //        |> Sg.applyRuntime runtime
        //        |> Sg.noEvents

        //    let label2 =
        //       //Sg.text f C4b.Green message
        //        Sg.markdown MarkdownConfig.light (AVal.constant "Hello, User! \n Flip the text \n Float 5.505050 \n Not working ;(")
        //            |> Sg.noEvents
        //            |> Sg.scale 0.1
        //            |> Sg.transform (Trafo3d.FromOrthoNormalBasis(-V3d.IOO,V3d.OOI,V3d.OIO))
        //            |> Sg.trafo (m.controllerTrafo |> AVal.map (fun mtw -> mtw.Forward.TransformPos(V3d.OOO) |> Trafo3d.Translation))
        //            |> Sg.billboard
        //            |> Sg.noEvents
        //            |> Sg.translate 0.0 5.0 0.0
        //    0

        Sg.ofSeq [
            deviceSgs; currentSphereProbeSg; probesSgs; heraSg; clipPlaneSg; tvSg;
            tvPosSphereSg; probeContrSg; laserContrSg; clippingContrSg; raySg;
            browserSg; boxSg; mainTouchpadSphereSg; secondTouchpadSphereSg
        ] |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.simpleLighting
            }    

    let pause (info : VrSystemInfo) (m : AdaptiveModel) =
        Sg.box' C4b.Red Box3d.Unit
        |> Sg.noEvents
        |> Sg.shader {
            do! DefaultSurfaces.trafo
            do! DefaultSurfaces.vertexColor
            do! DefaultSurfaces.simpleLighting
        }

    let app (client : Browser) (histogramClient : Browser) (viewTrafos : aval<Trafo3d []>) (projTrafos : aval<Trafo3d []>) (runtime : IRuntime) : ComposedApp<Model,AdaptiveModel,Message> =
        let frames = DataLoader.loadDataAllFrames runtime
        client.SetFocus true
        let viewTrafo = combinedTrafo viewTrafos
        let projTrafo = combinedTrafo projTrafos
        {
            unpersist = Unpersist.instance
            initial = initial runtime frames
            update = update runtime client histogramClient viewTrafo projTrafo frames
            threads = threads
            input = input 
            ui = ui runtime frames
            vr = vr runtime client histogramClient viewTrafo frames
            pauseScene = Some pause
        }