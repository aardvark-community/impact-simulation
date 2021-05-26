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
            | 1 -> [ResetHera controllerId; CopyBoxPlot controllerId]
            | 2 -> [GrabHera controllerId; ToggleBillboardVisibility controllerId; GrabTv controllerId]
            | _ -> []
        | VrMessage.UnpressButton(controllerId, buttonId) ->
            match buttonId with 
            | 2 -> [UngrabHera controllerId; UngrabTv controllerId]
            | _ -> []
        | VrMessage.Press(controllerId, buttonId) ->
            match buttonId with 
            | 0 -> [ToggleMainMenu controllerId; OpenMainMenu controllerId]
            | 1 -> [TakeBoxPlot controllerId; ActivateControllerMode controllerId; 
                    ScaleProbe controllerId; DeleteProbe controllerId; 
                    DeleteClippingPlane controllerId; SelectBoxPlotProbes controllerId; 
                    PlaceBoxPlot controllerId;DeleteBoxPlot controllerId]
            | _ -> []
        | VrMessage.Unpress(controllerId, buttonId) ->
            match buttonId with 
            | 1 ->  [DeactivateControllerMode controllerId; StopProbeScale controllerId; LeaveBoxPlot controllerId]
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
                     ScaleTv (controllerId, value.X); 
                     ChangeMainControllerMode controllerId; 
                     ChangeBoxPlotAttribute controllerId;
                     SelectGlobalAttribute controllerId;
                     SelectProbeAttribute controllerId;
                     ChangeBillboard controllerId
                     ]
            | _ -> []
        | VrMessage.UpdatePose(controllerId, pose) ->
            if pose.isValid then [MoveController (controllerId, pose.deviceToWorld); SetController controllerId] else []
        | _ -> []

    let ui (runtime : IRuntime) (data : Frame[]) (boxPlotClient : Browser) (info : VrSystemInfo) (m : AdaptiveModel) : DomNode<Message> = // 2D UI
        div [] [AardVolume.App.view runtime data boxPlotClient m.twoDModel |> UI.map TwoD]

    let planeSg positions : ISg<Message> = 
        Sg.draw IndexedGeometryMode.TriangleList
        |> Sg.vertexAttribute DefaultSemantic.Positions positions
        |> Sg.vertexAttribute DefaultSemantic.Normals (AVal.constant [| V3f.OOI; V3f.OOI; V3f.OOI; V3f.OOI |])
        |> Sg.vertexAttribute DefaultSemantic.DiffuseColorCoordinates  (AVal.constant  [| V2f.OO; V2f.IO; V2f.II; V2f.OI |])
        |> Sg.index (AVal.constant [|0;1;2; 0;2;3|])

    let vr (runtime : IRuntime) (client : Browser) (histogramClient : Browser) (boxPlotClient : Browser) (viewTrafo : aval<Trafo3d>) (data : Frame[]) (info : VrSystemInfo) (m : AdaptiveModel) : ISg<Message> = // HMD Graphics
       
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

        let textPlaneSg showTexture texture = 
            planeSg pPositions
            |> Sg.translate 0.0 -0.0006 0.0
            |> Sg.scale 0.022
            |> Sg.onOff showTexture
            |> Sg.diffuseTexture texture
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.diffuseTexture
            }     
            
        let showTextPlaneTexture deviceId contrId = 
            (deviceId, contrId) 
            ||> AVal.map2 (fun (currId : int) (id : Option<int>) -> 
                match id with 
                | Some i when i = currId -> true
                | _ -> false)

        let textPlaneSgs deviceId = 
            let showMainTexture = showTextPlaneTexture deviceId m.mainControllerId
            let showSecondTexture = showTextPlaneTexture deviceId m.secondControllerId
            textPlaneSg showMainTexture m.mainContrScreenTexture
            |> Sg.andAlso (textPlaneSg showSecondTexture m.secondContrScreenTexture)

        let texturePositions =  AVal.constant  [|V3f(-1.0, -1.0, 0.0); V3f(1.0, -1.0, 0.0); V3f(1.0, 1.0, 0.0); V3f(-1.0, 1.0, 0.0)|]

        let touchpadPlaneSg showTexture texture =  
            planeSg texturePositions
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

        let showTexture deviceId contrId showTex = 
            (deviceId, contrId, showTex) 
            |||> AVal.map3 (fun (currId : int) (id : Option<int>) show -> 
                match id with 
                | Some i when i = currId -> show
                | _ -> false)

        let touchpadSgs deviceId = 
            let showMainTexture = showTexture deviceId m.mainControllerId m.showMainTexture
            let showSecondTexture = showTexture deviceId m.secondControllerId m.showSecondTexture
            touchpadPlaneSg showMainTexture m.mainTouchpadTexture
            |> Sg.andAlso (touchpadPlaneSg showSecondTexture m.secondTouchpadTexture)

        //let touchingMain = m.touchpadDeviceId |> AVal.map (fun id -> id.IsSome)

        let touchpadPosMain = m.currMainTouchPadPos |> AVal.map (fun pos -> let z = tan (6.5 * (Math.PI / 180.0))
                                                                            V3d(pos, z * (pos.Y + 1.0)) * 0.019)

        let touchpadPosSecond = m.currSecondTouchPadPos |> AVal.map (fun pos -> let z = tan (6.5 * (Math.PI / 180.0))
                                                                                V3d(pos, z * (pos.Y + 1.0)) * 0.019)

        let touchpadSphereSg pos trafo touching = 
            Sg.sphere' 9 C4b.LightGreen 0.002
            |> Sg.noEvents
            |> Sg.translate 0.0 -0.05 0.0035 // translation so that it is in the middle of the touchpad
            |> Sg.translate' pos
            |> Sg.trafo trafo
            |> Sg.onOff touching

        let mainTouchpadSphereSg = touchpadSphereSg touchpadPosMain m.mainControllerTrafo m.mainTouching
        let secondTouchpadSphereSg = touchpadSphereSg touchpadPosSecond m.secondControllerTrafo m.secondTouching

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


        let contrOrientation = m.mainControllerTrafo |> AVal.map (fun t -> t.GetOrthoNormalOrientation()) 
        let controllerTypeMenuOpen = AVal.map3 (fun menuOpen level controllerMode -> menuOpen && level = 1 && not (controllerMode = ControllerMode.Analyze)) m.mainMenuOpen m.menuLevel m.controllerMode

        let controllerSg path rotX rotY rotZ scTrafo (contrPos : V3d) = 
            Loader.Assimp.load (Path.combine path)
            |> Sg.adapter
            |> Sg.transform (Trafo3d.Scale(1.0, 1.0, -1.0))
            |> Sg.transform (Trafo3d.RotationEulerInDegrees(rotX, rotY, rotZ))
            |> Sg.trafo scTrafo 
            |> Sg.transform (Trafo3d.Translation(contrPos))
            //|> Sg.trafo contrOrientation
            //|> Sg.translate' contrPos
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.diffuseTexture
                do! DefaultSurfaces.simpleLighting
            }
            |> Sg.pass pass0

        let controllerPos position = m.mainControllerTrafo |> AVal.map (fun t -> t.Forward.TransformPos(position)) 

        let probeContrPos = (V3d(-0.12, 0.11, 0.0))
        let rayContrPos = (V3d(0.0, 0.15, 0.0))
        let clippingContrPos = (V3d(0.12, 0.11, 0.0))

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

        let smallControllersSgs deviceId = 
            let visible =
                (deviceId, m.mainControllerId, controllerTypeMenuOpen) |||> AVal.map3 (fun currId mId isOpen ->
                    match mId with
                    | Some i when i = currId -> isOpen
                    | _ -> false)
            probeContrSg
            |> Sg.andAlso laserContrSg
            |> Sg.andAlso clippingContrSg
            |> Sg.onOff visible


        let defaultBoxPlotPositions = AVal.constant  [|V3f(-1.7, -1.0, 0.0); V3f(1.7, -1.0, 0.0); V3f(1.7, 1.0, 0.0); V3f(-1.7, 1.0, 0.0)|]
        
        let maxBoxPlotX = 1.7
        let maxBoxPlotY = 1.0
        let boxPlotScale = 0.2
        
        let activeBoxPlotTrafo (flipped : bool) =
            let scale = Trafo3d(Scale3d(boxPlotScale))
            let flip = if flipped then Trafo3d.FromOrthoNormalBasis(V3d.IOO,-V3d.OIO, V3d.OOI) else Trafo3d.Identity
            let rotate = Trafo3d.RotationXInDegrees(35.0)
            let translate = Trafo3d.Translation(0.0, 0.25, 0.1)
            scale * flip * rotate * translate

        let boxPlotSg deviceId =
            let showTexture =
                (deviceId, m.secondControllerId, m.showCurrBoxPlot) 
                |||> AVal.map3 (fun dId sId show ->
                    match sId with 
                    | Some id when id = dId -> show
                    | _ -> false)
            planeSg defaultBoxPlotPositions
            |> Sg.trafo (AVal.constant (activeBoxPlotTrafo true))
            |> Sg.onOff showTexture
            |> Sg.diffuseTexture boxPlotClient.Texture 
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.diffuseTexture
            }  
            |> Sg.blendMode (AVal.constant mode)
            |> Sg.pass pass2

        let convertRange (currPos : V2d) = 
            let newMaxX = maxBoxPlotX 
            let newMinX = -maxBoxPlotX 
            let newRange = newMaxX - newMinX
            let x = currPos.X
            let newX = (x * newRange) + newMinX

            let newMaxY = maxBoxPlotY
            let newMinY = -maxBoxPlotY
            let newRange = newMaxY - newMinY
            let y = 1.0 - currPos.Y
            let newY = (y * newRange) + newMinY
            V2d(newX, newY)

        let createVisualConnectionsSg (screenPositions : aval<V2d []>) (probesPositions : aval<V3d []>) 
                                      (trafo : aval<Trafo3d>) (showConnections : aval<bool>) = 
            (screenPositions, probesPositions)
            ||> AVal.map2 (fun screenPos probePos ->
                if screenPos.Length = probePos.Length then 
                    let positions = Array.zip screenPos probePos
                    positions 
                    |> Array.mapi (fun idx pos ->
                        let screenp = fst pos
                        let probep = snd pos
                        let finalPos =  
                            trafo
                            |> AVal.map (fun t ->
                                let convertedRange = convertRange screenp |> V3d
                                t.Forward.TransformPos(convertedRange))
                        let currColor =
                            match scheme10Colors.TryFind(idx) with
                            | Some c -> c
                            | None -> C4b.Black
                        let sphere = 
                            Sg.sphere' 6 currColor 0.008
                            |> Sg.noEvents
                            |> Sg.trafo (finalPos |> AVal.map (fun fp -> Trafo3d.Translation(fp)))
                        let currLine = finalPos |> AVal.map (fun fp -> [|Line3d(fp, probep)|]) 
                        currLine
                        |> Sg.lines (AVal.constant currColor)
                        |> Sg.noEvents
                        |> Sg.uniform "LineWidth" (AVal.constant 5)
                        |> Sg.effect [
                            toEffect DefaultSurfaces.trafo
                            toEffect DefaultSurfaces.vertexColor
                            toEffect DefaultSurfaces.thickLine
                            ]
                        |> Sg.andAlso sphere
                    )
                    |> Sg.ofArray
                    |> Sg.onOff showConnections
                else 
                    Sg.empty
            )
            |> Sg.dynamic

        let currentBPtrafo = 
            m.secondControllerTrafo
            |> AVal.map (fun t -> 
                let bpTF = activeBoxPlotTrafo false
                bpTF * t)
            
        let currBPvisualConnectionsSg = 
            createVisualConnectionsSg m.twoDModel.allProbesScreenPositions m.selectedProbesPositions currentBPtrafo m.showCurrBoxPlot

        let trafoBP = 
            m.takenBoxPlot |> AVal.bind (fun bp -> 
                match bp with 
                | AdaptiveSome b -> b.trafo
                | AdaptiveNone -> (AVal.constant Trafo3d.Identity))

        let texBP = 
            m.takenBoxPlot |> AVal.bind (fun bp -> 
                match bp with 
                | AdaptiveSome b -> (b.texture |> AVal.map (fun pi -> convertPixImageToITexture pi))
                | AdaptiveNone -> DefaultTextures.blackTex)

        let boxPlotScreenPos = 
            m.takenBoxPlot |> AVal.bind (fun bp -> 
                match bp with 
                | AdaptiveSome b -> b.screenPos
                | AdaptiveNone -> (AVal.constant [| |]))

        let boxPlotProbePos = 
            m.takenBoxPlot |> AVal.bind (fun bp -> 
                match bp with 
                | AdaptiveSome b -> b.probesPositions
                | AdaptiveNone -> (AVal.constant [| |]))

        let takenBoxPlotSg = 
            let visualConnections = 
                createVisualConnectionsSg boxPlotScreenPos boxPlotProbePos trafoBP m.movingBoxPlot
            planeSg defaultBoxPlotPositions
            |> Sg.trafo trafoBP
            |> Sg.onOff m.movingBoxPlot
            |> Sg.diffuseTexture texBP
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.diffuseTexture
            }  
            |> Sg.andAlso visualConnections

        let boxPlotsSgs = 
            m.allPlacedBoxPlots |> AMap.toASet |> ASet.choose (fun (key, boxPlot) ->
                let pixImage =
                    AVal.map2 (fun secondId tex  ->
                        match secondId with 
                        | Some i when i = key -> createColorPixImage boxPlotClient C4b.Red
                        | _ -> tex
                    ) m.secondContrBoxPlotIntersectionId boxPlot.texture 
                let texture = pixImage |> AVal.map (fun pi ->  convertPixImageToITexture pi)
                let scaleTrafo = 
                    m.mainContrBoxPlotIntersectionId 
                    |> AVal.map (fun mct ->
                        match mct with 
                        | Some i when i = key -> Trafo3d.Scale(0.95)
                        | _ -> Trafo3d.Identity)
                let bpTrafo = (scaleTrafo, boxPlot.trafo) ||> AVal.map2 (fun st bt -> st * bt)
                let visualConnections = createVisualConnectionsSg boxPlot.screenPos boxPlot.probesPositions bpTrafo (AVal.constant true)
                planeSg defaultBoxPlotPositions
                |> Sg.trafo bpTrafo
                |> Sg.diffuseTexture texture
                |> Sg.shader {
                    do! DefaultSurfaces.trafo
                    do! DefaultSurfaces.diffuseTexture
                }  
                |> Sg.andAlso visualConnections
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

        let mainSignPos = AVal.constant  [|V3f(-1.0, -1.0, 0.0); V3f(1.0, -1.0, 0.0); V3f(1.0, 1.0, 0.0); V3f(-1.0, 1.0, 0.0)|]
        let mainControllerSignSg deviceId = 
            let showTexture = 
                (deviceId, m.mainControllerId)
                ||> AVal.map2 (fun dId mId -> 
                    match mId with 
                    | Some id when id = dId -> true
                    | _ -> false)
            planeSg mainSignPos
            |> Sg.scale 0.005
            |> Sg.transform (Trafo3d.RotationXInDegrees(7.0))
            |> Sg.translate 0.0 -0.15 -0.0026
            |> Sg.onOff showTexture
            |> Sg.diffuseTexture m.mainContrSignTexture
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.diffuseTexture
            }  
            |> Sg.blendMode (AVal.constant mode)
            |> Sg.pass pass2



        let deviceSgs = 
            info.state.devices |> AMap.toASet |> ASet.chooseA (fun (_,d) ->
                //printf "Device Type: %A, %A \n" d.kind d.id
                //printf "Device Vibrate: %A \n" d.startVibrate  
                d.model |> AVal.map (fun m ->
                    match m.Value with
                    | Some sg -> 
                        sg 
                        |> Sg.noEvents 
                        |> Sg.andAlso textScreenSg
                        |> Sg.andAlso (textPlaneSgs d.id)
                        |> Sg.andAlso (touchpadSgs d.id)
                        |> Sg.andAlso (smallControllersSgs d.id)
                        |> Sg.andAlso (boxPlotSg d.id)
                        |> Sg.andAlso (mainControllerSignSg d.id)
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
            Sg.sphere 6 (AVal.constant C4b.White) m.sphereRadius
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

        let createStatisticsSg (p : AdaptiveProbe) (intersected : aval<bool>)= 
            let text = 
                 let allData = p.allData |> AMap.toAVal
                 (allData, p.currAttribute) ||> AVal.map2 (fun data attrib -> 
                    let array, stats = data.Item attrib
                    stats)
            let statisticsScaleTrafo = 
                (m.sphereRadius, p.radius, intersected) |||> AVal.map3 (fun r radius intr -> 
                    let sphereScale = radius / r
                    let newScale = if (intr && sphereScale < 1.0) then 1.0 else sphereScale
                    let statScale = 0.04 * newScale
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

        let createHistogramSg (p : AdaptiveProbe) (intersected : aval<bool>) = 
            let histogramScaleTrafo = 
                (m.sphereRadius, p.radius, intersected) |||> AVal.map3 (fun r radius intr -> 
                    let sphereScale = radius / r
                    let newScale = if (intr && sphereScale < 1.0) then 1.0 else sphereScale
                    let statScale = 0.12 * newScale
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
            planeSg probeHistogramPositions
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

        let showBillboard = (m.showBillboard, m.grabbingHera) ||> AVal.map2 (fun show grabbing -> show && not grabbing)

        //TODO: Probably causing overhead due to the complexity, especially when grabbing hera
        let probesSgs = 
            m.allProbes |> AMap.toASet |> ASet.choose (fun (key, probe) ->
                let colorAndIntersect =
                    AVal.map3 (fun mainId secondId col ->
                        match secondId with 
                        | Some i when i = key -> C4b.Red, false
                        | _ ->
                            match mainId with 
                            | Some i when i = key -> C4b.LightGreen, true
                            | _ -> col, false
                    ) m.mainContrProbeIntersectionId m.secondContrProbeIntersectionId probe.color
                let color = colorAndIntersect |> AVal.map (fun ci -> fst ci)
                let intersected = colorAndIntersect |> AVal.map (fun ci -> snd ci)
                let statisticsSg = createStatisticsSg probe intersected
                let probeHistogramSg = createHistogramSg probe intersected
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

        let viewVector = viewTrafo |> AVal.map (fun t -> t.Forward.C2.XYZ)

        let heraSg = 
            let model = m
            let m = m.twoDModel
            data
            |> HeraSg.HeraSg.createAnimatedVrSg 
                m.frame m.pointSize m.discardPoints m.normalizeData 
                m.enableShading m.reconstructNormal m.reconstructDepth 
                m.enableTransparency m.transparencyAttribute m.alphaStrength 
                m.transferFunction m.invertTF m.center m.startValue m.endValue
                m.lowerOutliers m.higherOutliers m.outliersRange model.scalingFactorHera 
                m.renderValue m.currentMap m.domainRange m.clippingPlane contrClippingPlane 
                m.boxFilter sphereProbe allPlacedSpheres spheresLength
                m.currFilters m.dataRange m.colorValue.c 
                m.cameraState.view viewTrafo viewVector
                runtime
            |> Sg.noEvents
            |> Sg.trafo model.heraTransformations
            |> Sg.pass pass0

        let clipPlaneSg positions (color : aval<C4b>) fillmode blendmode renderPass =
            Sg.draw IndexedGeometryMode.TriangleList
            |> Sg.vertexAttribute DefaultSemantic.Positions positions
            |> Sg.vertexAttribute DefaultSemantic.Normals (AVal.constant [| V3f.OOI; V3f.OOI; V3f.OOI; V3f.OOI |])
            |> Sg.vertexAttribute DefaultSemantic.DiffuseColorCoordinates  (AVal.constant  [| V2f.OO; V2f.IO; V2f.II; V2f.OI |])
            |> Sg.vertexBufferValue DefaultSemantic.Colors (color |> AVal.map (fun c -> c.ToC4f().ToV4f()))
            |> Sg.index (AVal.constant [|0;1;2; 0;2;3|])
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.simpleLighting
            }
            |> Sg.fillMode (fillmode |> AVal.constant)
            |> Sg.blendMode blendmode
            |> Sg.pass renderPass

        let planePositions = m.planeCorners |> AVal.map (fun q -> [|q.P0.ToV3f(); q.P1.ToV3f(); q.P2.ToV3f(); q.P3.ToV3f()|])

        let clipPlaneSg = clipPlaneSg planePositions m.clippingColor FillMode.Fill (AVal.constant mode) pass4

        //let quadPositions = m.tvQuad |> AVal.map (fun q -> [|q.P0.ToV3f(); q.P1.ToV3f(); q.P2.ToV3f(); q.P3.ToV3f()|])
        let qp = AVal.constant  [|browserQuad.P0; browserQuad.P1; browserQuad.P2; browserQuad.P3|]

        let browserSg = 
            Sg.draw IndexedGeometryMode.TriangleList
            |> Sg.vertexAttribute DefaultSemantic.Positions qp
            |> Sg.vertexAttribute DefaultSemantic.Normals (AVal.constant [| V3f.OOI; V3f.OOI; V3f.OOI; V3f.OOI |])
            |> Sg.vertexAttribute DefaultSemantic.DiffuseColorCoordinates  (AVal.constant  [| V2f.OO; V2f.IO; V2f.II; V2f.OI |])
            |> Sg.index (AVal.constant [|0;1;2; 0;2;3|])
            |> Sg.diffuseTexture client.Texture 
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.diffuseTexture
            }  

        let tvSg = 
            Loader.Assimp.load (Path.combine [__SOURCE_DIRECTORY__; "..";"..";"models";"tv";"tv.obj"])
            |> Sg.adapter
            |> Sg.transform (Trafo3d.Scale(1.0, 1.0, -1.0))
            |> Sg.andAlso browserSg
            |> Sg.trafo m.tvTransformations
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
            deviceSgs; currentSphereProbeSg; probesSgs; boxPlotsSgs; takenBoxPlotSg;
            heraSg; clipPlaneSg; tvSg; tvPosSphereSg; raySg;
            boxSg; mainTouchpadSphereSg; secondTouchpadSphereSg; currBPvisualConnectionsSg
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

    let app (client : Browser) (histogramClient : Browser) (boxPlotClient : Browser) (viewTrafos : aval<Trafo3d []>) (projTrafos : aval<Trafo3d []>) (runtime : IRuntime) : ComposedApp<Model,AdaptiveModel,Message> =
        let frames = DataLoader.loadDataAllFrames runtime
        client.SetFocus true
        //let viewTrafo = combinedTrafo viewTrafos
        //let projTrafo = combinedTrafo projTrafos
        let viewTrafo = viewTrafos |> AVal.map (fun vts -> vts.[0])
        let projTrafo = projTrafos |> AVal.map (fun pts -> pts.[0])
        {
            unpersist = Unpersist.instance
            initial = initial runtime frames
            update = update runtime client histogramClient boxPlotClient viewTrafo projTrafo frames
            threads = threads
            input = input 
            ui = ui runtime frames boxPlotClient
            vr = vr runtime client histogramClient boxPlotClient viewTrafo frames
            pauseScene = Some pause
        }