﻿namespace ImpactVisualization

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

              
    let threads (model : Model) =
        AardVolume.App.threads model.twoDModel |> ThreadPool.map TwoD
        
    let input (msg : VrMessage) =
        match msg with
        | VrMessage.PressButton(controllerId, buttonId) ->
           // printf "press button: %A " (controllerId, buttonId)
            if buttonId = 1 then 
                [ResetHera]
            else if buttonId = 2 then
                [GrabHera controllerId]
            else 
                []
        | VrMessage.UnpressButton(controllerId, buttonId) ->
            //printf "unpress button: %A " (controllerId, buttonId)
            if buttonId = 2 then
                [UngrabHera controllerId]
            else 
                []
        | VrMessage.Press(controllerId, buttonId) ->
            //printf "press: %A " (controllerId, buttonId)
            if buttonId = 0 then 
                [ToggleControllerMenu controllerId; OpenControllerMenu controllerId]
                //[ActivateRay controllerId]
            else if buttonId = 1 then
                [ActivateControllerMode controllerId]
            else
                []
        | VrMessage.Unpress(controllerId, buttonId) ->
          //  printf "unpress: %A " (controllerId, buttonId)
            if buttonId = 1 then
                [DeactivateControllerMode controllerId]
            else
                []
        | VrMessage.Touch(controllerId, buttonId) ->
           // printf "touch: %A " (controllerId, buttonId)
            if buttonId = 0 then 
                []
                //[ActivatePlane controllerId]
            else 
                []
        | VrMessage.Untouch(controllerId, buttonId) ->
            //printf "untouch: %A " (controllerId, buttonId)
            [UntouchDevice controllerId]
        | VrMessage.ValueChange(controllerId, buttonId, value) ->
            //printf "value change: %A " (controllerId, buttonId, value)
            if buttonId = 0 then 
                [ChangeTouchpadPos (controllerId, value); ScaleHera value.X; ChangeControllerMode controllerId; SelectAttribute controllerId]
            else 
                []
        | VrMessage.UpdatePose(controllerId, pose) ->
            if pose.isValid then [MoveController (controllerId, pose.deviceToWorld)] else []
        | _ -> []

    let ui (runtime : IRuntime) (data : Frame[]) (info : VrSystemInfo) (m : AdaptiveModel) : DomNode<Message> = // 2D UI
        div [] [
            AardVolume.App.view runtime data m.twoDModel |> UI.map TwoD
        ]

    let vr (runtime : IRuntime) (client : Browser) (viewTrafos : aval<Trafo3d []>) (data : Frame[]) (info : VrSystemInfo) (m : AdaptiveModel) : ISg<Message> = // HMD Graphics
        let pass0 = RenderPass.main
        let pass1 = RenderPass.after "pass1" RenderPassOrder.Arbitrary pass0 
        let pass2 = RenderPass.after "pass2" RenderPassOrder.Arbitrary pass1            
         
        let deviceSgs = 
            info.state.devices |> AMap.toASet |> ASet.chooseA (fun (_,d) ->
                d.model |> AVal.map (fun m ->
                    match m.Value with
                    | Some sg -> 
                        sg 
                        |> Sg.noEvents 
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

        let world = 
            Sg.textWithConfig TextConfig.Default m.text
            |> Sg.noEvents
            |> Sg.andAlso deviceSgs
            |> Sg.pass pass0

        let touching = m.touchpadDeviceId |> AVal.map (fun id -> id.IsSome)

        let touchpadPos = m.currTouchPadPos |> AVal.map (fun pos -> let z = tan (6.5 * (Math.PI / 180.0))
                                                                    V3d(pos, z * (pos.Y + 1.0)) * 0.019)

        let touchpadSphereSg = 
            Sg.sphere' 9 C4b.LightGreen 0.002
            |> Sg.noEvents
            |> Sg.translate 0.0 -0.05 0.0035 // translation so that it is in the middle of the touchpad
            |> Sg.translate' touchpadPos
            |> Sg.trafo m.touchpadDeviceTrafo
            |> Sg.onOff touching

        //TODO: Why not directly use hera trafo?!?!?!
        //let trafo = m.heraTrafo |> AVal.map (fun t -> trafoOrIdentity t)
            //AVal.map2 (fun contr heraContr -> 
            //    let contrTrafo = trafoOrIdentity contr 
            //    let heraContrTrafo = trafoOrIdentity heraContr
            //    heraContrTrafo * contrTrafo) m.controllerTrafo m.heraToControllerTrafo


        let heraScaleTrafo = m.scalingFactorHera |> AVal.map(fun s -> Trafo3d (Scale3d(s)))


        let sphereTrafo = 
            AVal.map2 (fun sphereContrTrafo (sphereScale : float) -> 
                Trafo3d(Scale3d(sphereScale)) * sphereContrTrafo
                ) m.sphereControllerTrafo m.sphereScale

       // let sphereScaleTrafo = m.sphereScale |> AVal.map(fun s -> Trafo3d (Scale3d(s)))

      //  let sphereTrafo = m.sphereControllerTrafo |> AVal.map (fun t -> trafoOrIdentity t)

        let contrClippingPlane = m.planeCorners |> AVal.map (fun c -> Plane3d(c.P0, c.P1, c.P2))

        let mutable mode = BlendMode(true)
        mode.Enabled <- true
        mode.Operation <- BlendOperation.Add
        mode.AlphaOperation <- BlendOperation.Add
        mode.SourceFactor <- BlendFactor.SourceAlpha
        mode.DestinationFactor <- BlendFactor.InvSourceAlpha
        mode.SourceAlphaFactor <- BlendFactor.One
        mode.DestinationAlphaFactor <- BlendFactor.InvSourceAlpha


        let heraTransformations = 
            AVal.map2 (fun heraScale heraMove -> 
                heraScale * Trafo3d.Translation(0.0, 0.0, 0.7) * heraMove
                ) heraScaleTrafo m.heraTrafo 

        let probeScale = 
            AVal.map3 (fun currProbMan currScale lastScale ->
                if currProbMan then 
                    currScale
                else
                    lastScale
                ) m.currentProbeManipulated m.sphereScale m.lastSphereScale


        let sphereProbe = 
            AVal.map3 (fun (trafo : Trafo3d) scale initRadius -> 
                if trafo.Forward.IsIdentity() then 
                    Sphere3d.Invalid
                else 
                    let spherePos = trafo.Forward.TransformPos(V3d.OOO)
                    let sphereRadius : float = initRadius * scale 
                    Sphere3d(spherePos, sphereRadius)
                ) m.sphereControllerTrafo probeScale m.sphereRadius

        //let probe = 
        //    m.lastFilterProbe |> AVal.map (fun pr -> 
        //        match pr with 
        //        | AdaptiveSome p -> 
        //        | AdaptiveNone -> )

        ////TODO: Must be changed, escpesially for deleting of probes and filtering
        //let probeWhenHeraMoved = 
        //    AVal.map2 (fun lastProbe  (heraTrafos : Trafo3d) -> 
        //        //match lastProbe with 
        //        //    | Some probe ->
        //        //        let newCenter = heraTrafos.Forward.TransformPos(probe.centerRelToHera)
        //        //        let newRadius = probe.radiusRelToHera * heraTrafos.Forward.GetScaleVector3().X
        //        //        let sphere = Sphere3d(newCenter, newRadius)
        //        //        sphere 
        //        //    | None -> Sphere3d.Invalid
        //    ) m.lastFilterProbe m.heraTransformations
           

       // let sphereProbe = Sphere3d.Invalid |> AVal.constant

        let combinedTrafo = 
            viewTrafos 
            |> AVal.map (fun trafos ->
                            let left = trafos.[0]
                            let right = trafos.[1]
                            let combined = (left.Forward + right.Forward)/2.0
                            let combinedInv = (left.Backward + right.Backward)/2.0
                            Trafo3d(combined, combinedInv))

        let leftTrafo = 
            viewTrafos 
            |> AVal.map (fun trafos ->
                            let left = trafos.[0]
                            left)
        
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
                m.boxFilter sphereProbe m.currFilters m.dataRange m.colorValue.c 
                m.cameraState.view leftTrafo
                runtime
            |> Sg.noEvents
            |> Sg.trafo model.heraTransformations
            |> Sg.pass pass0
            //|> Sg.blendMode (AVal.constant mode)

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

        let heraBBox = 
            Sg.box (AVal.constant C4b.White) m.twoDModel.currHeraBBox
            |> Sg.noEvents
            |> Sg.trafo m.heraTransformations
            |> Sg.fillMode (FillMode.Line |> AVal.constant)

        let currentSphereProbeSg = 
            Sg.sphere 9 m.sphereColor m.sphereRadius
            |> Sg.noEvents
            |> Sg.trafo sphereTrafo
            |> Sg.onOff m.currentProbeManipulated
           // |> Sg.fillMode (FillMode.Fill |> AVal.constant)
            |> Sg.cullMode (CullMode.Front |> AVal.constant)
            |> Sg.blendMode (AVal.constant mode)
            |> Sg.pass pass1

        let cfg : Aardvark.Rendering.Text.TextConfig = 
               {
                   font = Font "Calibri"
                   color = C4b.White
                   align = TextAlignment.Center
                   flipViewDependent = true
                   renderStyle       = RenderStyle.Billboard
               }


        let t = 
            Sg.textWithConfig ({ TextConfig.Default with renderStyle = RenderStyle.Normal })  (AVal.constant "hello world\nsuperstar")
            |> Sg.noEvents
            |> Sg.scale 0.08
            |> Sg.transform (Trafo3d.FromOrthoNormalBasis(V3d.IOO,-V3d.OIO, V3d.OOI))
            |> Sg.myBillboard combinedTrafo
            |> Sg.applyRuntime runtime
            |> Sg.noEvents
            
        let probesSgs = 
            m.allProbes |> AMap.toASet |> ASet.chooseA (fun (key, probe) ->
                probe.Current |> AVal.map (fun p -> 
                    let color =
                        AVal.map2 (fun probeIntId delId ->
                            match probeIntId with 
                            | Some i -> if i = key then 
                                            match delId with 
                                            | Some dId -> C4b(1.0,0.0,0.0,0.4) 
                                            | None -> C4b(0.0,1.0,0.0,0.4) 
                                        else 
                                            if p.insideHera then C4b(0.0,0.0,1.0,0.4)  else C4b(1.0,1.0,1.0,0.4)
                            | None -> if p.insideHera then C4b(0.0,0.0,1.0,0.4)  else C4b(1.0,1.0,1.0,0.4)
                        ) m.probeIntersectionId m.deletionControllerId
                    //let sphere = Sphere3d(p.center, p.radius)
                    //let sphereBoxSg = Sg.box' C4b.White sphere.BoundingBox3d |> Sg.noEvents |> Sg.fillMode (FillMode.Line |> AVal.constant)
                    let statisticsScaleTrafo = 
                        m.sphereRadius |> AVal.map (fun r -> 
                                                        let sphereScale = p.radius / r
                                                        let statScale = 0.05 * sphereScale
                                                        Trafo3d(Scale3d(statScale)))
                    let text = AVal.constant p.currStatistics
                    let statisticsSg = 
                       Sg.textWithConfig ({ TextConfig.Default with renderStyle = RenderStyle.Normal })  text
                       |> Sg.noEvents
                       |> Sg.trafo statisticsScaleTrafo
                       |> Sg.translate 0.0 (p.radius * 0.75) 0.0
                       |> Sg.transform (Trafo3d.FromOrthoNormalBasis(V3d.IOO,-V3d.OIO, V3d.OOI))
                       |> Sg.myBillboard combinedTrafo
                       |> Sg.applyRuntime runtime
                       |> Sg.noEvents
                       |> Sg.transform (Trafo3d.Translation(p.center))
                       |> Sg.translate 0.0 0.0 (p.radius * 1.8)
                    Sg.sphere 9 color (AVal.constant p.radiusRelToHera)
                    |> Sg.noEvents
                    |> Sg.transform (Trafo3d.Translation(p.centerRelToHera))
                    |> Sg.trafo m.heraTransformations // so that it moves with hera!!!
                    |> Sg.andAlso statisticsSg
                    |> Some
                )
            ) 
            |> Sg.set
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.simpleLighting
            }
            |> Sg.cullMode (CullMode.Front |> AVal.constant)
            |> Sg.blendMode (AVal.constant mode)
            |> Sg.pass pass1

        let lines = m.ray |> AVal.map (fun r -> [|Line3d(r.Origin, r.Direction)|]) 

        let ray =
            lines
                |> Sg.lines m.rayColor
                |> Sg.noEvents
                |> Sg.uniform "LineWidth" (AVal.constant 5)
                |> Sg.effect [
                    toEffect DefaultSurfaces.trafo
                    toEffect DefaultSurfaces.vertexColor
                    toEffect DefaultSurfaces.thickLine
                    ]
                //|> Sg.pass (RenderPass.after "lines" RenderPassOrder.Arbitrary RenderPass.main)
                |> Sg.depthTest (AVal.constant DepthTestMode.LessOrEqual)
                |> Sg.pass pass2
      
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
        let quadPositions = m.tvQuad |> AVal.map (fun q -> [|q.P0.ToV3f(); q.P1.ToV3f(); q.P2.ToV3f(); q.P3.ToV3f()|])
        let texturePositions =  AVal.constant  [|V3f(-1.0, -1.0, 0.0); V3f(1.0, -1.0, 0.0); V3f(1.0, 1.0, 0.0); V3f(-1.0, 1.0, 0.0)|]

        //let touchpadTex = 
        //    m.touchpadTexture |> AVal.map (fun tex -> 
        //        let path = texturesPath + tex
        //        FileTexture(path, true) :> ITexture) 

        let clipPlaneSg = planeSg planePositions (C4f(0.0,0.0,1.0,0.1)) FillMode.Fill (AVal.constant mode) pass1
        let quadSg = planeSg quadPositions C4f.Gray10 FillMode.Fill (AVal.constant BlendMode.None) pass0

        //let textureTrafo = m.textureDeviceTrafo |> AVal.map (fun t -> trafoOrIdentity t)

        let touchpadPlaneSg = 
            Sg.draw IndexedGeometryMode.TriangleList
            |> Sg.vertexAttribute DefaultSemantic.Positions texturePositions
            |> Sg.vertexAttribute DefaultSemantic.Normals (AVal.constant [| V3f.OOI; V3f.OOI; V3f.OOI; V3f.OOI |])
            |> Sg.vertexAttribute DefaultSemantic.DiffuseColorCoordinates  (AVal.constant  [| V2f.OO; V2f.IO; V2f.II; V2f.OI |])
            |> Sg.index (AVal.constant [|0;1;2; 0;2;3|])
            |> Sg.scale 0.0205
            |> Sg.transform (Trafo3d.RotationXInDegrees(6.5))
            |> Sg.translate 0.0 -0.05 0.0051
            |> Sg.trafo m.textureDeviceTrafo
            |> Sg.onOff m.showTexture
            |> Sg.diffuseTexture m.touchpadTexture
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.diffuseTexture
                do! TouchpadShaders.fragmentSh
            }
         


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
                //|> Sg.fillMode (fillmode |> AVal.constant)
                //|> Sg.blendMode blendmode
                //|> Sg.pass renderPass                


        let mutable temp = true

        //printf "trigger: \n"

        //if m.rayTriggerClicked then 
        //    printf "YESSSSSSSSSSS \n" 
        //    m.client.SetFocus true
        //    m.client.Mouse.Click(PixelPosition(400, 300, screenResolution.X, screenResolution.Y), MouseButtons.Left)
        //    temp <- false

        
       
        //let temp = 
        //    AVal.map (fun clicked pos ->
               
        //        ) m.rayTriggerClicked m.clickPosition
        
        //m.toggleAnim |> AVal.map (fun b ->
        //    if b then client.Mouse.Click(PixelPosition(V2i(30,23), Box2i(V2i(0,0),V2i(0,0))), Aardvark.Application.MouseButtons.Left))
        //|> ignore
               // printfn "%A" res



        let tvSg = 
            Loader.Assimp.load (Path.combine [__SOURCE_DIRECTORY__; "..";"..";"models";"tv";"tv.obj"])
                |> Sg.adapter

                |> Sg.transform (Trafo3d.Scale(1.0, 1.0, -1.0))
                |> Sg.trafo (tvTrafo |> AVal.constant)
                //|> Sg.scale 1.0
                //|> Sg.transform (Trafo3d.RotationEulerInDegrees(90.0, 0.0, -90.0))
                //|> Sg.translate 2.5 1.0 1.5

                |> Sg.shader {
                    do! DefaultSurfaces.trafo
                    do! DefaultSurfaces.diffuseTexture
                    do! DefaultSurfaces.normalMap
                    do! DefaultSurfaces.simpleLighting
                }
                |> Sg.pass pass0

        //let message = 
        //    m.hitPoint 
        //        |> AVal.map (fun p ->
        //            let m = 
        //                "X: " + p.X.ToString() + "\n" +
        //                "Y: " + p.Y.ToString() + "\n" + 
        //                "Z: " + p.Z.ToString() + "\n"
        //            m
        //            )


        // TODO: X and Y must be swapped for some reason !! Find why??!!
        let message = 
            m.screenHitPoint 
                |> AVal.map (fun p ->
                    let m = 
                        "X: " + p.Y.ToString() + "\n" +
                        "Y: " + p.X.ToString() + "\n" 
                    m
                    )

        let billboardSg = 
            Sg.markdown MarkdownConfig.light message
                |> Sg.billboard
                |> Sg.noEvents
                |> Sg.scale 0.1
                |> Sg.trafo (Trafo3d.RotationEulerInDegrees(90.0, 0.0, -90.0) |> AVal.constant)
                |> Sg.translate 2.5 2.0 0.5
                

        //let flatScreenSg = 
        //    Sg.ofSeq [quadSg; tvSg]
        //        |> Sg.scale 2.0
        //        |> Sg.transform (Trafo3d.RotationEulerInDegrees(90.0, 0.0, -90.0))
        //        |> Sg.translate 2.5 1.0 1.5

        let contrOrientation = m.menuControllerTrafo |> AVal.map (fun t -> t.GetOrthoNormalOrientation()) 

        let controllerPos position = m.menuControllerTrafo |> AVal.map (fun t -> t.Forward.TransformPos(position)) 

        let probeContrPos = controllerPos (V3d(-0.12, 0.11, 0.0))
        let rayContrPos = controllerPos (V3d(0.0, 0.15, 0.0))
        let clippingContrPos = controllerPos (V3d(0.12, 0.11, 0.0))

        let scaleTrafo mode = 
            m.controllerMode |> AVal.map (fun m ->
                if m = mode then
                    Trafo3d (Scale3d(0.7))
                else
                    Trafo3d (Scale3d(0.5)))

        let probeScaleTrafo = scaleTrafo ControllerMode.Probe
        let rayScaleTrafo = scaleTrafo ControllerMode.Ray
        let clippingScaleTrafo = scaleTrafo ControllerMode.Clipping

        let controllerTypeMenuOpen = 
            AVal.map2 (fun menuOpen level -> 
                menuOpen && level = 1
                ) m.controllerMenuOpen m.menuLevel
              
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
                do! DefaultSurfaces.normalMap
                do! DefaultSurfaces.simpleLighting
            }
            |> Sg.pass pass0


        let probeContrSg = 
            let path = [__SOURCE_DIRECTORY__; "..";"..";"models";"menuControllers";"probe";"probe.obj"]
            controllerSg path 90.0 0.0 30.0 probeScaleTrafo probeContrPos
           
        let laserContrSg = 
            let path = [__SOURCE_DIRECTORY__; "..";"..";"models";"menuControllers";"laser";"laser.obj"]
            controllerSg path 90.0 0.0 0.0 rayScaleTrafo rayContrPos

        let clippingContrSg = 
            let path = [__SOURCE_DIRECTORY__; "..";"..";"models";"menuControllers";"clipping";"clipping.obj"]
            controllerSg path 90.0 0.0 -30.0 clippingScaleTrafo clippingContrPos


        //let statisticsSg = 
        //    Sg.textWithConfig cfg (AVal.constant "Hello, User! \n Flip the text \n Float 5.505050 \n Not working ;(")
        //    |> Sg.noEvents
        //    |> Sg.transform (Trafo3d.FromOrthoNormalBasis(V3d.IOO,-V3d.OIO, V3d.OOI))
        //    |> Sg.scale 0.1

        //let statisticsSg = 
        //    Sg.textWithBackground (Font "Calibri") C4b.White C4b.Blue Border2d.None ( AVal.constant "Hello, User! \n Flip the text \n Float 5.505050 \n Not working ;(")
        //    |> Sg.noEvents
        //    |> Sg.scale 0.1
        //    |> Sg.transform (Trafo3d.FromOrthoNormalBasis(V3d.IOO,-V3d.OIO, V3d.OOI))
        //    |> Sg.billboard
        //    |> Sg.noEvents

            //|> Sg.transform (Trafo3d.RotationZInDegrees(180.0))



        let label2 =
           //Sg.text f C4b.Green message
            Sg.markdown MarkdownConfig.light (AVal.constant "Hello, User! \n Flip the text \n Float 5.505050 \n Not working ;(")
                |> Sg.noEvents
                |> Sg.scale 0.1
                |> Sg.transform (Trafo3d.FromOrthoNormalBasis(-V3d.IOO,V3d.OOI,V3d.OIO))
                |> Sg.trafo (m.controllerTrafo |> AVal.map (fun mtw -> mtw.Forward.TransformPos(V3d.OOO) |> Trafo3d.Translation))
                |> Sg.billboard
                |> Sg.noEvents
                |> Sg.translate 0.0 5.0 0.0



        Sg.ofSeq [
            deviceSgs; currentSphereProbeSg; probesSgs; heraSg; clipPlaneSg; tvSg;
            billboardSg; probeContrSg; laserContrSg; clippingContrSg; ray;
            browserSg; boxSg; touchpadSphereSg; touchpadPlaneSg
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

    let app (client : Browser) (viewTrafos : aval<Trafo3d []>) (runtime : IRuntime) : ComposedApp<Model,AdaptiveModel,Message> =
        let frames = DataLoader.loadDataAllFrames runtime
        {
            unpersist = Unpersist.instance
            initial = initial runtime frames
            update = update runtime client frames
            threads = threads
            input = input 
            ui = ui runtime frames
            vr = vr runtime client viewTrafos frames
            pauseScene = Some pause
        }