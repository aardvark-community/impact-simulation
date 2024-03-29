﻿namespace ImpactVisualization

open System
open System.IO
open System.Text.Json
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.Vr
open Aardvark.Application
open Aardvark.Rendering.GL


open FSharp.Data.Adaptive
open FSharpx.Collections

open AardVolume.Model
open AardVolume.App
open ImpactVisualization
open Aardvark.Cef
open Offler

open ImpactVisualization.UpdateFunctions
open ImpactVisualization.MoveControllerFunctions


module AppUpdate =
    open Aardvark.UI.Primitives
    open Aardvark.Rendering


    let initial runtime frames = 
        {   
            twoDModel = AardVolume.App.initial frames
            text = "hello" 
            threads = ThreadPool.Empty
            firstHistogram = true
            showBillboard = true
            devicesTrafos = HashMap.Empty
            hmdPos = V3d.OOO
            mainControllerId = None
            secondControllerId = None
            mainContrSignTexture = allTextures.Item "empty"
            mainControllerTrafo = Trafo3d.Identity
            secondControllerTrafo = Trafo3d.Identity
            heraTrafo = Trafo3d.Identity
            heraToControllerTrafo = Trafo3d.Identity
            grabbingHera = false
            holdingSphere = false
            scalingSphere = false
            scalingFactorHera = initialScalingHera
            scalingFactorTv = initialScalingTv
            lastTvScaleTrafo = Trafo3d(Scale3d(initialScalingTv))
            lastHeraScaleTrafo = Trafo3d(Scale3d(initialScalingHera))
            sphereScale = 1.0
            lastSphereScale = 1.0
            sphereRadius = 0.2
            createProbe = false
            currentProbeManipulated = false
            newProbePlaced = false
            existingProbeModified = false
            allProbes = HashMap.Empty
            mainContrProbeIntersectionId = None
            secondContrProbeIntersectionId = None
            lastFilterProbe = None
            lastFilterProbeId = None
            lastDeletedProbe = ""
            lastModifiedProbeIntId = -1
            boxPlotProbes = HashMap.empty
            boxPlotFrames = HashMap.empty
            framesOrder = Seq.empty
            mainContrBoxPlotIntersectionId = None
            secondContrBoxPlotIntersectionId = None
            movingBoxPlot = false
            takenBoxPlot = None
            takenBPTex = DefaultTextures.blackPix
            lastProbeId = 0
            currBoxPlotAttribSet = false
            currBoxPlotAttrib = RenderValue.Energy
            showCurrBoxPlot = false
            currBoxPlot = None
            allPlacedBoxPlots = HashMap.empty
            allCurrSelectedProbesIds = HashMap.empty
            selectedProbesPositions = Array.empty
            currProbeAnalyzeTime = None
            analyzeTimeProbePos = V3d.OOO
            rayActive = false
            ray = Ray3d.Invalid
            rayTriggerClicked = false
            clickPosition = None
            tvQuad = tvTrafos.Forward.TransformedPosArray(browserQuad.Points.ToArray(4)) |> Quad3d
            tvTrafo = Trafo3d.Identity
            tvToControllerTrafo = Trafo3d.Identity
            grabbingTV = false
            tvTransformations = tvTrafos
            rayColor = C4b.Red
            screenIntersection = false
            hitPoint = V3d.OOO
            screenHitPoint = V2d.OO
            screenCoordsHitPos = PixelPosition()
            holdClipping = false
            clippingActive = false
            clippingColor = C4b(1.0,1.0,0.1,0.2)
            planeCorners = Quad3d()
            planeCornersRelToHera = Quad3d()
            interesctingClippingPlane = false
            mainMenuOpen = false
            secondMenuOpen = false
            menuLevel = 0
            controllerMode = ControllerMode.NoneMode
            analyzeMode = AnalyzeMode.Region
            playbackMode = PlaybackMode.None
            currAnimationFlow = AnimationFlow.Paused
            attribute = RenderValue.NoValue
            currMainTouchPadPos = V2d.OO
            currSecondTouchPadPos = V2d.OO
            mainUntouched = false
            mainTouching = false
            secondTouching = false
            mainTouchpadTexture = allTextures.Item "initial"
            secondTouchpadTexture = allTextures.Item "initial-attributes"
            lastTouchpadModeTexture = allTextures.Item "initial"
            analyzeMainTexture = allTextures.Item "play-raw"
            mainContrScreenTexture = allTextures.Item "empty"
            secondContrScreenTexture = allTextures.Item "empty"
            lastContrScreenModeTexture = allTextures.Item "empty"
            analyzeContrScreenTexture = allTextures.Item "analyze-time-screen"
            showMainTexture = false
            showSecondTexture = false
            heraBox = Box3d.Infinite
            heraTransformations = Trafo3d(Scale3d(initialScalingHera)) * Trafo3d.Translation(0.0, 0.0, 0.7)
        }

    let rec update (runtime : IRuntime) (controllersOffler : Offler) (histogramOffler : Offler) (boxPlotOffler : Offler) (viewTrafo : aval<Trafo3d>) (projTrafo : aval<Trafo3d>) (frames : Frame[]) (state : VrState) (vr : VrActions) (model : Model) (msg : Message) =
        let mTwoD = model.twoDModel

       // printfn "HMD Pos: %A" (state.display.pose.deviceToWorld.GetModelOrigin())

        let callUpdate (msg : Message) = update runtime controllersOffler histogramOffler boxPlotOffler viewTrafo projTrafo frames state vr model msg
       
        //let currTex =
        //    match histogramClient.Texture |> AVal.force with
        //    | :? IBackendTexture as ba -> ba
        //    | _ -> failwith "no backend texture"
        
        //let t = currTex 

        ////let newTexture = new Texture(, t.Handle, t.Dimension, t.MipMapLevels, t.Samples, t.Size, t.Count, TextureFormat.Rgba, )

        //let currTexture = runtime.Download currTex

        let hmdPos = state.display.pose.deviceToWorld.GetModelOrigin()

        let controllersIds = 
            state.devices
            |> HashMap.filter (fun key deviceType -> deviceType.kind = VrDeviceKind.Controller)
            |> HashMap.toSeq
            |> Seq.map (fun value -> fst value)
            |> Seq.toArray

        let viewProjTrafoAVal = AVal.map2 (fun v p -> v * p) viewTrafo projTrafo
        let viewProj = AVal.force(viewProjTrafoAVal)
        let viewTr = AVal.force(viewTrafo)
        let projTr = AVal.force(projTrafo)
        //printf "View Trafo: %A \n" (AVal.force(viewTrafo))
       // printf "ViewProj Trafo: %A \n" (AVal.force(viewProjTrafoAVal))

        //printf "VR STATE: %A \n" state

        match msg with
        | ThreeD _ -> model
        | Nop -> model
        | TwoD msg -> 
            let sup = AardVolume.App.update frames model.twoDModel msg
            { model with twoDModel = sup }
        | StartVr -> vr.start(); model
        | SetController id ->
            if (model.mainControllerId.IsNone || model.secondControllerId.IsNone) && controllersIds.Length = 2 then
                let fstId = controllersIds.[0]
                let sndId = controllersIds.[1]
                {model with
                    mainControllerId = Some fstId
                    secondControllerId = Some sndId
                    mainContrSignTexture = texture "main-controller"}
            else
                model
        | ToggleBillboardVisibility id ->
            match model.secondControllerId with 
            | Some i when i = id ->
                {model with showBillboard = not model.showBillboard}
            | _ -> model
        | GrabHera id ->
            match model.mainControllerId with
            | Some i when i = id && not model.grabbingTV && not ((model.controllerMode = ControllerMode.Ray) && model.screenIntersection) ->
                let controlT = model.mainControllerTrafo
                let heraT = model.heraTrafo
                let controlHeraT = heraT * controlT.Inverse
                vibrate model.mainControllerId state 70.0
                {model with 
                    heraToControllerTrafo = controlHeraT
                    grabbingHera = true
                    //textureDeviceTrafo = controlT
                    mainTouchpadTexture = texture "initial-scaling"
                    mainContrScreenTexture = texture "empty"
                    menuLevel = 0
                    mainMenuOpen = false
                    secondMenuOpen = false}
            | _ -> model
        | GrabTv id ->
            match model.mainControllerId with 
            | Some i when i = id && (model.controllerMode = ControllerMode.Ray) && model.screenIntersection ->
                let controlT = model.mainControllerTrafo
                let tvT = model.tvTrafo
                let controlerTvTrafo = tvT * controlT.Inverse
                vibrate model.mainControllerId state 70.0
                {model with 
                    tvToControllerTrafo = controlerTvTrafo
                    grabbingTV = true
                    mainTouchpadTexture = texture "initial-scaling"
                    mainContrScreenTexture = texture "empty"
                    menuLevel = 0
                    mainMenuOpen = false
                    secondMenuOpen = false}
            | _ -> model
        | UngrabHera id -> 
            match model.mainControllerId with 
            | Some i when i = id && model.grabbingHera -> 
                let controlT = model.mainControllerTrafo
                let heraToControlT = model.heraToControllerTrafo
                let heraT = heraToControlT * controlT
                {model with 
                    heraTrafo = heraT
                    grabbingHera = false
                    mainContrScreenTexture = texture "empty"}
            | _ -> model
        | UngrabTv id ->
            match model.mainControllerId with 
            | Some i when i = id && model.grabbingTV -> 
                let controlT = model.mainControllerTrafo
                let tvToControlT = model.tvToControllerTrafo
                let tvTrafo = tvToControlT * controlT
                {model with 
                    tvTrafo = tvTrafo
                    grabbingTV = false
                    mainContrScreenTexture = texture "empty"}
            | _ -> model
        | ScaleHera (id, f) ->
            match model.mainControllerId with 
            | Some i when i = id && model.grabbingHera ->
                if model.mainTouching then
                    if f >= 0.0 then
                        let maxScale = 1.0
                        let currScale = model.scalingFactorHera * (f*f/5.0 + 1.0)
                        let newScale = if currScale >= maxScale then maxScale else currScale
                        {model with 
                            scalingFactorHera = newScale
                            mainTouchpadTexture = texture "scale-up"
                            mainContrScreenTexture = texture "scaling-up"}
                    else
                        let minScale = 0.001
                        let currScale = model.scalingFactorHera * (1.0 - f*f/5.0)
                        let newScale =  if currScale <= minScale then minScale else currScale
                        {model with 
                            scalingFactorHera = newScale
                            mainTouchpadTexture = texture "scale-down"
                            mainContrScreenTexture = texture "scaling-down"}
                else 
                    {model with 
                        mainTouchpadTexture = texture "initial-scaling"
                        mainContrScreenTexture = texture "empty"}
            | _ -> model
        | ScaleTv (id, f) ->
            match model.mainControllerId with 
            | Some i when i = id && model.grabbingTV ->
                if model.mainTouching then
                    if f >= 0.0 then
                        let maxScale = 6.0
                        let currScale = model.scalingFactorTv * (f*f/5.0 + 1.0)
                        let newScale = if currScale >= maxScale then maxScale else currScale
                        {model with 
                            scalingFactorTv = newScale
                            mainTouchpadTexture = texture "scale-up"
                            mainContrScreenTexture = texture "scaling-up"}
                    else
                        let minScale = 0.1
                        let currScale = model.scalingFactorTv * (1.0 - f*f/5.0)
                        let newScale =  if currScale <= minScale then minScale else currScale
                        {model with 
                            scalingFactorTv = newScale
                            mainTouchpadTexture = texture "scale-down"
                            mainContrScreenTexture = texture "scaling-down"}
                else 
                    {model with 
                        mainTouchpadTexture = texture "initial-scaling"
                        mainContrScreenTexture = texture "empty"}
            | _ -> model
        | MoveController (id, (trafo : Trafo3d)) -> 
            model
            |> updateHMDPos hmdPos
            |> updateDevicesTrafos id trafo
            |> updateSphereScalingFactor
            |> updateTvTrafos
            |> updateRay id controllersOffler
            |> updateHeraTrafos
            |> updateProbeIntersections id state
            |> updateBoxPlotIntersections id state
            |> updateClippingPlane
            |> updateSecondControllеrClippingIntersection id state
            |> updateMainControllerTextures
            |> updateSecondControllerTextures
            |> updateProbesWhenGrabbing
            |> updateBillboardsVisibility viewTr projTr
            |> updateTexturesVisibility
            |> updateTakenBoxPlot
        | ActivateControllerMode id ->
            match model.mainControllerId with 
            | Some i when i = id && not model.mainMenuOpen && model.mainContrBoxPlotIntersectionId.IsNone && not model.movingBoxPlot  -> 
                match model.controllerMode with
                | ControllerMode.Probe -> callUpdate (CreateProbe id)
                | ControllerMode.Ray -> callUpdate (CreateRay id)
                | ControllerMode.Clipping -> callUpdate (CreateClipping id)
                | _ -> model
            | _ -> model
        | CreateProbe id ->
            match model.mainContrProbeIntersectionId with // is the main controller currently intersecting a probe (id of probe is a string)
            | Some probe -> 
                let currProbe = model.allProbes.TryFind(probe)
                let currScale, probeId = 
                    match currProbe with
                    | Some pr -> (pr.radius / model.sphereRadius), Some pr.id
                    | None -> model.sphereScale, None
                let pr = model.allProbes.Item probe
                if not pr.selected && not pr.currSelected && not pr.timeAnalyze then 
                    let intId = pr.numberId

                    let newHashmap, updatedTwoDmodel = 
                        if intId <> -1 then 
                            let hm = model.boxPlotProbes.Remove(intId)
                            let dataForBoxPlot = hm |> HashMap.toSeq |> Seq.map (fun result -> snd result ) |> Seq.toArray
                            hm, { mTwoD with boxPlotData = dataForBoxPlot}
                        else 
                            model.boxPlotProbes, model.twoDModel

                    {model with 
                        createProbe = true
                        allProbes = model.allProbes.Remove(probe)
                        boxPlotProbes = newHashmap
                        lastFilterProbe = currProbe
                        lastFilterProbeId = probeId
                        currentProbeManipulated = true
                        newProbePlaced = false
                        existingProbeModified = true
                        lastModifiedProbeIntId = intId
                        holdingSphere = true
                        sphereScale = currScale
                        mainContrProbeIntersectionId = None
                        twoDModel = updatedTwoDmodel}
                else {model with createProbe = false}
            | None -> 
                {model with 
                    createProbe = true
                    currentProbeManipulated = true
                    lastModifiedProbeIntId = -1
                    newProbePlaced = false
                    holdingSphere = true}                 
        | CreateRay id ->
            let origin, direction =     
                if not model.rayActive then 
                    model.mainControllerTrafo.Forward.TransformPos(V3d(0.0, 0.02, 0.0)),
                    model.mainControllerTrafo.Forward.TransformPos(V3d.OIO * 1000.0)
                else 
                    model.ray.Origin, model.ray.Direction
            //client.Mouse.Down(model.screenCoordsHitPos, MouseButtons.Left)
            //vrMouse.Down(model.screenCoordsHitPos, MouseButtons.Left)
            controllersOffler.MouseDown(model.screenCoordsHitPos.Position.X, model.screenCoordsHitPos.Position.Y, MouseButton.Left, false, false, false)
            vibrate model.mainControllerId state 80.0
            {model with
                ray = Ray3d(origin, direction)
                rayTriggerClicked = true
                clickPosition = Some model.screenHitPoint
                rayActive = true}
        | CreateClipping id ->
            let p0 = model.mainControllerTrafo.Forward.TransformPos(planePos0)
            let p1 = model.mainControllerTrafo.Forward.TransformPos(planePos1)
            let p2 = model.mainControllerTrafo.Forward.TransformPos(planePos2)
            let p3 = model.mainControllerTrafo.Forward.TransformPos(planePos3)
            let heraInvMatrix = model.heraTransformations.Backward 
            let transformedQuad = heraInvMatrix.TransformedPosArray([|p0; p1; p2; p3|]) |> Quad3d
            {model with 
                planeCorners = Quad3d(p0, p1, p2, p3)
                planeCornersRelToHera = transformedQuad
                holdClipping = true
                clippingActive = true}
        | ScaleProbe id ->
            match model.secondControllerId with 
            | Some i when i = id -> 
                let scaling = model.holdingSphere
                {model with scalingSphere = scaling}
            | _ -> {model with scalingSphere = false}
        | StopProbeScale id ->
            match model.secondControllerId with 
            | Some i when i = id -> 
                let scaling =  model.holdingSphere && model.scalingSphere
                {model with scalingSphere = if scaling then false else model.scalingSphere}
            | _ -> model
        | DeleteProbe id ->
            match model.secondContrProbeIntersectionId with // is the controller currently intersecting a probe (id of probe is a string)
            | Some probe -> 
                match model.secondControllerId with
                | Some i when i = id && not model.holdingSphere -> 
                    let filterProbe, probeId = 
                        match model.lastFilterProbe with 
                            | Some pr when pr.id <> probe -> model.lastFilterProbe, Some pr.id
                            | _ -> None, None

                    let currProbe = model.allProbes.Item probe

                    if not currProbe.selected && not currProbe.currSelected && not currProbe.timeAnalyze then 
                        let intId = currProbe.numberId
                        let newHashmap = model.boxPlotProbes.Remove(intId)

                        let dataForBoxPlot = newHashmap |> HashMap.toSeq |> Seq.map (fun result -> snd result ) |> Seq.toArray

                        let updatedTwoDmodel = 
                            { mTwoD with
                                sphereFilter = None
                                data = VersionedArray.ofArray [||] 
                                dataRange = mTwoD.initDataRange
                                boxPlotData = dataForBoxPlot
                            }

                        {model with 
                            allProbes = model.allProbes.Remove(probe)
                            lastDeletedProbe = probe
                            boxPlotProbes = newHashmap
                            lastFilterProbe = filterProbe
                            lastFilterProbeId = probeId
                            twoDModel = updatedTwoDmodel
                            secondContrProbeIntersectionId = None} 
                    else model
                | _ -> model
            | None -> model
        | DeleteClippingPlane id ->
            match model.secondControllerId with 
            | Some i when i = id && model.interesctingClippingPlane ->
                {model with
                    planeCorners = Quad3d()
                    planeCornersRelToHera = Quad3d()
                    holdClipping = false}
            | _ -> model
        | SelectBoxPlotProbes id ->
            match model.mainControllerId with 
            | Some i when i = id && not model.mainMenuOpen && model.controllerMode = ControllerMode.Analyze && model.analyzeMode = AnalyzeMode.Region -> 
                match model.mainContrProbeIntersectionId with 
                | Some probeId when model.boxPlotFrames.IsEmpty ->
                    let intersectedProbe = model.allProbes.Item probeId
                    let isCurrSelected = intersectedProbe.currSelected

                    let arrayBoxPlot, statsBoxPlot = intersectedProbe.allData.[mTwoD.frame].Item model.currBoxPlotAttrib
                    let arrayData = RefEqual.toRef arrayBoxPlot

                    let newHashmap, allProbesIds = 
                        if not isCurrSelected then 
                            model.boxPlotProbes.Add(model.lastProbeId, arrayData), model.allCurrSelectedProbesIds.Add(model.lastProbeId, intersectedProbe)
                        else 
                            let currProbe = model.allProbes.Item probeId
                            let intId = currProbe.numberId
                            model.boxPlotProbes.Remove(intId), model.allCurrSelectedProbesIds.Remove(intId)

                    let dataForBoxPlot = newHashmap |> HashMap.toSeq |> Seq.map (fun result -> snd result ) |> Seq.toArray

                    let probesPositions = allProbesIds |> HashMap.toSeq |> Seq.map (fun result -> (snd result).id, (snd result).centerRelToHera) |> Seq.toArray

                    let updatedTwoDmodel = 
                        { mTwoD with
                            boxPlotRegion = true
                            boxPlotData = dataForBoxPlot
                            boxPlotAttribute = renderValueToString model.currBoxPlotAttrib
                        }

                    let updatedProbe = 
                        if not isCurrSelected then 
                            {intersectedProbe with
                                color = if intersectedProbe.timeAnalyze then intersectedProbe.color else C4b.Yellow
                                numberId = model.lastProbeId
                                currSelected = true
                                timesSelected = intersectedProbe.timesSelected + 1}
                        else 
                            {intersectedProbe with
                                color = if intersectedProbe.timeAnalyze then intersectedProbe.color else (if intersectedProbe.selected then C4b.Yellow else C4b.Blue)
                                numberId = -1
                                currSelected = false
                                timesSelected = intersectedProbe.timesSelected - 1}

                    let update (pr : Option<Probe>) = updatedProbe 
                    let allProbesUpdated = model.allProbes |> HashMap.update probeId update

                    {model with 
                        twoDModel = updatedTwoDmodel
                        boxPlotProbes = newHashmap
                        currBoxPlotAttribSet = true
                        allProbes = allProbesUpdated
                        allCurrSelectedProbesIds = allProbesIds
                        selectedProbesPositions = probesPositions
                        lastProbeId = model.lastProbeId + 1
                        }
                | _ -> model
            | _ -> model
        | SelectBoxPlotProbeTime id ->
            match model.mainControllerId with 
            | Some i when i = id && not model.mainMenuOpen && model.controllerMode = ControllerMode.Analyze && model.analyzeMode = AnalyzeMode.Time -> 
                match model.mainContrProbeIntersectionId with 
                | Some probeId when model.allProbes.TryFind(probeId).IsSome && model.boxPlotProbes.IsEmpty -> 
                    let currProbe = model.allProbes.Item probeId

                    let arrayBoxPlot, statsBoxPlot = currProbe.allData.[mTwoD.frame].Item model.currBoxPlotAttrib
                    let arrayData = RefEqual.toRef arrayBoxPlot

                    let newProbeAnalyzeTime, analyzeTimeProbePos, updatedProbe, newHashmap, currSelectedProbe, newOrder = 
                        if model.currProbeAnalyzeTime.IsNone then 
                            let updatedProbe = 
                                {currProbe with 
                                    timeAnalyze = true
                                    timeAnalyzeSelected = currProbe.timeAnalyzeSelected + 1
                                    color = C4b.Orange}
                            //let order = model.framesOrder |> Seq.append([mTwoD.frame])
                            let tempH = model.boxPlotFrames.Add(mTwoD.frame, arrayData)
                            let currSelected = model.allCurrSelectedProbesIds.Add(500, currProbe)
                            let newOrder = tempH |> HashMap.toSeq |> Seq.map (fun result -> fst result)
                            Some currProbe, currProbe.centerRelToHera, updatedProbe, tempH, currSelected, newOrder
                        else 
                            if model.currProbeAnalyzeTime.Value.id = probeId then 
                                let updatedProbe = 
                                    {currProbe with 
                                        timeAnalyze = false
                                        timeAnalyzeSelected = currProbe.timeAnalyzeSelected - 1
                                        color = if currProbe.timesSelected >= 1 then C4b.Yellow else C4b.Blue}
                                None, V3d.OOO, updatedProbe, HashMap.empty, HashMap.empty, Seq.empty
                            else 
                                model.currProbeAnalyzeTime, model.analyzeTimeProbePos, currProbe, model.boxPlotFrames, model.allCurrSelectedProbesIds, model.framesOrder

                    let dataForBoxPlot = newHashmap |> HashMap.toSeq |> Seq.map (fun result -> snd result ) |> Seq.toArray

                    let updatedTwoDmodel = 
                        { mTwoD with
                            boxPlotRegion = false
                            framesOrder = newOrder |> Seq.toArray
                            boxPlotData = dataForBoxPlot
                            boxPlotAttribute = renderValueToString model.currBoxPlotAttrib
                        }
                    let update (pr : Option<Probe>) = updatedProbe 
                    let allProbesUpdated = model.allProbes |> HashMap.update probeId update
                    {model with
                        boxPlotFrames = newHashmap
                        framesOrder = newOrder
                        currProbeAnalyzeTime = newProbeAnalyzeTime
                        analyzeTimeProbePos = analyzeTimeProbePos
                        allCurrSelectedProbesIds = currSelectedProbe
                        allProbes = allProbesUpdated
                        twoDModel = updatedTwoDmodel}
                | _ -> model
            | _ -> model
        | PlaceBoxPlot id ->
            match model.secondControllerId with 
            | Some i when i = id && model.controllerMode = ControllerMode.Analyze && model.secondContrBoxPlotIntersectionId.IsNone && 
                (not model.boxPlotProbes.IsEmpty || not model.boxPlotFrames.IsEmpty) ->
                let texture = getScreenshot boxPlotOffler
                let texCopy = texture.CopyToPixImage()
                //texture.SaveAsImage(sprintf @"C:\Users\vasileva\source\image\temp.png")
                let currTrafo = defaultBoxPlotsTrafo * model.secondControllerTrafo
                let transformedQuad = currTrafo.Forward.TransformedPosArray(defaultBoxPlotPositions.Points.ToArray(4)) |> Quad3d
                let isRegion, currBoxPlotData, probePos = 
                    if not model.boxPlotProbes.IsEmpty then true, model.boxPlotProbes, V3d.OOO
                    else if not model.boxPlotFrames.IsEmpty then false, model.boxPlotFrames, model.currProbeAnalyzeTime.Value.centerRelToHera
                    else true, HashMap.empty, V3d.OOO
                let boxPlot = createBoxPlot isRegion model.currBoxPlotAttrib currTrafo transformedQuad 
                                    texCopy currBoxPlotData model.allCurrSelectedProbesIds 
                                    model.twoDModel.allProbesScreenPositions model.selectedProbesPositions probePos true
                let allPlacedBoxPlots = model.allPlacedBoxPlots.Add(boxPlot.id, boxPlot)
                let updatedTwoDmodel = 
                    {mTwoD with 
                        framesOrder = Array.empty
                        boxPlotData = [| |]
                        boxPlotAttribute = "Select probes with main controller!" }
                let allProbesUpdated = 
                    model.allProbes 
                    |> HashMap.map (fun key probe ->
                        {probe with 
                            color = if probe.currSelected && not probe.timeAnalyze then C4b.Yellow else probe.color
                            numberId = -1
                            selected = if probe.currSelected then true else probe.selected
                            currSelected = false})
                //allPlacedBoxPlots
                //|> HashMap.iter(fun k v ->
                //        v.texture.SaveAsImage(sprintf @"C:\Users\vasileva\source\image\temp%s.png" k)
                //    )
                {model with 
                    allProbes = allProbesUpdated
                    allPlacedBoxPlots = allPlacedBoxPlots
                    framesOrder = Seq.empty
                    boxPlotProbes = HashMap.empty
                    boxPlotFrames = HashMap.empty
                    allCurrSelectedProbesIds = HashMap.empty
                    selectedProbesPositions = Array.empty
                    currProbeAnalyzeTime = None
                    twoDModel = updatedTwoDmodel}
            | _ -> model
        | DeleteBoxPlot id ->
            match model.secondControllerId with 
            | Some i when i = id && model.secondContrBoxPlotIntersectionId.IsSome ->
                let intersectedBoxPlotId = model.secondContrBoxPlotIntersectionId.Value
                let currBoxPlot = model.allPlacedBoxPlots.Item intersectedBoxPlotId
                let allProbesUpdated = 
                    model.allProbes 
                    |> HashMap.map (fun key probe ->
                        let probeInBoxPlot = 
                            currBoxPlot.probeIds
                            |> HashMap.exists (fun numberId selProbe -> 
                                selProbe.id = key
                            ) 
                        if probeInBoxPlot then 
                            if currBoxPlot.isRegion then 
                                if probe.timesSelected >= 2 then    
                                    {probe with   
                                        selected = false
                                        //currSelected = false
                                        timesSelected = probe.timesSelected - 1}
                                else 
                                    {probe with 
                                        color = if probe.timeAnalyze then C4b.Orange else C4b.Blue
                                        selected = false
                                        currSelected = false
                                        timesSelected = 0}
                            else 
                                if probe.timeAnalyzeSelected >= 2 then
                                    {probe with 
                                        timeAnalyzeSelected = probe.timeAnalyzeSelected - 1}
                                else 
                                    {probe with 
                                        timeAnalyze = false 
                                        color = if probe.timesSelected >= 1 then C4b.Yellow else C4b.Blue
                                        timeAnalyzeSelected = 0}                        
                        else 
                            probe)
                let updateBoxPlots = model.allPlacedBoxPlots.Remove(intersectedBoxPlotId)
                {model with 
                    allPlacedBoxPlots = updateBoxPlots
                    allProbes = allProbesUpdated}
            | _ -> model
        | ToggleVisualLinks id -> 
            match model.mainControllerId with 
            | Some i when i = id && model.mainContrBoxPlotIntersectionId.IsSome -> 
                let intersectedBoxPlotId = model.mainContrBoxPlotIntersectionId.Value
                let boxPlot = model.allPlacedBoxPlots.TryFind(intersectedBoxPlotId)
                match boxPlot with 
                | Some b ->
                    let updatedBoxPlot = {b with showVisualLinks = not b.showVisualLinks}
                    let update (bp : Option<BoxPlot>) = updatedBoxPlot 
                    let allBpsUpdated = model.allPlacedBoxPlots |> HashMap.update intersectedBoxPlotId update
                    {model with allPlacedBoxPlots = allBpsUpdated}
                | None -> model 
            | _ -> model    
        | CopyBoxPlot id ->
            match model.secondControllerId with 
            | Some i when i = id && model.secondContrBoxPlotIntersectionId.IsSome ->
                let intersectedBoxPlotId = model.secondContrBoxPlotIntersectionId.Value
                let boxPlot = model.allPlacedBoxPlots.TryFind(intersectedBoxPlotId)
                match boxPlot with
                | Some b when b.isRegion -> 
                    let attrib = b.attribute
                    let dataForBoxPlot = b.data |> HashMap.toSeq |> Seq.map (fun result -> snd result ) |> Seq.toArray
                    let allSelected = b.probeIds

                    let allProbesUpdated = 
                        model.allProbes
                        |> HashMap.map (fun key probe ->
                            let probeSelected = 
                                allSelected
                                |> HashMap.filter (fun numberId selProbe -> 
                                    selProbe.id = key
                                ) 

                            if not probeSelected.IsEmpty then 
                                {probe with 
                                    numberId = probeSelected |> HashMap.toSeq |> Seq.exactlyOne |> fst
                                    timesSelected = probe.timesSelected + 1
                                    currSelected = true}
                            else 
                                {probe with 
                                    numberId = -1
                                    currSelected = false})

                    let probesPositions = allSelected |> HashMap.toSeq |> Seq.map (fun result -> (snd result).id, (snd result).centerRelToHera) |> Seq.toArray
                    
                    let updatedTwoDmodel = 
                        { mTwoD with
                            boxPlotRegion = b.isRegion
                            boxPlotData = dataForBoxPlot
                            boxPlotAttribute = renderValueToString attrib
                        }

                    {model with 
                        twoDModel = updatedTwoDmodel
                        boxPlotProbes = b.data
                        currBoxPlotAttrib = attrib
                        allCurrSelectedProbesIds = allSelected
                        selectedProbesPositions = probesPositions
                        allProbes = allProbesUpdated}
                | _ -> model 
            | _ -> model
        | TakeBoxPlot id ->
            match model.mainControllerId with 
            | Some i when i = id && model.mainContrBoxPlotIntersectionId.IsSome  ->
                let intersectedBoxPlotId = model.mainContrBoxPlotIntersectionId.Value
                let boxPlot = model.allPlacedBoxPlots.TryFind(intersectedBoxPlotId)
                //model.allPlacedBoxPlots
                //|> HashMap.iter(fun k v ->
                //        v.texture.SaveAsImage(sprintf @"C:\Users\vasileva\source\image\temp%s.png" k)
                //    )
                match boxPlot with
                | Some b -> 
                    //boxPlot.Value.texture.SaveAsImage(sprintf @"C:\Users\vasileva\source\image\temp.png")
                    let currTrafo = defaultBoxPlotsTrafo * model.mainControllerTrafo
                    let updatedB = {b with trafo = currTrafo}
                    let updateBoxPlots = model.allPlacedBoxPlots.Remove(intersectedBoxPlotId)
                    vibrate model.mainControllerId state 50.0
                    {model with 
                        movingBoxPlot = true
                        allPlacedBoxPlots = updateBoxPlots
                        takenBoxPlot = Some updatedB
                        takenBPTex = b.texture}
                | None -> model 
            | _ -> model
        | LeaveBoxPlot id ->
            match model.mainControllerId with 
            | Some i when i = id && model.movingBoxPlot ->
                let currTakenBoxPlot = model.takenBoxPlot.Value
                let currTrafo = currTakenBoxPlot.trafo
                let transformedQuad = currTrafo.Forward.TransformedPosArray(defaultBoxPlotPositions.Points.ToArray(4)) |> Quad3d
                let updatedBoxPlot = {currTakenBoxPlot with positions = transformedQuad}
                let updateBoxPlots = model.allPlacedBoxPlots.Add(updatedBoxPlot.id, updatedBoxPlot)
                vibrate model.mainControllerId state 50.0
                {model with 
                    movingBoxPlot = false
                    allPlacedBoxPlots = updateBoxPlots
                    takenBoxPlot = None}
            | _ -> model
        | DeactivateControllerMode id ->
            if not model.mainMenuOpen && not model.movingBoxPlot then 
                match model.controllerMode with 
                | ControllerMode.Probe -> 
                    match model.mainControllerId  with
                    | Some i when i = id && model.createProbe -> 
                        let t = model.mainControllerTrafo
                        let heraInvMatrix = model.heraTransformations.Backward

                        let spherePos = t.Forward.TransformPos(V3d.OOO)
                        let spherePosTransformed = heraInvMatrix.TransformPos(spherePos)
                        let sphereRadius = model.sphereRadius * model.sphereScale

                        let heraScale = heraInvMatrix.GetScaleVector3()
                        let radiusTransformed = sphereRadius * heraScale.X

                        let sphere = Sphere3d(spherePos, sphereRadius)
                        let sphereTransformed = Sphere3d(spherePosTransformed, radiusTransformed)

                        //TODO: use only the relative Pos and Radius to spare code and storage
                        let intersection = model.heraBox.Intersects(sphere)
                                    
                        let pl = mTwoD.clippingPlane
                        let plane = V3d(pl.x, pl.y, pl.z)

                        let pc = model.planeCorners
                        let corner1 = heraInvMatrix.TransformPos(pc.P0)
                        let corner2 = heraInvMatrix.TransformPos(pc.P1)
                        let corner3 = heraInvMatrix.TransformPos(pc.P2)
                        let clippingPlane = Plane3d(corner1, corner2, corner3)

                        let discardProperties : DiscardProperties = 
                            {
                                plane = plane
                                controllerPlane = clippingPlane
                            }

                        let allData = filterAllDataForAllFramesSphere frames (Some sphereTransformed) discardProperties
                        let attrib, billboardType = 
                            if model.existingProbeModified then 
                                let lastProbe = model.lastFilterProbe.Value
                                let lastAttrib =  lastProbe.currAttribute
                                let lastBillboardType = lastProbe.currBillboard
                                lastAttrib, lastBillboardType
                            else 
                                let att = if model.attribute = RenderValue.NoValue then RenderValue.Energy else model.attribute
                                att, BillboardType.Histogram

                        let array, stats = allData.[mTwoD.frame].Item attrib

                        //let currSelected = model.lastModifiedProbeIntId <> -1 
                        let color = if intersection then C4b.Blue else C4b.White
                        let probe = createProbe model.lastModifiedProbeIntId false false 0 false 0 color spherePos sphereRadius spherePosTransformed radiusTransformed intersection allData attrib true true billboardType V3d.OOO

                        let filteredData = if intersection then array else mTwoD.data.arr
                        let attributeAsString = renderValueToString attrib

                        //let newCurrBoxPlotAttrib = if not model.currBoxPlotAttribSet then attrib else model.currBoxPlotAttrib
                        //let newHashmap, dataForBoxPlot = 
                        //    if model.existingProbeModified && selected then 
                        //        let arrayBoxPlot, statsBoxPlot = allData.Item model.currBoxPlotAttrib
                        //        let hm = model.boxPlotProbes.Add(model.lastModifiedProbeIntId, arrayBoxPlot) 
                        //        let boxPlotData = hm |> HashMap.toSeq |> Seq.map (fun result -> snd result ) |> Seq.toArray
                        //        hm, boxPlotData
                        //    else 
                        //        model.boxPlotProbes, mTwoD.boxPlotData

                        let updatedTwoDmodel = 
                            { mTwoD with
                                sphereFilter = Some sphereTransformed
                                data = { version = mTwoD.data.version + 1; arr = filteredData}
                                attributeText = attributeAsString
                                //boxPlotData = dataForBoxPlot
                                //boxPlotAttribute = renderValueToString newCurrBoxPlotAttrib
                            }

                        let sleepTime = computeSleepTime model.firstHistogram filteredData.Length

                        let threadsVr = 
                            proclist { 
                                do! Async.SwitchToThreadPool()
                                do! Async.Sleep sleepTime
                                // perform readback
                                let t = getScreenshot histogramOffler
                                let texCopy = t.CopyToPixImage()
                                yield SetTexture(texCopy, probe)
                            }

                        {model with
                            currentProbeManipulated = false
                            allProbes = model.allProbes.Add(probe.id, probe)
                            lastSphereScale = model.sphereScale
                            lastFilterProbe = Some probe
                            lastFilterProbeId = Some probe.id
                            newProbePlaced = true
                            firstHistogram = false
                            existingProbeModified = false
                            holdingSphere = false
                            sphereScale = 1.0
                            //boxPlotProbes = newHashmap
                            //currBoxPlotAttribSet = true
                            //currBoxPlotAttrib = newCurrBoxPlotAttrib
                            twoDModel = updatedTwoDmodel
                            threads = ThreadPool.start threadsVr ThreadPool.empty}
                    | _ -> model 
                | ControllerMode.Ray ->     
                    match model.mainControllerId with
                    | Some i when i = id ->
                        //client.Mouse.Up(model.screenCoordsHitPos, MouseButtons.Left)
                        //vrMouse.Up(model.screenCoordsHitPos, MouseButtons.Left)
                        let xPos = model.screenCoordsHitPos.Position.X
                        let yPos = model.screenCoordsHitPos.Position.Y
                        //printfn "xPos: %A" xPos
                        //printfn "yPos: %A" yPos
                        controllersOffler.MouseUp(xPos, yPos, MouseButton.Left, false, false, false)
                        if xPos < 650 || xPos > 1150 || yPos < 455 || yPos > 700 then
                            controllersOffler.MouseClick(xPos, yPos, MouseButton.Left, 1, false, false, false)
                        {model with 
                            rayTriggerClicked = false
                            clickPosition = None}
                    | _ -> model
                | ControllerMode.Clipping -> 
                    match model.mainControllerId with 
                    | Some i when i = id -> {model with holdClipping = false}
                    | _ -> model
                | _ -> model
            else 
                model 
        | ToggleMainMenu id -> 
            //let r, theta = convertCartesianToPolar model.currTouchPadPos
            let level, isOpen = 
                let closeMenu = 0, false
                let goToNextMenu = (model.menuLevel + 1), true
                if model.mainContrProbeIntersectionId.IsNone && not model.grabbingHera then 
                    match model.mainControllerId with 
                    | Some i when i = id ->
                        match model.menuLevel with
                        | 0 -> goToNextMenu
                        | 1 -> if model.controllerMode = ControllerMode.Analyze then goToNextMenu else closeMenu
                        | 2 -> closeMenu
                        | _ -> closeMenu
                    | _ -> model.menuLevel, model.mainMenuOpen
                else 
                    closeMenu
            let screenTex = if not isOpen then model.lastContrScreenModeTexture else model.mainContrScreenTexture
            {model with 
                menuLevel = level
                mainMenuOpen = isOpen
                showCurrBoxPlot = ((not isOpen) && model.controllerMode = ControllerMode.Analyze)}
        | OpenMainMenu id ->
            let texture, screenTexture =
                match model.menuLevel with
                | l when l = 1 -> if model.controllerMode = ControllerMode.NoneMode then (texture "initial"), (texture "select-tool") else model.lastTouchpadModeTexture, model.lastContrScreenModeTexture
                | l when l = 2 -> (texture "analyze-region"), (texture "analyze-region-screen") //TODO: will be changed
                | _ -> model.mainTouchpadTexture, model.mainContrScreenTexture
            if model.mainMenuOpen then
                {model with 
                    mainTouchpadTexture = texture
                    mainContrScreenTexture = screenTexture}
            else    
                model
        | ChangeMainControllerMode id -> 
            match model.mainControllerId with  
            | Some i when i = id && model.mainMenuOpen && model.menuLevel = 1 && not model.grabbingHera && model.mainContrProbeIntersectionId.IsNone -> 
                let r, theta = convertCartesianToPolar model.currMainTouchPadPos
                let newContrlMode = 
                    if r >= 0.5 then 
                        match theta with 
                        | t when (t >= 0.0   && t < 45.0) || (t >= 315.0 && t < 360.0)  -> ControllerMode.Clipping
                        | t when t >= 45.0  && t < 135.0 -> ControllerMode.Ray
                        | t when t >= 135.0 && t < 225.0 -> ControllerMode.Probe
                        | t when t >= 225.0 && t < 315.0 -> ControllerMode.Analyze
                        | _ -> model.controllerMode
                    else 
                        model.controllerMode
                if newContrlMode <> model.controllerMode then vibrate model.mainControllerId state 50.0
                let tex, screenTexture = 
                    match model.controllerMode with 
                    | m when m = newContrlMode -> model.mainTouchpadTexture, model.mainContrScreenTexture // if the  controller mode is the same then texture should not be loaded again   
                    | _ ->
                        let texName, screenTexName = 
                            match newContrlMode with
                            | ControllerMode.Probe -> "left", "probe" 
                            | ControllerMode.Ray -> "middle", "ray" 
                            | ControllerMode.Clipping -> "right", "clipping"
                            | ControllerMode.Analyze -> "analyze", "analyze-probes"
                            | _ -> "initial", "select-tool"
                        (texture texName), (texture screenTexName)
                {model with 
                    mainUntouched = false
                    playbackMode = PlaybackMode.None
                    controllerMode = newContrlMode
                    mainTouchpadTexture = tex
                    lastTouchpadModeTexture = tex
                    mainContrScreenTexture = screenTexture
                    lastContrScreenModeTexture = screenTexture
                    rayActive = false
                    clippingActive = false}
            | _ -> model
        | ChangeAnalyzeMode id ->
            match model.mainControllerId with 
            | Some i when i = id && model.mainMenuOpen && model.menuLevel = 2 && model.mainTouching && not model.grabbingHera && model.mainContrProbeIntersectionId.IsNone ->
                let r, theta = convertCartesianToPolar model.currMainTouchPadPos
                let newAnalyzeMode = 
                    match theta with 
                    | t when (t >= 0.0 && t < 90.0) || (t >= 270.0 && t < 360.0) -> AnalyzeMode.Time
                    | t when t >= 90.0 && t < 270.0 -> AnalyzeMode.Region
                    | _ -> model.analyzeMode
                if newAnalyzeMode <> model.analyzeMode then vibrate model.mainControllerId state 50.0
                let tex, screenTexture, region = 
                    match model.analyzeMode with 
                    | m when m = newAnalyzeMode -> model.mainTouchpadTexture, model.mainContrScreenTexture, mTwoD.boxPlotRegion // if the  controller mode is the same then texture should not be loaded again   
                    | _ ->
                        let texName, screenTexName, r = 
                            match newAnalyzeMode with
                            | AnalyzeMode.Region -> "analyze-region", "analyze-region-screen", true 
                            | AnalyzeMode.Time -> "analyze-time", "analyze-time-screen", false 
                            | _ -> "analyze-region", "analyze-region-screen", true
                        (texture texName), (texture screenTexName), r
                let updatedTwoDModel =
                    {mTwoD with
                        boxPlotRegion = region}
                {model with 
                    analyzeMode = newAnalyzeMode
                    mainTouchpadTexture = tex
                    mainContrScreenTexture = screenTexture
                    lastContrScreenModeTexture = screenTexture
                    twoDModel = updatedTwoDModel}
            | _ -> model
        | ChangeAnimationPlayback id ->
            match model.mainControllerId with 
            | Some i when i = id && model.mainUntouched && not model.mainMenuOpen && model.mainTouching && model.controllerMode = ControllerMode.Analyze && 
                model.analyzeMode = AnalyzeMode.Time && not model.grabbingHera && model.mainContrProbeIntersectionId.IsNone ->
                let r, theta = convertCartesianToPolar model.currMainTouchPadPos
                let newPlaybackMode = 
                    if r >= 0.4 then 
                        match theta with 
                        | t when (t >= 0.0 && t < 45.0) || (t >= 315.0 && t < 360.0)  -> PlaybackMode.Forward
                        | t when t >= 45.0  && t < 135.0 -> PlaybackMode.Screenshot
                        | t when t >= 135.0 && t < 225.0 -> PlaybackMode.Backward
                        | t when t >= 225.0 && t < 315.0 -> PlaybackMode.Stop
                        | _ -> PlaybackMode.None
                    else 
                        PlaybackMode.AnimationFlow

                let tex, screenTexture = 
                    //match model.analyzeMode with 
                    //| m when m = newAnalyzeMode -> model.mainTouchpadTexture, model.mainContrScreenTexture // if the  controller mode is the same then texture should not be loaded again   
                    //| _ ->
                        let texName, screenTexName = 
                            match model.currAnimationFlow with
                            | AnimationFlow.Playing ->
                                match newPlaybackMode with 
                                | PlaybackMode.Forward -> "pause-forward" ,"analyze-time-screen" 
                                | PlaybackMode.Backward -> "pause-backward" ,"analyze-time-screen" 
                                | PlaybackMode.Stop -> "pause-stop" ,"analyze-time-screen" 
                                | PlaybackMode.Screenshot -> "pause-screenshot" ,"analyze-time-screen" 
                                | PlaybackMode.AnimationFlow -> "pause-pause" ,"analyze-time-screen" 
                                | PlaybackMode.None -> "pause-raw" ,"analyze-time-screen" 
                                | _ -> "pause-raw" ,"analyze-time-screen" 
                            | AnimationFlow.Paused -> 
                                match newPlaybackMode with 
                                | PlaybackMode.Forward -> "play-forward" ,"analyze-time-screen" 
                                | PlaybackMode.Backward -> "play-backward" ,"analyze-time-screen" 
                                | PlaybackMode.Stop -> "play-stop" ,"analyze-time-screen" 
                                | PlaybackMode.Screenshot -> "play-screenshot" ,"analyze-time-screen" 
                                | PlaybackMode.AnimationFlow -> "play-play" ,"analyze-time-screen" 
                                | PlaybackMode.None -> "play-raw" ,"analyze-time-screen" 
                                | _ -> "play-raw" ,"analyze-time-screen" 
                            | _ -> "play-raw" ,"analyze-time-screen"  
                        (texture texName), (texture screenTexName)
                {model with 
                    playbackMode = newPlaybackMode
                    //currAnimationFlow = newAnimationFlow
                    mainTouchpadTexture = tex
                    mainContrScreenTexture = screenTexture
                    analyzeMainTexture = tex
                    analyzeContrScreenTexture = screenTexture
                    }
            | _ -> model
        | ChangeBillboard id ->
            match model.mainContrProbeIntersectionId with
            | Some probeId when not model.mainMenuOpen && not model.grabbingHera ->
                match model.mainControllerId with 
                | Some i when i = id && model.mainTouching ->
                    let r, theta = convertCartesianToPolar model.currMainTouchPadPos
                    let intersectedProbe = model.allProbes.Item probeId
                    let newBillboardType = 
                        match theta with 
                        | t when (t >= 0.0 && t < 90.0) || (t >= 270.0 && t < 360.0) -> BillboardType.Histogram
                        | t when t >= 90.0 && t < 270.0 -> BillboardType.Statistic
                        | _ -> intersectedProbe.currBillboard
                    if newBillboardType <> intersectedProbe.currBillboard then vibrate model.mainControllerId state 50.0
                    let updatedProbe = {intersectedProbe with currBillboard = newBillboardType}
                    let update (pr : Option<Probe>) = updatedProbe 
                    let allProbesUpdated = model.allProbes |> HashMap.update probeId update
                    let texture, screenTexture =    
                        let texName, screenTexName =    
                            match newBillboardType with
                            | BillboardType.Histogram -> "histogram-selected", "histogram"
                            | BillboardType.Statistic -> "statistics-selected", "statistics"
                            | _ -> "histogram-selected", "histogram"
                        (texture texName), (texture screenTexName)
                    {model with 
                        allProbes = allProbesUpdated
                        mainTouchpadTexture = texture
                        mainContrScreenTexture = screenTexture}
                        //showTexture = true}
                | _ -> model
            | _ -> model
        | ChangeBoxPlotAttribute id ->
            match model.secondControllerId with  
            | Some i when i = id && model.controllerMode = ControllerMode.Analyze ->
                let newAttribute = computeNewAttribute model.currSecondTouchPadPos model.currBoxPlotAttrib model.secondControllerId state
                let texture, screenTexture = computeNewAttributeTextures newAttribute

                if newAttribute <> model.currBoxPlotAttrib then 

                    let newHashmap, newModel = 
                        if model.currProbeAnalyzeTime.IsSome then
                            let currProbe = model.currProbeAnalyzeTime.Value
                            let bp = 
                                model.boxPlotFrames
                                |> HashMap.map (fun frame array ->
                                    let newArray, newStats = currProbe.allData.[frame].Item newAttribute
                                    let arrayData = RefEqual.toRef newArray
                                    arrayData
                                )
                            bp, {model with boxPlotFrames = bp}
                        else 
                            let bp = 
                                model.boxPlotProbes
                                |> HashMap.map (fun key array ->
                                    //printfn "Box Plot Probe Key: %A" key
                                    let currProbe = 
                                        model.allProbes
                                        |> HashMap.filter (fun k probe -> 
                                            //printfn "Curr Probe Key: %A" probe.numberId
                                            probe.currSelected && probe.numberId = key
                                        )
                                        |> HashMap.toSeq
                                        |> Seq.exactlyOne
                                        |> snd
                                    let newArray, newStats = currProbe.allData.[mTwoD.frame].Item newAttribute
                                    let arrayData = RefEqual.toRef newArray
                                    arrayData                                
                                )
                            bp, {model with boxPlotProbes = bp}

                    let dataForBoxPlot = newHashmap |> HashMap.toSeq |> Seq.map (fun result -> snd result ) |> Seq.toArray

                    let updatedTwoDmodel = 
                        { mTwoD with
                            boxPlotData = dataForBoxPlot
                            boxPlotAttribute = renderValueToString newAttribute
                        }

                    {newModel with 
                        twoDModel = updatedTwoDmodel
                        currBoxPlotAttrib = newAttribute
                        currBoxPlotAttribSet = true
                        showSecondTexture = true
                        secondTouchpadTexture = texture
                        secondContrScreenTexture = screenTexture}
                else 
                    model
            | _ -> model
        | SelectGlobalAttribute id ->
            match model.secondControllerId with  
            | Some i when i = id && model.controllerMode = ControllerMode.Probe && not model.grabbingHera && model.mainContrProbeIntersectionId.IsNone -> 
                let newAttribute = computeNewAttribute model.currSecondTouchPadPos model.attribute model.secondControllerId state
                let texture, screenTexture = computeNewAttributeTextures newAttribute //model.attribute
                {model with 
                    attribute = newAttribute
                    showSecondTexture = true
                    secondTouchpadTexture = texture
                    secondContrScreenTexture = screenTexture}
            | _ -> model
        | SelectProbeAttribute id ->
            match model.secondControllerId with 
            | Some i when i = id && model.mainContrProbeIntersectionId.IsSome -> //&& model.changeProbeAttribute ->
                match model.mainContrProbeIntersectionId with
                | Some probeId ->
                    let intersectedProbe = model.allProbes.TryFind(probeId)
                    if intersectedProbe.IsSome then 
                        let probe = intersectedProbe.Value
                        let newAttribute = computeNewAttribute model.currSecondTouchPadPos probe.currAttribute model.secondControllerId state
                        let texture, screenTexture = computeNewAttributeTextures newAttribute //probe.currAttribute
                        let updatedProbe = {probe with currAttribute = newAttribute}
                        let update (pr : Option<Probe>) = updatedProbe 
                        let allProbesUpdated = model.allProbes |> HashMap.update probeId update
                        let filteredData, stats = probe.allData.[mTwoD.frame].Item newAttribute

                        let attributeAsText = renderValueToString newAttribute

                        let updatedTwoDmodel = 
                            { mTwoD with 
                                data = {version = mTwoD.data.version + 1; arr = filteredData}
                                attributeText = attributeAsText
                                }

                        let sleepTime = computeSleepTime model.firstHistogram filteredData.Length

                        let threadsVr = 
                            proclist { 
                                do! Async.SwitchToThreadPool()
                                do! Async.Sleep sleepTime
                                // perform readback
                                let t = getScreenshot histogramOffler
                                let texCopy = t.CopyToPixImage()
                                yield SetTexture(texCopy, updatedProbe)
                            }

                        {model with 
                            allProbes = allProbesUpdated
                            secondTouchpadTexture = texture
                            secondContrScreenTexture = screenTexture
                            twoDModel = updatedTwoDmodel
                            threads = ThreadPool.start threadsVr ThreadPool.empty}
                    else 
                        model
                | None -> model
            | _ -> model
        | ChangeTouchpadPos (id, pos) -> 
            match model.mainControllerId with 
            | Some i when i = id && model.rayActive && model.screenIntersection && not model.grabbingHera && not model.grabbingTV -> 
                //client.Mouse.Scroll(model.screenCoordsHitPos, pos.Y * 50.0)
                //vrMouse.Scroll(model.screenCoordsHitPos, pos.Y * 50.0)
                //controllersOffler.Value.MouseWheel(model.screenCoordsHitPos.Position.X, model.screenCoordsHitPos.Position.Y, MouseButton.Left, false, false, false)
                controllersOffler.MouseWheel(model.screenCoordsHitPos.Position.X, model.screenCoordsHitPos.Position.Y, pos.X * 50.0, pos.Y * 50.0, false, false, false)
                vibrate model.mainControllerId state 5.0
            | _ -> ()

            //when both X and Y are equal to 0.0 it means we are currently not touching the touchpad
            let mainPos, mainTouching =
                match model.mainControllerId with
                | Some i when i = id -> pos, pos <> V2d.OO
                | _ -> model.currMainTouchPadPos, model.mainTouching

            let secondPos, secondTouching =     
                match model.secondControllerId with
                | Some i when i = id -> pos, pos <> V2d.OO
                | _ -> model.currSecondTouchPadPos, model.secondTouching
            {model with 
                currMainTouchPadPos = mainPos
                currSecondTouchPadPos = secondPos
                mainTouching = mainTouching
                secondTouching = secondTouching}
        | SetTexture (t, p) ->
            let probeHistogramsUpdated = 
                model.allProbes 
                |> HashMap.map (fun key probe -> 
                    let newHistogramTexture = 
                        if p.id = key then
                            Some t
                        else 
                            probe.currHistogram
                    { probe with currHistogram = newHistogramTexture}
                )
            {model with allProbes = probeHistogramsUpdated}
        | TouchDevice id ->
            //let mainTouching = 
            //    match model.mainControllerId with
            //    | Some i when i = id -> true
            //    | _ -> model.mainTouching
            //let secondTouching = 
            //    match model.secondControllerId with
            //    | Some i when i = id -> true
            //    | _ -> model.secondTouching

            match model.mainControllerId with
            | Some i when i = id && not model.mainMenuOpen && model.controllerMode = ControllerMode.Analyze && model.analyzeMode = AnalyzeMode.Time && not model.grabbingHera && model.mainContrProbeIntersectionId.IsNone -> 
                vibrate model.mainControllerId state 50.0
            | _ -> ()
            model
            //{model with 
            //    mainTouching = mainTouching
            //    secondTouching = secondTouching}
             //{model with touchpadDeviceId = Some id} 
        | UntouchDevice id ->
            //printf "Untouch"
            let untouched = 
                match model.mainControllerId with
                | Some i when i = id && not model.mainMenuOpen && model.controllerMode = ControllerMode.Analyze && model.analyzeMode = AnalyzeMode.Time && model.mainContrProbeIntersectionId.IsNone -> true
                | _ -> model.mainUntouched
            let newModel, animFlow, play, frame, reversedAnim, offsetTimeId = 
                match model.mainControllerId with
                | Some i when i = id && untouched && not model.mainMenuOpen && model.controllerMode = ControllerMode.Analyze && model.analyzeMode = AnalyzeMode.Time && not model.grabbingHera && model.mainContrProbeIntersectionId.IsNone -> 
                    let newAnimationFlow, playAnim =
                        if model.playbackMode = PlaybackMode.AnimationFlow then 
                            match model.currAnimationFlow with
                            | AnimationFlow.Playing ->  sw.Stop(); AnimationFlow.Paused, false 
                            | AnimationFlow.Paused -> sw.Start(); AnimationFlow.Playing, true
                            | _ -> AnimationFlow.Paused, false 
                        else if model.playbackMode = PlaybackMode.Stop then 
                            sw.Stop(); sw.Reset(); AnimationFlow.Paused, false     
                        else
                            model.currAnimationFlow, mTwoD.playAnimation
                    let currFrameId = mTwoD.frameId
                    let allF = frames.Length
                    let offsetTime = mTwoD.offsetId
                    let newMod, newFrameId, reverse, offset = 
                        match newAnimationFlow with
                        | AnimationFlow.Playing ->
                            match model.playbackMode with 
                            | PlaybackMode.Forward -> sw.Restart(); model, currFrameId, false, mTwoD.frame 
                            | PlaybackMode.Backward -> sw.Restart(); model, currFrameId, true, (if mTwoD.frame = 0 then 0 else allF - mTwoD.frame)
                            | PlaybackMode.Stop -> model, 0, false, 0 
                            | PlaybackMode.Screenshot -> 
                                let newM = takeFrameScreenshot mTwoD.frame model
                                newM, currFrameId, false, offsetTime
                            | PlaybackMode.AnimationFlow -> model, currFrameId, false, offsetTime
                            | PlaybackMode.None -> model, currFrameId, false, offsetTime
                            | _ -> model, 0, false, offsetTime 
                        | AnimationFlow.Paused -> 
                            match  model.playbackMode with 
                            | PlaybackMode.Forward -> model, (currFrameId + 1) % allF, false, (offsetTime + 1)
                            | PlaybackMode.Backward -> model, (if mTwoD.frame = 0 then allF - 1 else currFrameId - 1 ), false, (offsetTime - 1) 
                            | PlaybackMode.Stop -> model, 0, false, 0
                            | PlaybackMode.Screenshot -> 
                                let newM = takeFrameScreenshot mTwoD.frame  model
                                newM, currFrameId, false, offsetTime
                            | PlaybackMode.AnimationFlow -> model, currFrameId, false, offsetTime 
                            | PlaybackMode.None -> model, currFrameId, false, offsetTime 
                            | _ -> model, 0, false, offsetTime
                        | _ -> model, 0, false, offsetTime 
                    let texName, screenTexName = 
                        match newAnimationFlow with
                        | AnimationFlow.Playing -> "pause-raw" ,"analyze-time-screen" 
                        | AnimationFlow.Paused -> "play-raw" ,"analyze-time-screen" 
                        | _ -> "play-raw" ,"analyze-time-screen" 
                    let updatedModel = 
                        {newMod with 
                            analyzeMainTexture = (texture texName)
                            analyzeContrScreenTexture = (texture screenTexName)}
                    updatedModel, newAnimationFlow, playAnim, newFrameId, reverse, offset
                | _ -> model, model.currAnimationFlow, mTwoD.playAnimation, mTwoD.frameId, mTwoD.reverseAnimation, mTwoD.offsetId
            let updatedTwoDModel = 
                {newModel.twoDModel with 
                    playAnimation = play
                    frameId = frame
                    offsetId = offsetTimeId
                    reverseAnimation = reversedAnim}
            let mainTouching = 
                match model.mainControllerId with
                | Some i when i = id -> false
                | _ -> model.mainTouching
            let secondTouching = 
                match model.secondControllerId with
                | Some i when i = id -> false
                | _ -> model.secondTouching
            {newModel with 
                mainUntouched = untouched
                mainTouching = mainTouching
                secondTouching = secondTouching
                currAnimationFlow = animFlow
                twoDModel = updatedTwoDModel}
            //let tex = if model.allowHeraScaling then (texture "initial-scaling") else model.touchpadTexture
            //match model.touchpadDeviceId with 
            //| Some i when i = id -> 
            //    {model with touchpadDeviceId = None} 
            //| _ -> model
        | ResetHera id ->
            match model.mainControllerId with 
            | Some i when i = id && model.mainContrBoxPlotIntersectionId.IsNone -> initial runtime frames
            | _ -> model