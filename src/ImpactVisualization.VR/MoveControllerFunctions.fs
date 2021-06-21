namespace ImpactVisualization

open System
open System.IO
open System.Text.Json
open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Vr
open Aardvark.Application
open Aardvark.Rendering.GL


open FSharp.Data.Adaptive
open FSharpx.Collections

open AardVolume.Model
open AardVolume.App
open ImpactVisualization
open ImpactVisualization.UpdateFunctions
open Aardvark.Cef

module MoveControllerFunctions =

    let checkProbeIntersection (id : int) (model : Model) (controllerId : Option<int>) (controllerTrafo : Trafo3d) (controllerIntersectionId : Option<string>) =
        match controllerId with 
           | Some i when i = id && not model.currentProbeManipulated && not model.grabbingHera ->
               let intersectionContrTrafoPos = controllerTrafo.Forward.TransformPos(V3d(0.0, 0.0, 0.0))
               let controllerSphere = Sphere3d(intersectionContrTrafoPos, 0.05)
               model.allProbes
               |> HashMap.filter (fun key probe ->
                   let sphere = Sphere3d(probe.center, probe.radius)
                   sphereIntersection controllerSphere sphere)
               |> HashMap.toSeq
               |> Seq.map (fun (key, probe) -> key)
               |> Seq.tryLast    
           | None -> None
           | _ -> controllerIntersectionId
           
    let checkBoxPlotIntersection (id : int) (model : Model) (controllerId : Option<int>) (condition : bool) (controllerTrafo : Trafo3d) (controllerIntersectionId : Option<string>) =
        match controllerId with 
           | Some i when i = id && condition && not model.grabbingHera ->
               let intersectionContrTrafoPos = controllerTrafo.Forward.TransformPos(V3d(0.0, 0.0, 0.0))
               let controllerSphere = Sphere3d(intersectionContrTrafoPos, 0.05)
               model.allPlacedBoxPlots
               |> HashMap.filter (fun key boxPlot -> controllerSphere.BoundingBox3d.Intersects(boxPlot.positions))
               |> HashMap.toSeq
               |> Seq.map (fun (key, boxPlot) -> key)
               |> Seq.tryLast      
           | None -> None
           | _ -> controllerIntersectionId

    let updateHMDPos (hmdPos : V3d) (model : Model) =
        {model with hmdPos = hmdPos}

    let updateDevicesTrafos (id : int) (trafo : Trafo3d) (model : Model) = 
        let newInput = model.devicesTrafos.Add(id, trafo)
        let mainContrTrafo = newOrOldTrafo id trafo model.mainControllerId model.mainControllerTrafo
        let secondContrTrafo = newOrOldTrafo id trafo model.secondControllerId model.secondControllerTrafo
        {model with 
            devicesTrafos = newInput
            mainControllerTrafo = mainContrTrafo
            secondControllerTrafo = secondContrTrafo}

    let updateSphereScalingFactor (model : Model) = 
        let scalingFactor =
            if not model.scalingSphere then
                model.sphereScale
            else 
                let contrPos = model.mainControllerTrafo.Forward.TransformPos(V3d.OOO)
                let scalerPos = model.secondControllerTrafo.Forward.TransformPos(V3d.OOO)
                let distance = contrPos.Distance(scalerPos)
                let newScale = (4.0/3.0)*Math.PI*Math.Pow((distance + model.sphereRadius), 3.0)
                newScale + model.sphereRadius/2.0
        {model with sphereScale = scalingFactor}

    let updateTvTrafos (model : Model) = 
        let newTvScaleTrafo = 
            if model.grabbingTV && model.mainTouching then
                Trafo3d(Scale3d(model.scalingFactorTv))
            else 
                model.lastTvScaleTrafo

        let newTvTransformations, updatedTvQuad = 
            if model.grabbingTV then 
                let tvTrafo = model.tvToControllerTrafo * model.mainControllerTrafo
                let rotation = Trafo3d.RotationEulerInDegrees(90.0, 0.0, -90.0)
                let translation = Trafo3d.Translation(2.5, 1.0, 1.5)

                let newTvTrafos = newTvScaleTrafo * rotation * translation * tvTrafo
                let newTvQuad = newTvTrafos.Forward.TransformedPosArray(browserQuad.Points.ToArray(4)) |> Quad3d

                newTvTrafos, newTvQuad
            else 
                model.tvTransformations, model.tvQuad
        {model with 
            lastTvScaleTrafo = newTvScaleTrafo
            tvTransformations = newTvTransformations
            tvQuad = updatedTvQuad}

    let updateRay (id : int) (client : Browser) (model : Model)  = 
        match model.mainControllerId with 
        | Some i when i = id -> 
            let initRay = 
                if model.rayActive then
                    let origin = model.mainControllerTrafo.Forward.TransformPos(V3d(0.0, 0.02, 0.0))
                    let direction = model.mainControllerTrafo.Forward.TransformPos(V3d.OIO * 1000.0)
                    Ray3d(origin, direction)
                else Ray3d.Invalid    
            let mutable hit = RayHit3d.MaxRange
            let hitPoint = if model.rayActive then initRay.Hits(model.tvQuad, &hit) else false
            let currRay, rColor, screenCoordsHitPos =
                if model.rayActive then 
                    if hitPoint then
                        let screenCoords = (V2d(hit.Coord.X * screenResolution.ToV2d().X, hit.Coord.Y * screenResolution.ToV2d().Y)).ToV2i()
                        let screenPos = PixelPosition(screenCoords, screenResolution.X, screenResolution.Y)
                        if model.rayTriggerClicked then
                            client.Mouse.Move(screenPos)
                            printfn "Move"
                        Ray3d(initRay.Origin, hit.Point), C4b.Green, screenPos
                    else
                        initRay, C4b.Red, PixelPosition()
                else Ray3d.Invalid, C4b.Red, PixelPosition()

            {model with 
                screenIntersection = hitPoint
                hitPoint = hit.Point
                screenHitPoint = hit.Coord
                ray = currRay
                rayColor = rColor
                screenCoordsHitPos = screenCoordsHitPos}
        | _ -> model

    let updateHeraTrafos (model : Model) =
        let newHeraScaleTrafo = 
            if model.grabbingHera && model.mainTouching then
                Trafo3d(Scale3d(model.scalingFactorHera))
            else 
                model.lastHeraScaleTrafo
           
        let heraTrafos = 
            if model.grabbingHera then
                let heraTrafo = model.heraToControllerTrafo * model.mainControllerTrafo
                let heraTranslation = Trafo3d.Translation(0.0, 0.0, 0.7)
                newHeraScaleTrafo * heraTranslation * heraTrafo
            else 
                model.heraTransformations
        let heraBBox = model.twoDModel.currHeraBBox.Transformed(heraTrafos)

        {model with 
            lastHeraScaleTrafo = newHeraScaleTrafo
            heraTransformations = heraTrafos
            heraBox = heraBBox}

    let updateProbeIntersections (id : int) (state : VrState) (model : Model) = 
        //let device = state.devices |> HashMap.tryFind id
        let mainContrProbeIntersection = checkProbeIntersection id model model.mainControllerId model.mainControllerTrafo model.mainContrProbeIntersectionId
        vibrateController mainContrProbeIntersection model.mainContrProbeIntersectionId model.mainControllerId 50.0 state
        let secondContrProbeIntersection = checkProbeIntersection id model model.secondControllerId model.secondControllerTrafo model.secondContrProbeIntersectionId
        vibrateController secondContrProbeIntersection model.secondContrProbeIntersectionId model.secondControllerId 50.0 state
        {model with
            mainContrProbeIntersectionId = mainContrProbeIntersection
            secondContrProbeIntersectionId = secondContrProbeIntersection}

    let updateBoxPlotIntersections (id : int) (state : VrState) (model : Model) = 
        let mainContrBoxPlotIntersection = checkBoxPlotIntersection id model model.mainControllerId model.mainContrProbeIntersectionId.IsNone model.mainControllerTrafo model.mainContrBoxPlotIntersectionId
        vibrateController mainContrBoxPlotIntersection model.mainContrBoxPlotIntersectionId model.mainControllerId 50.0 state
        let secondContrBoxPlotIntersection = checkBoxPlotIntersection id model model.secondControllerId model.secondContrProbeIntersectionId.IsNone model.secondControllerTrafo model.secondContrBoxPlotIntersectionId
        vibrateController secondContrBoxPlotIntersection model.secondContrBoxPlotIntersectionId model.secondControllerId 50.0 state
        {model with
            mainContrBoxPlotIntersectionId = mainContrBoxPlotIntersection
            secondContrBoxPlotIntersectionId = secondContrBoxPlotIntersection}

    let updateClippingPlane (model : Model) = 
        let currCorners = 
            if model.holdClipping && model.clippingActive then
                let p0 = model.mainControllerTrafo.Forward.TransformPos(planePos0)
                let p1 = model.mainControllerTrafo.Forward.TransformPos(planePos1)
                let p2 = model.mainControllerTrafo.Forward.TransformPos(planePos2)
                let p3 = model.mainControllerTrafo.Forward.TransformPos(planePos3)
                Quad3d(p0, p1, p2, p3)
            else model.planeCorners
        {model with planeCorners = currCorners}

    let updateSecondControllеrClippingIntersection (id : int) (model : Model) = 
        let secondContrClippingIntersection =
            match model.secondControllerId with 
            | Some i when i = id && not model.holdClipping && model.planeCorners.IsValid ->
                let intersectionContrTrafoPos = model.secondControllerTrafo.Forward.TransformPos(V3d(0.0, 0.0, 0.0))
                let controllerSphere = Sphere3d(intersectionContrTrafoPos, 0.05)
                controllerSphere.BoundingBox3d.Intersects(model.planeCorners)
            | _ -> 
                if model.holdClipping || not model.planeCorners.IsValid then false else 
                    model.interesctingClippingPlane
        {model with 
            interesctingClippingPlane = secondContrClippingIntersection
            clippingColor = if secondContrClippingIntersection then C4b(1.0,0.0,0.0,0.2) else C4b(1.0,1.0,0.1,0.2)}

    let updateMainControllerTextures (model : Model) =
        //printf "AnalyzeMode: %A \n" model.analyzeMode
        let tex, screenTex = 
            if not model.grabbingHera && not model.mainMenuOpen then 
                match model.mainContrProbeIntersectionId with
                | Some probeId when model.allProbes.TryFind(probeId).IsSome ->
                    let intersectedProbe = model.allProbes.Item probeId
                    let billboard = intersectedProbe.currBillboard
                    let texName, screenTexName =    
                        match billboard with
                        | BillboardType.Histogram -> "histogram-selected", "histogram"
                        | BillboardType.Statistic -> "statistics-selected", "statistics"
                        | _ -> "histogram-selected", "histogram"
                    (texture texName), (texture screenTexName)
                | _ ->
                    if model.controllerMode = ControllerMode.Probe then
                        let newContrScreenTexture = 
                            match model.attribute with 
                            | RenderValue.Energy -> texture "probe-energy"
                            | RenderValue.CubicRoot -> texture "probe-cubicroot"
                            | RenderValue.Strain -> texture "probe-strain"
                            | RenderValue.AlphaJutzi -> texture "probe-alphajutzi"
                            | RenderValue.Pressure -> texture "probe-pressure"
                            | RenderValue.Density -> texture "probe-density"
                            | _ -> model.mainContrScreenTexture
                        model.mainTouchpadTexture, newContrScreenTexture
                    else if model.controllerMode = ControllerMode.Analyze && model.analyzeMode = AnalyzeMode.Time then 
                        model.analyzeMainTexture, model.analyzeContrScreenTexture
                    else
                        model.mainTouchpadTexture, model.mainContrScreenTexture
            else 
                model.mainTouchpadTexture, model.mainContrScreenTexture
        {model with
            mainTouchpadTexture = tex
            mainContrScreenTexture = screenTex}

    let updateSecondControllerTextures (model : Model) = 
        let secondTex, secondContrScreenTex = 
            if model.secondContrProbeIntersectionId.IsSome || model.interesctingClippingPlane || model.secondContrBoxPlotIntersectionId.IsSome then
                model.secondTouchpadTexture, (texture "delete-object") 
            else     
                if not model.mainMenuOpen && not model.secondTouching && model.mainContrProbeIntersectionId.IsNone && not model.grabbingHera && model.controllerMode = ControllerMode.Probe then 
                    model.secondTouchpadTexture, (texture "select-attribute") 
                else
                    if model.controllerMode = ControllerMode.Analyze then
                        computeNewAttributeTextures model.currBoxPlotAttrib
                    else 
                        match model.mainContrProbeIntersectionId with 
                        | Some probeId when model.allProbes.TryFind(probeId).IsSome ->
                            let intersectedProbe = model.allProbes.Item probeId
                            let probeAttrib = intersectedProbe.currAttribute
                            computeNewAttributeTextures probeAttrib
                        | _ ->
                            if model.secondTouching then model.secondTouchpadTexture, model.secondContrScreenTexture else model.secondTouchpadTexture, texture ("empty")
        {model with
            secondTouchpadTexture = secondTex
            secondContrScreenTexture = secondContrScreenTex}

    let updateProbesWhenGrabbing (model : Model) =
        let probesUpdate =
            if model.grabbingHera && not model.allProbes.IsEmpty then 
                model.allProbes
                |> HashMap.map (fun key probe -> 
                    let newCenter = model.heraTransformations.Forward.TransformPos(probe.centerRelToHera)
                    let newRadius = probe.radiusRelToHera * model.heraTransformations.Forward.GetScaleVector3().X
                    { probe with 
                        center = newCenter
                        radius = newRadius
                    })
            else model.allProbes     
        {model with allProbes = probesUpdate}

    let updateBillboardsVisibility (viewTrafo : Trafo3d) (projTrafo : Trafo3d) (model : Model) = 
        let allProbesUpdated = 
            if model.allProbes.Count >= 2 && not model.grabbingHera then
                model.allProbes
                |> HashMap.map (fun key probe -> 
                    let currProbeIntersected = 
                        match model.mainContrProbeIntersectionId with 
                        | Some prKey when prKey = key -> true
                        | _ -> false
                    let allProbesWithoutCurrent = model.allProbes.Remove(key)
                    let probePos, probeRadius = probeInScreenCoordinates viewTrafo projTrafo probe.center probe.radius
                    let statisticsVisible = 
                        if currProbeIntersected then true
                        else
                            allProbesWithoutCurrent
                            |> HashMap.forall (fun k p ->
                                let pPos, pRadius = probeInScreenCoordinates viewTrafo projTrafo p.center p.radius
                                let distance = probePos.XY.Distance(pPos.XY)
                                let allowedMinDistance = 0.65 * (probeRadius + pRadius)
                                if (distance < allowedMinDistance) then // checks if the two probes intersect
                                    if probePos.Z < pPos.Z then // checks wether the current probe is closer to the screen
                                        match model.mainContrProbeIntersectionId with 
                                        | Some prKey when prKey = k -> false
                                        | _ -> true
                                    else not p.showStatistics
                                else true)
                    {probe with 
                        showStatistics = statisticsVisible
                        showHistogram = statisticsVisible})
            else model.allProbes
        {model with allProbes = allProbesUpdated}

    let updateTexturesVisibility (model : Model) =
        let showMainTexture = 
            model.mainContrProbeIntersectionId.IsSome || model.grabbingHera || model.mainMenuOpen || 
            model.grabbingTV || (model.controllerMode = ControllerMode.Analyze && model.analyzeMode = AnalyzeMode.Time)

        let showSecondTexture = 
            model.secondContrProbeIntersectionId.IsNone && not model.interesctingClippingPlane && 
            (((model.controllerMode = ControllerMode.Probe) && not showMainTexture && model.secondTouching) || 
                model.mainContrProbeIntersectionId.IsSome || model.controllerMode = ControllerMode.Analyze) 
        {model with 
            showMainTexture = showMainTexture
            showSecondTexture = showSecondTexture}

    let updateTakenBoxPlot (model : Model) = 
        let updateTakenBoxPlot = 
            if model.movingBoxPlot then 
                let bp = model.takenBoxPlot.Value
                let newTrafo = defaultBoxPlotsTrafo * model.mainControllerTrafo
                let updateBP = {bp with trafo = newTrafo}
                Some updateBP
            else 
                model.takenBoxPlot
        {model with 
            takenBoxPlot = updateTakenBoxPlot
            newProbePlaced = (if model.newProbePlaced then false else model.newProbePlaced)}

