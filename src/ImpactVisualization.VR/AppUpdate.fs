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
open Aardvark.Cef

module AppUpdate =
    open Aardvark.UI.Primitives
    open Aardvark.Base.Rendering

    let quadRightUp   = V3d(0.732, 0.432, -0.013)
    let quadLeftUp    = V3d(-0.732, 0.432, -0.013)
    let quadLeftDown  = V3d(-0.732, -0.41483, -0.013)
    let quadRightDown = V3d(0.732, -0.41483, -0.013)

    let planePos0 = V3d(-0.7, 0.05, -0.5)
    let planePos1 = V3d(-0.7, 0.05, 0.5)
    let planePos2 = V3d(0.7, 0.05, 0.5)
    let planePos3 = V3d(0.7, 0.05, -0.5)

    let flatScreenTrafo rotY rotZ upZ = 
        let scale = Trafo3d(Scale3d(2.0))
        let rotation = Trafo3d.RotationEulerInDegrees(90.0, rotY, rotZ)
        let translation = Trafo3d.Translation(2.5, 1.0, upZ)
        scale * rotation * translation

    let tvQuadTrafo = flatScreenTrafo 180.0 90.0 1.535
    let tvTrafo = flatScreenTrafo 0.0 -90.0 1.5

    let screenResolution = V2i(1008,729)

    let texturesPath = @"..\..\..\src\ImpactVisualization\resources\textures\"
    
    let fromStreamToTexture textureName = 
        let s = File.Open((texturesPath + textureName), FileMode.Open, FileAccess.Read, FileShare.Read)
        let p = PixImage.Create s
        let t = PixTexture2d(PixImageMipMap [| p |], true) :> ITexture
        t

    let convertPixImageToITexture (p : PixImage) =
        PixTexture2d(PixImageMipMap [| p |], true) :> ITexture

    let allTextures = 
        HashMap.ofList ["initial", fromStreamToTexture "initial.png"; 
                        "left", fromStreamToTexture "left.png"; 
                        "middle", fromStreamToTexture "middle.png"; 
                        "right", fromStreamToTexture "right.png"; 
                        "analyze", fromStreamToTexture "analyze.png"; 
                        "bottom-left", fromStreamToTexture "bottom-left.png"; 
                        "bottom-middle", fromStreamToTexture "bottom-middle.png"; 
                        "bottom-right", fromStreamToTexture "bottom-right.png"; 
                        "top-left", fromStreamToTexture "top-left.png"; 
                        "top-middle", fromStreamToTexture "top-middle.png"; 
                        "top-right", fromStreamToTexture "top-right.png"; 
                        "initial-attributes", fromStreamToTexture "initial-attributes.png"; 
                        "scale-down", fromStreamToTexture "scale-down.png"; 
                        "scale-up", fromStreamToTexture "scale-up.png"; 
                        "initial-scaling", fromStreamToTexture "initial-scaling.png";
                        "energy", fromStreamToTexture "energy.png";
                        "cubicroot", fromStreamToTexture "cubicroot.png";
                        "strain", fromStreamToTexture "strain.png";
                        "alphajutzi", fromStreamToTexture "alphajutzi.png";
                        "pressure", fromStreamToTexture "pressure.png";
                        "density", fromStreamToTexture "density.png";
                        "probe", fromStreamToTexture "probe.png";
                        "probe-alphajutzi", fromStreamToTexture "probe-alphajutzi.png";
                        "probe-cubicroot", fromStreamToTexture "probe-cubicroot.png";
                        "probe-density", fromStreamToTexture "probe-density.png";
                        "probe-energy", fromStreamToTexture "probe-energy.png";
                        "probe-pressure", fromStreamToTexture "probe-pressure.png";
                        "probe-strain", fromStreamToTexture "probe-strain.png";
                        "ray", fromStreamToTexture "ray.png";
                        "clipping", fromStreamToTexture "clipping.png";
                        "select-attribute", fromStreamToTexture "select-attribute.png";
                        "select-tool", fromStreamToTexture "select-tool.png";
                        "empty", fromStreamToTexture "empty.png";
                        "grab-sphere", fromStreamToTexture "grab-sphere.png";
                        "scaling-up", fromStreamToTexture "scaling-up.png";
                        "scaling-down", fromStreamToTexture "scaling-down.png";
                        "histogram-selected", fromStreamToTexture "histogram-selected.png";
                        "statistics-selected", fromStreamToTexture "statistics-selected.png";
                        "histogram", fromStreamToTexture "histogram.png";
                        "statistics", fromStreamToTexture "statistics.png";
                        "delete-object", fromStreamToTexture "delete-object.png";
                        "analyze-probes", fromStreamToTexture "analyze-probes.png";
                        "main-controller", fromStreamToTexture "main-controller.png"]
    
    let trafoOrIdentity trafo = Option.defaultValue Trafo3d.Identity trafo

    let newOrOldTrafo (id : int) (trafo : Trafo3d) (contrId : Option<int>) (contrTrafo : Trafo3d) =
        match contrId with 
        | Some i when i = id -> trafo 
        | _ -> contrTrafo
    
    let idIsSet (contrId : Option<int>) = contrId.IsSome

    let sphereIntersection (controllerSphere : Sphere3d) (probe : Sphere3d) = controllerSphere.Intersects(probe)
    

    let createProbe (numberId : int) (selected : bool) (color : C4b) (pos : V3d) (rad : float) (posToHera : V3d) (radToHera : float) 
                    (inside : bool) (allData : HashMap<RenderValue, (float[] * string)>) 
                    (attribute : RenderValue) (showStats : bool) (showHisto : bool) 
                    (billboardType : BillboardType) : Probe = 
        {
            id = Guid.NewGuid().ToString()
            numberId = numberId
            selected = selected
            color = color
            center = pos
            radius = rad
            centerRelToHera = posToHera
            radiusRelToHera = radToHera
            insideHera = inside
            allData = allData
            currAttribute = attribute
            showStatistics = showStats
            currHistogram = None
            showHistogram = showHisto
            currBillboard = billboardType
        }

    let createBoxPlot (attribute : RenderValue) (trafo : Trafo3d) (texture : PixImage) (data : HashMap<int, float[]>): BoxPlot =
        {   
            id = Guid.NewGuid().ToString()
            attribute = attribute
            trafo = trafo
            texture = texture 
            data = data
        }
    
    let convertCartesianToPolar (cartCoords : V2d) = 
        let x = cartCoords.X
        let y = cartCoords.Y
    
        let r = Math.Sqrt(Math.Pow(x, 2.0) + Math.Pow(y, 2.0))
        let t = Math.Atan2(y, x)
        let theta = 
            let angle = t * (180.0 / Math.PI) 
            if y < 0.0 then angle + 360.0 else angle
        r, theta

    let computeNewAttribute (touchpadPos : V2d) (currAttribute : RenderValue) =
        let r, theta = convertCartesianToPolar touchpadPos
        if r >= 0.5 then 
            match theta with 
            | t when t >= 0.0   && t < 60.0  -> RenderValue.Energy
            | t when t >= 60.0  && t < 120.0 -> RenderValue.CubicRoot
            | t when t >= 120.0 && t < 180.0 -> RenderValue.Strain
            | t when t >= 180.0 && t < 240.0 -> RenderValue.AlphaJutzi
            | t when t >= 240.0 && t < 300.0 -> RenderValue.Pressure
            | t when t >= 300.0 && t < 360.0 -> RenderValue.Density
            | _ -> currAttribute
        else 
            currAttribute


    let initialScalingHera = 0.05

    let defaultBoxPlotsTrafo = 
        let scale = Trafo3d.Scale(0.3)
        let rotation = Trafo3d.RotationXInDegrees(35.0)
        let translation = Trafo3d.Translation(0.0, 0.25, 0.1)
        scale * rotation * translation

    let initial runtime frames = 
        {   
            twoDModel = AardVolume.App.initial frames
            text = "hello" 
            threads = ThreadPool.Empty
            firstHistogram = true
            showBillboard = true
            devicesTrafos = HashMap.Empty
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
            lastHeraScaleTrafo = Trafo3d(Scale3d(initialScalingHera))
            sphereScale = 1.0
            lastSphereScale = 1.0
            sphereRadius = 0.2
            currentProbeManipulated = false
            newProbePlaced = false
            existingProbeModified = false
            allProbes = HashMap.Empty
            mainContrProbeIntersectionId = None
            secondContrProbeIntersectionId = None
            lastFilterProbe = None
            lastFilterProbeId = None
            lastModifiedProbeIntId = -1
            boxPlotProbes = HashMap.empty
            mainContrBoxPlotIntersectionId = None
            secondContrBoxPlotIntersectionId = None
            lastProbeId = 0
            currBoxPlotAttribSet = false
            currBoxPlotAttrib = RenderValue.Energy
            showCurrBoxPlot = false
            currBoxPlot = None
            allPlacedBoxPlots = HashMap.empty
            rayActive = false
            ray = Ray3d.Invalid
            rayTriggerClicked = false
            clickPosition = None
            tvQuad = 
                let quadRightUpTransf = tvQuadTrafo.Forward.TransformPos(quadRightUp)
                let quadLeftUpTransf = tvQuadTrafo.Forward.TransformPos(quadLeftUp)
                let quadLeftDownTransf = tvQuadTrafo.Forward.TransformPos(quadLeftDown)
                let quadRightDownTransf = tvQuadTrafo.Forward.TransformPos(quadRightDown)
                Quad3d(quadLeftDownTransf, quadLeftUpTransf, quadRightUpTransf, quadRightDownTransf)
            rayColor = C4b.Red
            screenIntersection = false
            hitPoint = V3d.OOO
            screenHitPoint = V2d.OO
            screenCoordsHitPos = PixelPosition()
            holdClipping = false
            clippingActive = false
            clippingColor = C4b(1.0,1.0,0.1,0.2)
            planeCorners = Quad3d()
            interesctingClippingPlane = false
            mainMenuOpen = false
            secondMenuOpen = false
            menuLevel = 0
            controllerMode = ControllerMode.NoneMode
            attribute = RenderValue.NoValue
            currMainTouchPadPos = V2d.OO
            currSecondTouchPadPos = V2d.OO
            mainTouching = false
            secondTouching = false
            mainTouchpadTexture = allTextures.Item "initial"
            secondTouchpadTexture = allTextures.Item "initial-attributes"
            lastTouchpadModeTexture = allTextures.Item "initial"
            mainContrScreenTexture = allTextures.Item "empty"
            secondContrScreenTexture = allTextures.Item "empty"
            lastContrScreenModeTexture = allTextures.Item "empty"
            showMainTexture = false
            showSecondTexture = false
            heraBox = Box3d.Infinite
            heraTransformations = Trafo3d(Scale3d(initialScalingHera)) * Trafo3d.Translation(0.0, 0.0, 0.7)
        }

    let getScreenshot (histogramClient : Browser) = 
        let pixImage = PixImage<byte>(Col.Format.BGRA,histogramClient.Size.GetValue())
        let temp = pixImage.GetMatrix<C4b>().SetByCoord(fun (v : V2l) -> histogramClient.ReadPixel(V2i v) |> C4b) 
        pixImage :> PixImage

    let rec update (runtime : IRuntime) (client : Browser) (histogramClient : Browser) (boxPlotClient : Browser) (viewTrafo : aval<Trafo3d>) (projTrafo : aval<Trafo3d>) (frames : Frame[]) (state : VrState) (vr : VrActions) (model : Model) (msg : Message) =
        let mTwoD = model.twoDModel

        let defaultTex = allTextures.Item "empty"
        let getTexture tex = Option.defaultValue defaultTex tex
        let texture tex = allTextures.TryFind(tex) |> getTexture    

        let computeNewAttributeTextures (newAttribute : RenderValue) = //(currAttribute : RenderValue) = 
            //match currAttribute with 
            //| a when a = newAttribute && newAttribute <> RenderValue.Energy -> model.touchpadTexture, model.contrScreenTexture // if the attribute is the same then texture should not be loaded again   
            //| _ ->
            let texName, screenTexName = 
                match newAttribute with 
                | RenderValue.Energy -> "top-right", "energy"
                | RenderValue.CubicRoot -> "top-middle", "cubicroot"
                | RenderValue.Strain -> "top-left", "strain"
                | RenderValue.AlphaJutzi -> "bottom-left", "alphajutzi"
                | RenderValue.Pressure -> "bottom-middle", "pressure"
                | RenderValue.Density -> "bottom-right", "density"
                | _ -> "initial-attributes", "select-attribute"
            (texture texName), (texture screenTexName)

        let callUpdate (msg : Message) = update runtime client histogramClient boxPlotClient viewTrafo projTrafo frames state vr model msg
       
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

        // https://stackoverflow.com/questions/3717226/radius-of-projected-sphere
        let probeInScreenCoordinates (probeCenter : V3d) (radius : float) = 
            let center_view = viewTr.Forward.TransformPos(probeCenter)
            let pointOnSphere = probeCenter + V3d(radius, 0.0, 0.0)
            let pointOnSphere_view = viewTr.Forward.TransformPos(pointOnSphere)
            let radius_view = center_view.Distance(pointOnSphere_view)
            let pointOnSphere_perpendicular = center_view + V3d(radius_view, 0.0, 0.0)
            let center_screen = projTr.Forward.TransformPosProj(center_view)
            let pointOnSphere_screen = projTr.Forward.TransformPosProj(pointOnSphere_perpendicular)
            let radius_screen = center_screen.Distance(pointOnSphere_screen)
            center_screen, radius_screen

        let computeSleepTime (dataSize : int) = 
            match dataSize with 
            | d when 800000 <= d -> 1800
            | d when (500000 <= d && d < 800000) || model.firstHistogram -> 1400
            | d when 100000 <= d && d < 500000 -> 800
            | d when 50000 <= d && d < 100000 -> 300
            | d when 10000 <= d && d < 50000 -> 200
            | d when 100 <= d && d < 10000 -> 130
            | _ -> 100

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
            | Some i when i = id ->
                let controlT = model.mainControllerTrafo
                let heraT = model.heraTrafo
                let controlHeraT = heraT * controlT.Inverse
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
        | UngrabHera id -> 
            match model.mainControllerId with 
            | Some i when i = id -> 
                let controlT = model.mainControllerTrafo
                let heraToControlT = model.heraToControllerTrafo
                let heraT = heraToControlT * controlT
                {model with 
                    heraTrafo = heraT
                    grabbingHera = false
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
        | MoveController (id, (trafo : Trafo3d)) -> 
            let newInput = model.devicesTrafos.Add(id, trafo)
            let mainContrTrafo = newOrOldTrafo id trafo model.mainControllerId model.mainControllerTrafo
            let secondContrTrafo = newOrOldTrafo id trafo model.secondControllerId model.secondControllerTrafo

            //CURRENT SPHERE UPDATE
            let scalingFactor = 
                if not model.scalingSphere then
                    model.sphereScale
                else 
                    let mainContrTrafo = model.mainControllerTrafo 
                    let scaleContrTrafo = model.secondControllerTrafo
                    let contrPos = mainContrTrafo.Forward.TransformPos(V3d.OOO)
                    let scalerPos = scaleContrTrafo.Forward.TransformPos(V3d.OOO)
                    let distance = contrPos.Distance(scalerPos)
                    let newScale = (4.0/3.0)*Math.PI*Math.Pow((distance + model.sphereRadius), 3.0)
                    newScale + model.sphereRadius/2.0
            
            //RAY UPDATE
            let initRay = 
                if model.rayActive then
                    let origin = mainContrTrafo.Forward.TransformPos(V3d(0.0, 0.02, 0.0))
                    let direction = mainContrTrafo.Forward.TransformPos(V3d.OIO * 1000.0)
                    Ray3d(origin, direction)
                else Ray3d.Invalid    
            //let intersect = initRay.Intersects(model.tvQuad)
            let mutable hit = RayHit3d.MaxRange
            let hitPoint = if model.rayActive then initRay.Hits(model.tvQuad, &hit) else false
            let currRay, rColor, screenCoordsHitPos =
                if model.rayActive then 
                    if hitPoint then
                        let screenCoords = (V2d(hit.Coord.Y * screenResolution.ToV2d().X, hit.Coord.X * screenResolution.ToV2d().Y)).ToV2i()
                        let screenPos = PixelPosition(screenCoords, screenResolution.X, screenResolution.Y)
                        if model.rayTriggerClicked then
                            client.Mouse.Move(screenPos)
                        Ray3d(initRay.Origin, hit.Point), C4b.Green, screenPos
                    else
                        initRay, C4b.Red, PixelPosition()
                else Ray3d.Invalid, C4b.Red, PixelPosition()
                    

            let newHeraScaleTrafo = 
                if model.grabbingHera && model.mainTouching then
                    //printf "UPDATE SCALING \n"
                    Trafo3d(Scale3d(model.scalingFactorHera))
                else 
                    model.lastHeraScaleTrafo
    
            //HERA TRANSFORMATIONS UPDATE
            let heraTrafos = 
                if model.grabbingHera then
                    let heraTrafo = model.heraToControllerTrafo * mainContrTrafo
                    //let heraScaleTrafo = Trafo3d(Scale3d(model.scalingFactorHera))
                    let heraTranslation = Trafo3d.Translation(0.0, 0.0, 0.7)
                    newHeraScaleTrafo * heraTranslation * heraTrafo
                else 
                    model.heraTransformations
            let heraBBox = model.twoDModel.currHeraBBox.Transformed(heraTrafos)

            //INTERSECTION OF MAIN CONTROLLER WITH A PROBE
            let mainContrProbeIntersection = 
                match model.mainControllerId with 
                | Some i when i = id && not model.currentProbeManipulated && not model.grabbingHera ->
                    let intersectionContrTrafoPos = model.mainControllerTrafo.Forward.TransformPos(V3d(0.0, 0.0, 0.0))
                    let controllerSphere = Sphere3d(intersectionContrTrafoPos, 0.05)
                    model.allProbes
                    |> HashMap.filter (fun key probe ->
                        let sphere = Sphere3d(probe.center, probe.radius)
                        sphereIntersection controllerSphere sphere)
                    |> HashMap.toSeq
                    |> Seq.map (fun (key, probe) -> key)
                    |> Seq.tryLast    
                | None -> None
                | _ -> model.mainContrProbeIntersectionId

            let mainContrBoxPlotIntersection = 
                match model.mainControllerId with 
                | Some i when i = id && mainContrProbeIntersection.IsNone && not model.grabbingHera ->
                    let intersectionContrTrafoPos = model.mainControllerTrafo.Forward.TransformPos(V3d(0.0, 0.0, 0.0))
                    let controllerSphere = Sphere3d(intersectionContrTrafoPos, 0.05)
                    model.allProbes
                    |> HashMap.filter (fun key probe ->
                        let sphere = Sphere3d(probe.center, probe.radius)
                        sphereIntersection controllerSphere sphere)
                    |> HashMap.toSeq
                    |> Seq.map (fun (key, probe) -> key)
                    |> Seq.tryLast    
                | None -> None
                | _ -> model.mainContrProbeIntersectionId

            //INTERSECTION OF SECOND CONTROLLER WITH A PROBE
            let secondContrProbeIntersection = 
                match model.secondControllerId with 
                | Some i when i = id && not model.currentProbeManipulated && not model.grabbingHera ->
                    let intersectionContrTrafoPos = model.secondControllerTrafo.Forward.TransformPos(V3d(0.0, 0.0, 0.0))
                    let controllerSphere = Sphere3d(intersectionContrTrafoPos, 0.05)
                    model.allProbes
                    |> HashMap.filter (fun key probe ->
                        let sphere = Sphere3d(probe.center, probe.radius)
                        sphereIntersection controllerSphere sphere)
                    |> HashMap.toSeq
                    |> Seq.map (fun (key, probe) -> key)
                    |> Seq.tryLast    
                | None -> None
                | _ -> model.secondContrProbeIntersectionId

            //CLIPPING PLANE UPDATE
            let currCorners = 
                if model.holdClipping && model.clippingActive then
                    let p0 = mainContrTrafo.Forward.TransformPos(planePos0)
                    let p1 = mainContrTrafo.Forward.TransformPos(planePos1)
                    let p2 = mainContrTrafo.Forward.TransformPos(planePos2)
                    let p3 = mainContrTrafo.Forward.TransformPos(planePos3)
                    Quad3d(p0, p1, p2, p3)
                else model.planeCorners

            let secondContrClippingIntersection =
                match model.secondControllerId with 
                | Some i when i = id && not model.holdClipping && currCorners.IsValid ->
                    let intersectionContrTrafoPos = model.secondControllerTrafo.Forward.TransformPos(V3d(0.0, 0.0, 0.0))
                    let controllerSphere = Sphere3d(intersectionContrTrafoPos, 0.05)
                    controllerSphere.BoundingBox3d.Intersects(currCorners)
                | _ -> false

            ////UPDATE MAIN CONTROLLER TEXTURES WHEN INTERSECTING WITH A PROBE
            let tex, screenTex = 
                if not model.grabbingHera && not model.mainMenuOpen then 
                    match mainContrProbeIntersection with
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
                        else 
                            model.mainTouchpadTexture, model.lastContrScreenModeTexture
                else 
                    model.mainTouchpadTexture, model.mainContrScreenTexture

            let secondTex, secondContrScreenTex = 
                if secondContrProbeIntersection.IsSome || secondContrClippingIntersection then
                    model.secondTouchpadTexture, (texture "delete-object") 
                else     
                    if not model.mainMenuOpen && not model.secondTouching && mainContrProbeIntersection.IsNone && not model.grabbingHera && model.controllerMode = ControllerMode.Probe then 
                        model.secondTouchpadTexture, (texture "select-attribute") 
                    else
                        if model.controllerMode = ControllerMode.Analyze then
                            computeNewAttributeTextures model.currBoxPlotAttrib
                        else 
                            match mainContrProbeIntersection with 
                            | Some probeId when model.allProbes.TryFind(probeId).IsSome ->
                                let intersectedProbe = model.allProbes.Item probeId
                                let probeAttrib = intersectedProbe.currAttribute
                                computeNewAttributeTextures probeAttrib
                            | _ ->
                                if model.secondTouching then model.secondTouchpadTexture, model.secondContrScreenTexture else model.secondTouchpadTexture, texture ("empty")



            //PROBES UPDATE WHEN HERA GRABBED
            let probesUpdate =
                if model.grabbingHera && not model.allProbes.IsEmpty then 
                    model.allProbes
                    |> HashMap.map (fun key probe -> 
                        let newCenter = heraTrafos.Forward.TransformPos(probe.centerRelToHera)
                        let newRadius = probe.radiusRelToHera * heraTrafos.Forward.GetScaleVector3().X
                        //let sphere = Sphere3d(newCenter, newRadius)
                        //let intersection = heraBBox.Intersects(sphere)
                        { probe with 
                            center = newCenter
                            radius = newRadius
                            //insideHera = intersection
                        }
                    )
                else 
                    model.allProbes      

            // UPDATE VISIBILITY OF THE BILLBOARDS ABOVE PROBES
            let allProbesUpdated = 
                if model.allProbes.Count >= 2 && not model.grabbingHera then
                    probesUpdate
                    |> HashMap.map (fun key probe -> 
                        let currProbeIntersected = 
                            match mainContrProbeIntersection with 
                            | Some prKey when prKey = key -> true
                            | _ -> false
                        let allProbesWithoutCurrent = probesUpdate.Remove(key)
                        let probePos, probeRadius = probeInScreenCoordinates probe.center probe.radius
                        let statisticsVisible = 
                            if currProbeIntersected then true
                            else
                                allProbesWithoutCurrent
                                |> HashMap.forall (fun k p ->
                                    //let distanceWorld = probe.center.Distance(p.center)
                                    //let allowedMinDistanceWorld = 0.65 * (probe.radius + p.radius)
                                    //let depthCurr = hmdPos.Distance(probe.center)
                                    //let depth = hmdPos.Distance(p.center)
                                    let pPos, pRadius = probeInScreenCoordinates p.center p.radius
                                    let distance = probePos.XY.Distance(pPos.XY)
                                    let allowedMinDistance = 0.65 * (probeRadius + pRadius)
                                    //let allowedMinDistanceHisto = 0.85 * (probeRadius + pRadius)
                                    if (distance < allowedMinDistance) then // checks if the two probes intersect
                                        if probePos.Z < pPos.Z then // checks wether the current probe is closer to the screen
                                            match mainContrProbeIntersection with 
                                            | Some prKey when prKey = k -> false
                                            | _ -> true
                                        else not p.showStatistics
                                    else true
                            )
                        {probe with 
                            showStatistics = statisticsVisible
                            showHistogram = statisticsVisible}
                    )
                else 
                    probesUpdate

            // UPDATE TEXTURES VISIBILITY
            let showMainTexture = mainContrProbeIntersection.IsSome || model.grabbingHera || model.mainMenuOpen

            let showSecondTexture = 
                secondContrProbeIntersection.IsNone && not secondContrClippingIntersection && 
                (((model.controllerMode = ControllerMode.Probe) && not showMainTexture && model.secondTouching) || 
                    mainContrProbeIntersection.IsSome || model.controllerMode = ControllerMode.Analyze) 
        
            {model with 
                devicesTrafos = newInput
                mainControllerTrafo = mainContrTrafo
                secondControllerTrafo = secondContrTrafo
                sphereScale = scalingFactor
                ray = currRay
                planeCorners = currCorners
                screenIntersection = hitPoint
                rayColor = rColor
                clippingColor = if secondContrClippingIntersection then C4b(1.0,0.0,0.0,0.2) else C4b(1.0,1.0,0.1,0.2)
                interesctingClippingPlane = secondContrClippingIntersection
                hitPoint = hit.Point
                screenHitPoint = hit.Coord
                screenCoordsHitPos = screenCoordsHitPos
                mainContrProbeIntersectionId = mainContrProbeIntersection
                secondContrProbeIntersectionId = secondContrProbeIntersection
                mainTouchpadTexture = tex
                mainContrScreenTexture = screenTex
                secondTouchpadTexture = secondTex
                secondContrScreenTexture = secondContrScreenTex
                showMainTexture = showMainTexture
                showSecondTexture = showSecondTexture
                heraBox = heraBBox
                allProbes = allProbesUpdated
                heraTransformations = heraTrafos
                lastHeraScaleTrafo = newHeraScaleTrafo
                newProbePlaced = (if model.newProbePlaced then false else model.newProbePlaced)}
        | ActivateControllerMode id ->
            match model.mainControllerId with 
            | Some i when i = id && not model.mainMenuOpen -> 
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
                let intId = pr.numberId

                let newHashmap, updatedTwoDmodel = 
                    if intId <> -1 then 
                        let hm = model.boxPlotProbes.Remove(intId)
                        let dataForBoxPlot = hm |> HashMap.toSeq |> Seq.map (fun result -> snd result ) |> Seq.toArray
                        hm, { mTwoD with boxPlotData = dataForBoxPlot}
                    else 
                        model.boxPlotProbes, model.twoDModel

                {model with 
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
            | None -> 
                {model with 
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
            client.Mouse.Down(model.screenCoordsHitPos, MouseButtons.Left)
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
            {model with 
                planeCorners = Quad3d(p0, p1, p2, p3)
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
                        boxPlotProbes = newHashmap
                        lastFilterProbe = filterProbe
                        lastFilterProbeId = probeId
                        twoDModel = updatedTwoDmodel
                        secondContrProbeIntersectionId = None} 
                | _ -> model
            | None -> model
        | DeleteClippingPlane id ->
            match model.secondControllerId with 
            | Some i when i = id && model.interesctingClippingPlane ->
                {model with
                    planeCorners = Quad3d()
                    holdClipping = false}
            | _ -> model
        | SelectBoxPlotProbes id ->
            match model.mainControllerId with 
            | Some i when i = id && not model.mainMenuOpen && model.controllerMode = ControllerMode.Analyze -> 
                match model.mainContrProbeIntersectionId with 
                | Some probeId ->
                    let intersectedProbe = model.allProbes.Item probeId
                    let isSelected = intersectedProbe.selected

                    let arrayBoxPlot, statsBoxPlot = intersectedProbe.allData.Item model.currBoxPlotAttrib

                    let newHashmap = 
                        if not isSelected then 
                            model.boxPlotProbes.Add(model.lastProbeId, arrayBoxPlot) 
                        else 
                            let currProbe = model.allProbes.Item probeId
                            let intId = currProbe.numberId
                            model.boxPlotProbes.Remove(intId)

                    let dataForBoxPlot = newHashmap |> HashMap.toSeq |> Seq.map (fun result -> snd result ) |> Seq.toArray

                    let updatedTwoDmodel = 
                        { mTwoD with
                            boxPlotData = dataForBoxPlot
                            boxPlotAttribute = renderValueToString model.currBoxPlotAttrib
                        }

                    let updatedProbe = 
                        if not isSelected then 
                            {intersectedProbe with
                                color = C4b.Yellow
                                numberId = model.lastProbeId
                                selected = not isSelected}
                        else 
                            {intersectedProbe with
                                color = C4b.Blue
                                numberId = -1
                                selected = not isSelected}


                    let update (pr : Option<Probe>) = updatedProbe 
                    let allProbesUpdated = model.allProbes |> HashMap.update probeId update

                    {model with 
                        boxPlotProbes = newHashmap
                        currBoxPlotAttribSet = true
                        allProbes = allProbesUpdated
                        twoDModel = updatedTwoDmodel
                        lastProbeId = model.lastProbeId + 1
                        }
                | None -> model
            | _ -> model
        | PlaceBoxPlot id ->
            match model.secondControllerId with 
            | Some i when i = id && model.controllerMode = ControllerMode.Analyze && not model.boxPlotProbes.IsEmpty ->
                let texture = getScreenshot boxPlotClient
                let currTrafo = defaultBoxPlotsTrafo * model.secondControllerTrafo
                let boxPlot = createBoxPlot model.currBoxPlotAttrib currTrafo texture model.boxPlotProbes
                let allPlacedBoxPlots = model.allPlacedBoxPlots.Add(boxPlot.id, boxPlot)
                let updatedTwoDmodel = 
                    {model.twoDModel with 
                        boxPlotData = [| |]
                        boxPlotAttribute = "" }
                let allProbesUpdated = 
                    model.allProbes 
                    |> HashMap.map (fun key probe ->
                        {probe with 
                            color = C4b.Blue
                            numberId = -1
                            selected = false})
                {model with 
                    allProbes = allProbesUpdated
                    allPlacedBoxPlots = allPlacedBoxPlots
                    boxPlotProbes = HashMap.empty
                    twoDModel = updatedTwoDmodel}
            | _ -> model
        | DeleteBoxPlot id ->
            model
        | TakeBoxPlot id ->
            model
        | DeactivateControllerMode id ->
            if not model.mainMenuOpen then 
                match model.controllerMode with 
                | ControllerMode.Probe -> 
                    match model.mainControllerId with
                    | Some i when i = id -> 
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

                        let allData = filterAllDataForOneFrameSphere frames.[mTwoD.frame] (Some sphereTransformed) discardProperties
                        let attrib, billboardType = 
                            if model.existingProbeModified then 
                                let lastProbe = model.lastFilterProbe.Value
                                let lastAttrib =  lastProbe.currAttribute
                                let lastBillboardType = lastProbe.currBillboard
                                lastAttrib, lastBillboardType
                            else 
                                let att = if model.attribute = RenderValue.NoValue then RenderValue.Energy else model.attribute
                                att, BillboardType.Histogram

                        let array, stats = allData.Item attrib


                        let selected = model.lastModifiedProbeIntId <> -1 
                        let color = if intersection then (if selected then C4b.Yellow else C4b.Blue) else C4b.White

                        let probe = createProbe model.lastModifiedProbeIntId selected color spherePos sphereRadius spherePosTransformed radiusTransformed intersection allData attrib true true billboardType

                        let filteredData = if intersection then array else mTwoD.data.arr
                        let attributeAsString = renderValueToString attrib

                        //let newCurrBoxPlotAttrib = if not model.currBoxPlotAttribSet then attrib else model.currBoxPlotAttrib
                        let newHashmap, dataForBoxPlot = 
                            if model.existingProbeModified && selected then 
                                let arrayBoxPlot, statsBoxPlot = allData.Item model.currBoxPlotAttrib
                                let hm = model.boxPlotProbes.Add(model.lastModifiedProbeIntId, arrayBoxPlot) 
                                let boxPlotData = hm |> HashMap.toSeq |> Seq.map (fun result -> snd result ) |> Seq.toArray
                                hm, boxPlotData
                            else 
                                model.boxPlotProbes, mTwoD.boxPlotData

                        let updatedTwoDmodel = 
                            { mTwoD with
                                sphereFilter = Some sphereTransformed
                                data = { version = mTwoD.data.version + 1; arr = filteredData}
                                attributeText = attributeAsString
                                boxPlotData = dataForBoxPlot
                                //boxPlotAttribute = renderValueToString newCurrBoxPlotAttrib
                            }

                        let sleepTime = computeSleepTime filteredData.Length

                        let threadsVr = 
                            proclist { 
                                do! Async.SwitchToThreadPool()
                                do! Async.Sleep sleepTime
                                // perform readback
                                let t = getScreenshot histogramClient
                                yield SetTexture(t, probe)
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
                            boxPlotProbes = newHashmap
                            //currBoxPlotAttribSet = true
                            //currBoxPlotAttrib = newCurrBoxPlotAttrib
                            twoDModel = updatedTwoDmodel
                            threads = ThreadPool.start threadsVr ThreadPool.empty}
                    | _ -> model 
                | ControllerMode.Ray ->     
                    match model.mainControllerId with
                    | Some i when i = id ->
                        client.Mouse.Up(model.screenCoordsHitPos, MouseButtons.Left)
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
                        | 1 -> closeMenu //if model.controllerMode = ControllerMode.Probe then goToNextMenu else closeMenu
                        | _ -> closeMenu
                    | _ -> model.menuLevel, model.mainMenuOpen
                else 
                    closeMenu
            let screenTex = if not isOpen then model.lastContrScreenModeTexture else model.mainContrScreenTexture
            {model with 
                menuLevel = level
                mainMenuOpen = isOpen}
        | OpenMainMenu id ->
            let texture, screenTexture =
                match model.menuLevel with
                | l when l = 1 -> if model.controllerMode = ControllerMode.NoneMode then (texture "initial"), (texture "select-tool") else model.lastTouchpadModeTexture, model.lastContrScreenModeTexture
                | l when l = 2 -> (texture "initial-attributes"), (texture "select-attribute") //TODO: will be changed
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
                        | t when t >= 0.0   && t < 60.0  -> ControllerMode.Clipping
                        | t when t >= 60.0  && t < 120.0 -> ControllerMode.Ray
                        | t when t >= 120.0 && t < 180.0 -> ControllerMode.Probe
                        | t when t >= 180.0 && t < 360.0 -> ControllerMode.Analyze
                        | _ -> model.controllerMode
                    else 
                        model.controllerMode
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
                    controllerMode = newContrlMode
                    showCurrBoxPlot = (newContrlMode = ControllerMode.Analyze)
                    mainTouchpadTexture = tex
                    lastTouchpadModeTexture = tex
                    mainContrScreenTexture = screenTexture
                    lastContrScreenModeTexture = screenTexture
                    rayActive = false
                    clippingActive = false}
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
                let newAttribute = computeNewAttribute model.currSecondTouchPadPos model.currBoxPlotAttrib
                let texture, screenTexture = computeNewAttributeTextures newAttribute

                if newAttribute <> model.currBoxPlotAttrib then 

                    let newHashmap = 
                        model.boxPlotProbes
                        |> HashMap.map (fun key array ->
                            let currProbe = 
                                model.allProbes
                                |> HashMap.filter (fun k probe -> 
                                    probe.selected && probe.numberId = key
                                )
                                |> HashMap.toSeq
                                |> Seq.exactlyOne
                                |> snd
                            let newArray, newStats = currProbe.allData.Item newAttribute
                            newArray                                
                        )
                       
                    let dataForBoxPlot = newHashmap |> HashMap.toSeq |> Seq.map (fun result -> snd result ) |> Seq.toArray

                    let updatedTwoDmodel = 
                        { mTwoD with
                            boxPlotData = dataForBoxPlot
                            boxPlotAttribute = renderValueToString newAttribute
                        }

                    {model with 
                        twoDModel = updatedTwoDmodel
                        boxPlotProbes = newHashmap
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
                let newAttribute = computeNewAttribute model.currSecondTouchPadPos model.attribute 
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
                        let newAttribute = computeNewAttribute model.currSecondTouchPadPos probe.currAttribute
                        let texture, screenTexture = computeNewAttributeTextures newAttribute//probe.currAttribute
                        let updatedProbe = {probe with currAttribute = newAttribute}
                        let update (pr : Option<Probe>) = updatedProbe 
                        let allProbesUpdated = model.allProbes |> HashMap.update probeId update
                        let filteredData, stats = probe.allData.Item newAttribute

                        let attributeAsText = renderValueToString newAttribute

                        let updatedTwoDmodel = 
                            { mTwoD with 
                                data = {version = mTwoD.data.version + 1; arr = filteredData}
                                attributeText = attributeAsText
                                }

                        let sleepTime = computeSleepTime filteredData.Length

                        let threadsVr = 
                            proclist { 
                                do! Async.SwitchToThreadPool()
                                do! Async.Sleep sleepTime
                                // perform readback
                                let t = getScreenshot histogramClient
                                yield SetTexture(t, updatedProbe)
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
            | Some i when i = id && model.rayActive && model.screenIntersection && not model.grabbingHera -> 
                client.Mouse.Scroll(model.screenCoordsHitPos, pos.Y * 50.0)
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
            let mainTouching = 
                match model.mainControllerId with
                | Some i when i = id -> true
                | _ -> model.mainTouching
            let secondTouching = 
                match model.secondControllerId with
                | Some i when i = id -> true
                | _ -> model.secondTouching
            {model with 
                mainTouching = mainTouching
                secondTouching = secondTouching}
             //{model with touchpadDeviceId = Some id} 
        | UntouchDevice id ->
            let mainTouching = 
                match model.mainControllerId with
                | Some i when i = id -> false
                | _ -> model.mainTouching
            let secondTouching = 
                match model.secondControllerId with
                | Some i when i = id -> false
                | _ -> model.secondTouching
            {model with 
                mainTouching = mainTouching
                secondTouching = secondTouching}
            //let tex = if model.allowHeraScaling then (texture "initial-scaling") else model.touchpadTexture
            //match model.touchpadDeviceId with 
            //| Some i when i = id -> 
            //    {model with touchpadDeviceId = None} 
            //| _ -> model
        | ResetHera -> initial runtime frames