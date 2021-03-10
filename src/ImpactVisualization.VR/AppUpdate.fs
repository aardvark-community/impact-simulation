namespace ImpactVisualization

open System
open System.IO
open Aardvark.Base
open Aardvark.Vr
open Aardvark.Application

open FSharp.Data.Adaptive

open AardVolume.Model
open AardVolume.App
open ImpactVisualization
open Aardvark.Cef

type ThreeDMessage = Nop

type Message =
    | ThreeD of ThreeDMessage
    | TwoD of AardVolume.Message 
    | Nop
    | StartVr
    | GrabHera of int
    | UngrabHera of int
    | ScaleHera of int * float
    | MoveController of int * Trafo3d
    | ToggleControllerMenu of int
    | OpenControllerMenu of int
    | ChangeTouchpadPos of int * V2d
    | ChangeControllerMode of int
    | SelectAttribute of int
    | ActivateControllerMode of int
    | CreateProbe of int * Trafo3d
    | CreateRay of int * Trafo3d
    | CreateClipping of int * Trafo3d
    | DeactivateControllerMode of int
    | UntouchDevice of int 
    | MouseClick
    | ResetHera 

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

    let allTextures = 
        HashMap.ofList ["initial", fromStreamToTexture "initial.png"; 
                        "left", fromStreamToTexture "left.png"; 
                        "middle", fromStreamToTexture "middle.png"; 
                        "right", fromStreamToTexture "right.png"; 
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
                        "ray", fromStreamToTexture "ray.png";
                        "clipping", fromStreamToTexture "clipping.png";
                        "select-attribute", fromStreamToTexture "select-attribute.png";
                        "select-tool", fromStreamToTexture "select-tool.png";
                        "empty", fromStreamToTexture "empty.png";
                        "grab-sphere", fromStreamToTexture "grab-sphere.png";
                        "scaling-up", fromStreamToTexture "scaling-up.png";
                        "scaling-down", fromStreamToTexture "scaling-down.png"]
    
    let trafoOrIdentity trafo = Option.defaultValue Trafo3d.Identity trafo

    let newOrOldTrafo (id : int) (trafo : Trafo3d) (contrId : Option<int>) (contrTrafo : Trafo3d) =
        match contrId with 
        | Some i when i = id -> trafo 
        | _ -> contrTrafo
    
    let idIsSet (contrId : Option<int>) = contrId.IsSome

    let sphereIntersection (controllerSphere : Sphere3d) (probe : Sphere3d) = controllerSphere.Intersects(probe)
    
    let createProbe (pos : V3d) (rad : float) (posToHera : V3d) (radToHera : float) (inside : bool) (statistics : string ) (billboard : bool): Probe = 
        {
            id = Guid.NewGuid().ToString()
            center = pos
            radius = rad
            centerRelToHera = posToHera
            radiusRelToHera = radToHera
            insideHera = inside
            currStatistics = statistics
            showBillboard = billboard
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

    let initial runtime frames = 
        {   
            twoDModel = AardVolume.App.initial frames
            text = "hello" 
            devicesTrafos = HashMap.Empty
            controllerTrafo = Trafo3d.Identity
            heraTrafo = Trafo3d.Identity
            heraToControllerTrafo = Trafo3d.Identity
            grabberId = None
            allowHeraScaling = false
            sphereControllerTrafo = Trafo3d.Identity
            sphereControllerId = None
            sphereScalerTrafo = Trafo3d.Identity
            sphereScalerId = None
            scalingFactorHera = 0.05
            sphereScale = 1.0
            lastSphereScale = 1.0
            sphereRadius = 0.2
            sphereColor = C4b(1.0,1.0,1.0,0.4)
            currentProbeManipulated = false
            currentProbe = None
            allProbes = HashMap.Empty
            probeIntersectionId = None
            intersectionControllerId = None
            deletionControllerId = None
            lastFilterProbe = None
            statistics = ""
            rayDeviceId = None
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
            clippingPlaneDeviceTrafo = Trafo3d.Identity
            clippingPlaneDeviceId = None
            planeCorners = Quad3d(V3d(), V3d(), V3d(), V3d())
            controllerMenuOpen = false
            menuControllerTrafo = Trafo3d.Identity
            menuControllerId = None
            menuLevel = 0
            controllerMode = ControllerMode.NoneMode
            attribute = RenderValue.NoValue
            currTouchPadPos = V2d.OO
            touchpadDeviceId = None
            touchpadDeviceTrafo = Trafo3d.Identity
            touchpadTexture = FileTexture(texturesPath + "initial.png", true) :> ITexture
            lastTouchpadModeTexture = FileTexture(texturesPath + "initial.png", true) :> ITexture
            contrScreenTexture = FileTexture(texturesPath + "empty.png", true) :> ITexture
            lastContrScreenModeTexture = FileTexture(texturesPath + "empty.png", true) :> ITexture
            textureDeviceTrafo = Trafo3d.Identity
            showTexture = false
            heraBox = Box3d.Infinite
            heraTransformations = Trafo3d(Scale3d(0.05)) * Trafo3d.Translation(0.0, 0.0, 0.7)
        }

    let rec update (runtime : IRuntime) (client : Browser) (frames : Frame[]) (state : VrState) (vr : VrActions) (model : Model) (msg : Message) =
        let mTwoD = model.twoDModel

        let getTexture tex = Option.defaultValue model.touchpadTexture tex
        let texture tex = allTextures.TryFind(tex) |> getTexture    

        let hmdPos = state.display.pose.deviceToWorld.GetModelOrigin()

        //printf "VR STATE: %A \n" state

        match msg with
        | ThreeD _ -> model
        | Nop -> model
        | TwoD msg -> 
            let sup = AardVolume.App.update frames model.twoDModel msg
            { model with twoDModel = sup }
        | StartVr -> vr.start(); model
        | GrabHera id ->
            match model.grabberId with 
            | Some i -> model
            | None -> 
                let currentContrTr = model.devicesTrafos.TryFind(id)
                let controlT = trafoOrIdentity currentContrTr
                let heraT = model.heraTrafo
                let controlHeraT = heraT * controlT.Inverse
                {model with 
                    grabberId = Some id
                    controllerTrafo = controlT
                    heraToControllerTrafo = controlHeraT
                    allowHeraScaling = true
                    textureDeviceTrafo = controlT
                    touchpadTexture = texture "initial-scaling"
                    contrScreenTexture = texture "empty"
                    showTexture = true
                    menuLevel = 0
                    controllerMenuOpen = false}
        | UngrabHera id -> 
            let controlT = model.controllerTrafo
            let heraToControlT = model.heraToControllerTrafo
            let heraT = heraToControlT * controlT
            match model.grabberId with 
            | Some i when i = id -> 
                {model with 
                    grabberId = None
                    heraTrafo = heraT
                    allowHeraScaling = false
                    showTexture = false
                    contrScreenTexture = texture "empty"}
            | _ -> model
        | ScaleHera (id, f) ->
           // printf "SCALE HERA \n"
            match model.grabberId with 
            | Some i when i = id && model.allowHeraScaling ->
                if model.touchpadDeviceId.IsSome then
                    if f >= 0.0 then
                        let maxScale = 1.0
                        let currScale = model.scalingFactorHera * (f*f/5.0 + 1.0)
                        let newScale = if currScale >= maxScale then maxScale else currScale
                        {model with 
                            scalingFactorHera = newScale
                            touchpadTexture = texture "scale-up"
                            contrScreenTexture = texture "scaling-up"}
                    else
                        let minScale = 0.001
                        let currScale = model.scalingFactorHera * (1.0 - f*f/5.0)
                        let newScale =  if currScale <= minScale then minScale else currScale
                        {model with 
                            scalingFactorHera = newScale
                            touchpadTexture = texture "scale-down"
                            contrScreenTexture = texture "scaling-down"}
                else 
                    {model with 
                        touchpadTexture = texture "initial-scaling"
                        contrScreenTexture = texture "empty"}
            | _ -> model
        | MoveController (id, (trafo : Trafo3d)) -> 
            //printf "MOVE \n"

            let newInput = model.devicesTrafos.Add(id, trafo)

            //CURRENT SPHERE UPDATE
            let sphereContrTrafo = newOrOldTrafo id trafo model.sphereControllerId model.sphereControllerTrafo
            let sphereScTrafo = 
                match model.sphereScalerId with 
                | Some i when i = id && model.sphereControllerId.IsSome -> trafo 
                | _ -> model.sphereScalerTrafo
            let scalingFactor = 
                if not (idIsSet model.sphereControllerId && idIsSet model.sphereScalerId) then
                    model.sphereScale
                else 
                    let mainContrTrafo = model.sphereControllerTrafo 
                    let scaleContrTrafo = model.sphereScalerTrafo
                    let contrPos = mainContrTrafo.Forward.TransformPos(V3d.OOO)
                    let scalerPos = scaleContrTrafo.Forward.TransformPos(V3d.OOO)
                    let distance = contrPos.Distance(scalerPos)
                    let newScale = (4.0/3.0)*Math.PI*Math.Pow((distance + model.sphereRadius), 3.0)
                    newScale + model.sphereRadius/2.0
            
            //RAY UPDATE
            let initRay = 
                match model.rayDeviceId with    
                | Some id ->
                    let currDeviceTrafo = trafoOrIdentity (model.devicesTrafos.TryFind(id))
                    let origin = currDeviceTrafo.Forward.TransformPos(V3d(0.0, 0.02, 0.0))
                    let direction = currDeviceTrafo.Forward.TransformPos(V3d.OIO * 1000.0)
                    Ray3d(origin, direction)
                | None -> Ray3d.Invalid    
            let intersect = initRay.Intersects(model.tvQuad)
            let mutable hit = RayHit3d.MaxRange
            let hitPoint = initRay.Hits(model.tvQuad, &hit)
            let currRay, rColor, screenCoordsHitPos =
                if intersect then
                    let screenCoords = (V2d(hit.Coord.Y * screenResolution.ToV2d().X, hit.Coord.X * screenResolution.ToV2d().Y)).ToV2i()
                    let screenPos = PixelPosition(screenCoords, screenResolution.X, screenResolution.Y)
                    if model.rayTriggerClicked then
                        printf "client MOVE \n" 
                        client.Mouse.Move(screenPos)
                    //printf "client Focus TRUE \n" 
                    //client.SetFocus true
                    Ray3d(initRay.Origin, hit.Point), C4b.Green, screenPos
                else
                   // printf "client Focus FALSE" 
                    //
                   // client.SetFocus false
                    initRay, C4b.Red, PixelPosition()
    
            //HERA TRANSFORMATIONS UPDATE
            let heraContrTrafo = newOrOldTrafo id trafo model.grabberId model.controllerTrafo
            let heraTrafos = 
                if model.grabberId.IsSome then
                    let heraTrafo = model.heraToControllerTrafo * heraContrTrafo
                    let heraScaleTrafo = Trafo3d(Scale3d(model.scalingFactorHera))
                    let heraTranslation = Trafo3d.Translation(0.0, 0.0, 0.7)
                    heraScaleTrafo * heraTranslation * heraTrafo
                else 
                    model.heraTransformations
            let heraBBox = model.twoDModel.currHeraBBox.Transformed(heraTrafos)

            //INTERSECTION OF CONTROLLER WITH A PROBE
            let probeIntersection = 
                match model.intersectionControllerId with 
                | Some i when i = id && not model.currentProbeManipulated -> 
                        let intersectionContrTrafo = trafoOrIdentity (model.devicesTrafos.TryFind(i))
                        let intersectionContrTrafoPos = intersectionContrTrafo.Forward.TransformPos(V3d(0.0, 0.0, 0.0))
                        let controllerSphere = Sphere3d(intersectionContrTrafoPos, 0.05)
                        model.allProbes
                        |> HashMap.filter (fun key probe ->
                            let sphere = Sphere3d(probe.center, probe.radius)
                            sphereIntersection controllerSphere sphere)
                        |> HashMap.toSeq
                        |> Seq.map (fun (key, probe) -> key)
                        |> Seq.tryLast    
                | None -> None
                | _ -> model.probeIntersectionId

            //let screenTex = if probeIntersection.IsSome then texture "grab-sphere" else model.contrScreenTexture

            //CLIPPING PLANE UPDATE
            let clippingContrTrafo = newOrOldTrafo id trafo model.clippingPlaneDeviceId model.clippingPlaneDeviceTrafo
            let currCorners = 
                match model.clippingPlaneDeviceId with
                | Some id ->
                    let p0 = clippingContrTrafo.Forward.TransformPos(planePos0)
                    let p1 = clippingContrTrafo.Forward.TransformPos(planePos1)
                    let p2 = clippingContrTrafo.Forward.TransformPos(planePos2)
                    let p3 = clippingContrTrafo.Forward.TransformPos(planePos3)
                    Quad3d(p0, p1, p2, p3)
                | None -> model.planeCorners

            let mutable copyOfAllProbes = model.allProbes //TODO: At each iteration the showBillboard must be false!!!

            if model.allProbes.Count >= 2 then
                model.allProbes
                |> HashMap.iter (fun key probe -> 
                    let temp = 
                        model.allProbes
                        |> HashMap.map (fun k p ->
                            if k = key || p.showBillboard = false then
                               p
                            else 
                                //TODO: Convert it into screenspace!!!!!!!!
                                //let currProbeCenter = probe.center
                                //let probeCenter = p.center
                                let distance = probe.center.Distance(p.center)
                                let allowedMaxDistance = 0.75 * (probe.radius + p.radius)
                                let newVisibility = 
                                    if (distance < allowedMaxDistance) then
                                        let depthProbe = hmdPos.Distance(probe.center)
                                        let depthP = hmdPos.Distance(p.center)
                                        if depthProbe >= depthP then true else false 
                                    else
                                        true
                                {p with showBillboard = newVisibility}
                        )
                    copyOfAllProbes <- temp
                )
            

                //if model.allProbes.Count > 2 then
                //    let mutable copyOfAllProbes = model.allProbes
                //    while (not copyOfAllProbes.IsEmpty) do 
                //        let keyFirst, pFirst = copyOfAllProbes |> Seq.head
                //        copyOfAllProbes <- (copyOfAllProbes.Remove(keyFirst))
                //        copyOfAllProbes
                //        |> HashMap.map (fun key probe -> 
                //            let newProbeValue = {probe with showBillboard = false}
                //            model.allProbes.Add(key, newProbeValue)
                //            )
                //        |> ignore


            //PROBES UPDATE
            let allProbesUpdated =
                model.allProbes
                |> HashMap.map (fun key probe -> 
                    let billboardVisible = copyOfAllProbes.Item(key).showBillboard
                    if model.grabberId.IsSome && not model.allProbes.IsEmpty then 
                        let newCenter = heraTrafos.Forward.TransformPos(probe.centerRelToHera)
                        let newRadius = probe.radiusRelToHera * heraTrafos.Forward.GetScaleVector3().X
                        let sphere = Sphere3d(newCenter, newRadius)
                        let intersection = heraBBox.Intersects(sphere)
                        { probe with 
                            center = newCenter
                            radius = newRadius
                            insideHera = intersection
                            showBillboard = billboardVisible
                        }
                    else 
                        {probe with showBillboard = billboardVisible}
                )


            //MENU, TOUCHPAD AND TEXTURES TRAFOS
            let currMenuTrafo = newOrOldTrafo id trafo model.menuControllerId model.menuControllerTrafo
            let newTouchpadDeviceTrafo = newOrOldTrafo id trafo model.touchpadDeviceId model.touchpadDeviceTrafo 
            let newTextureDeviceTrafo = 
                if model.controllerMenuOpen then currMenuTrafo
                else if model.allowHeraScaling then heraContrTrafo
                else model.textureDeviceTrafo

            {model with 
                controllerTrafo = heraContrTrafo
                sphereControllerTrafo = sphereContrTrafo
                sphereScalerTrafo = sphereScTrafo
                sphereScale = scalingFactor
                devicesTrafos = newInput
                ray = currRay
                clippingPlaneDeviceTrafo = clippingContrTrafo
                planeCorners = currCorners
                screenIntersection = intersect
                menuControllerTrafo = currMenuTrafo
                rayColor = rColor
                hitPoint = hit.Point
                screenHitPoint = hit.Coord
                screenCoordsHitPos = screenCoordsHitPos
                probeIntersectionId = probeIntersection
                //contrScreenTexture = screenTex
                heraBox = heraBBox
                allProbes = allProbesUpdated
                heraTransformations = heraTrafos
                touchpadDeviceTrafo = newTouchpadDeviceTrafo
                textureDeviceTrafo = newTextureDeviceTrafo}
        | ActivateControllerMode id ->
            //printf "VR STATE: %A \n" state.devices
            
            //let device = 
            //    state.devices 
            //    |> HashMap.tryFind id
            //match device with
            //| Some d ->
            //    printfn "vibrate: %A" id
            //    d.startVibrate (MicroTime.FromSeconds 0.1)
            //| _ -> ()
            
            let currDeviceTrafo = trafoOrIdentity (model.devicesTrafos.TryFind(id))
            if not model.controllerMenuOpen then 
                match model.controllerMode with
                | ControllerMode.Probe -> update runtime client frames state vr model (CreateProbe (id, currDeviceTrafo))
                | ControllerMode.Ray -> update runtime client frames state vr model (CreateRay (id, currDeviceTrafo))
                | ControllerMode.Clipping -> update runtime client frames state vr model (CreateClipping (id, currDeviceTrafo))
                | _ -> model
            else 
                { model with 
                    sphereControllerId = None
                    sphereScalerId = None
                    rayDeviceId = None
                    clippingPlaneDeviceId = None 
                }
        | CreateProbe (id, trafo) ->
            match model.probeIntersectionId with // is the controller currently intersecting a probe (id of probe is a string)
            | Some probe -> 
                match model.intersectionControllerId with // id of the controller intersecting with sphere
                | Some i when i = id -> 
                    if model.deletionControllerId.IsSome then // if deletion controller is set then DELETE current probe with main controller 
                        //TODO: Check if the deleted probe is the last one used for filtering
                        let filterProbe = 
                            match model.lastFilterProbe with 
                                | Some pr when pr.id <> probe -> model.lastFilterProbe
                                | _ -> None
                        let updatedTwoDmodel = 
                            { mTwoD with
                                sphereFilter = None
                                data = VersionedArray.ofArray [||] 
                                dataRange = mTwoD.initDataRange
                            }
                        {model with 
                            allProbes = model.allProbes.Remove(probe)
                            lastFilterProbe = filterProbe
                            twoDModel = updatedTwoDmodel
                            } 
                    else // if deletion controller is not set -> MANIPULATE probe
                        let currProbe = model.allProbes.TryFind(probe)
                        let currScale = 
                            match currProbe with
                            | Some pr -> (pr.radius / model.sphereRadius)
                            | None -> model.sphereScale
                        {model with 
                            allProbes = model.allProbes.Remove(probe)
                            lastFilterProbe = currProbe
                            currentProbeManipulated = true
                            sphereControllerTrafo = trafo
                            sphereControllerId = Some id
                            sphereScale = currScale
                            probeIntersectionId = None}
                | None -> {model with deletionControllerId = None} // if no controller is intersectiong, then the probe cannot be deleted
                | _ -> {model with deletionControllerId = Some id} // if probe is interseecting but current controller is not the same as intersection controller -> set deletion controller
            | None -> // controller is not intersecting with a probe
                match model.sphereControllerId with // is the id of the controller creating spheres set 
                | Some i when i <> id -> 
                    {model with // if not the same id -> sphere shold be scaled 
                        currentProbeManipulated = true
                        sphereScalerId = Some id
                        sphereScalerTrafo = trafo} 
                | _ -> // if the current id is the same as the previous -> set new trafos for the sphere
                    {model with 
                        currentProbeManipulated = true
                        sphereControllerTrafo = trafo
                        sphereControllerId = Some id
                        intersectionControllerId = Some id}
        | CreateRay (id, trafo) ->
            match model.rayDeviceId with 
            | Some i when i = id -> 
                printf "client Mouse DOWN" 
                client.Mouse.Down(model.screenCoordsHitPos, MouseButtons.Left)
               // if model.screenIntersection then client.SetFocus true
                {model with 
                    rayTriggerClicked = true
                    clickPosition = Some model.screenHitPoint}
            | None ->
                let origin = trafo.Forward.TransformPos(V3d(0.0, 0.02, 0.0))
                let direction = trafo.Forward.TransformPos(V3d.OIO * 1000.0) 
                {model with 
                    rayDeviceId = Some id
                    ray = Ray3d(origin, direction)}
            | _ -> model
        | CreateClipping (id, trafo) ->
            let p0 = trafo.Forward.TransformPos(planePos0)
            let p1 = trafo.Forward.TransformPos(planePos1)
            let p2 = trafo.Forward.TransformPos(planePos2)
            let p3 = trafo.Forward.TransformPos(planePos3)
            match model.clippingPlaneDeviceId with 
            | Some i when i = id -> 
                {model with 
                    clippingPlaneDeviceTrafo = trafo
                    clippingPlaneDeviceId = Some id}
            | None -> 
                {model with 
                    planeCorners = Quad3d(p0, p1, p2, p3)
                    clippingPlaneDeviceTrafo = trafo
                    clippingPlaneDeviceId = Some id}
            | _ -> model
        | DeactivateControllerMode id ->
            if not model.controllerMenuOpen then 
                match model.controllerMode with 
                | ControllerMode.Probe -> 
                    let delControllerId = 
                        match model.deletionControllerId with 
                        | Some i -> if i = id then None else model.deletionControllerId
                        | None -> None
                    match model.sphereControllerId with
                    | Some i when i = id -> 
                        let t = model.sphereControllerTrafo
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
                                    
                        let array, stats = filterDataForOneFrameSphere frames.[mTwoD.frame] (Some sphereTransformed) model.attribute
                        let probe = createProbe spherePos sphereRadius spherePosTransformed radiusTransformed intersection stats true

                        // printf "Statistics: \n %A" stats
                        let filteredData = if intersection then array else mTwoD.data.arr

                        let updatedTwoDmodel = 
                            { mTwoD with
                                sphereFilter = Some sphereTransformed
                                data = { version = mTwoD.data.version + 1; arr = filteredData}
                            }
                        {model with
                            currentProbeManipulated = false
                            allProbes = model.allProbes.Add(probe.id, probe)
                            lastSphereScale = model.sphereScale
                            lastFilterProbe = Some probe
                            statistics = stats
                            sphereScale = 1.0
                            sphereControllerId = None
                            sphereScalerId = None
                            deletionControllerId = delControllerId
                            twoDModel = updatedTwoDmodel}
                    | _ -> {model with 
                                    sphereScalerId = None
                                    deletionControllerId = delControllerId}
                | ControllerMode.Ray ->     
                    match model.rayDeviceId with
                    | Some i when i = id ->
                        printf "client Mouse UP + CLICK \n" 
                        client.Mouse.Up(model.screenCoordsHitPos, MouseButtons.Left)
                        //client.Mouse.Click(model.screenCoordsHitPos, MouseButtons.Left)
                       // client.SetFocus false
                      //  let temp = client.Execute "document.activeElement.blur();"
                        {model with 
                            rayTriggerClicked = false
                            clickPosition = None}
                    | _ -> model
                | ControllerMode.Clipping -> 
                    match model.clippingPlaneDeviceId with 
                    | Some i when i = id -> {model with clippingPlaneDeviceId = None}
                    | _ -> model
                | _ -> model
            else 
                { model with 
                    sphereControllerId = None
                    sphereScalerId = None
                    rayDeviceId = None
                    clippingPlaneDeviceId = None 
                }
        | ToggleControllerMenu id -> 
            let r, theta = convertCartesianToPolar model.currTouchPadPos
            let level, isOpen = 
                let closeMenu = 0, false
                let goToNextMenu = (model.menuLevel + 1), true
                match model.menuControllerId with 
                | Some i when i = id ->
                    match model.menuLevel with
                    | 0 -> goToNextMenu
                    | 1 -> if model.controllerMode = ControllerMode.Probe then goToNextMenu else closeMenu
                    | _ -> closeMenu
                | _ -> 1, true
            let screenTex = if not isOpen then (texture "empty") else model.contrScreenTexture
            {model with 
                menuLevel = level
                controllerMenuOpen = isOpen
                showTexture = isOpen
                contrScreenTexture = screenTex
                sphereControllerId = None
                sphereScalerId = None
                rayDeviceId = None
                clippingPlaneDeviceId = None }
        | OpenControllerMenu id ->
            let currDeviceTrafo = trafoOrIdentity (model.devicesTrafos.TryFind(id))
            let r, theta = convertCartesianToPolar model.currTouchPadPos
            let texture, screenTexture =
                match model.menuLevel with
                | l when l = 1 -> if model.controllerMode = ControllerMode.NoneMode then (texture "initial"), (texture "select-tool") else model.lastTouchpadModeTexture, model.lastContrScreenModeTexture
                | l when l = 2 -> (texture "initial-attributes"), (texture "select-attribute")
                | _ -> model.touchpadTexture, model.contrScreenTexture
            if model.controllerMenuOpen then //TODO: Handle the case when opening the menu with the first controller and clicking on the second controller
                {model with 
                    menuControllerTrafo = currDeviceTrafo
                    menuControllerId = Some id
                    textureDeviceTrafo = currDeviceTrafo
                    touchpadTexture = texture
                    contrScreenTexture = screenTexture}
            else    
                {model with menuControllerId = None}
        | ChangeControllerMode id -> 
            match model.menuControllerId with  
            | Some i when i = id && model.menuLevel = 1 && not model.allowHeraScaling -> 
                let r, theta = convertCartesianToPolar model.currTouchPadPos
                let newContrlMode = 
                    if r >= 0.5 then 
                        match theta with 
                        | t when t >= 0.0   && t < 60.0  -> ControllerMode.Clipping
                        | t when t >= 60.0  && t < 120.0 -> ControllerMode.Ray
                        | t when t >= 120.0 && t < 180.0 -> ControllerMode.Probe
                        | _ -> model.controllerMode
                    else 
                        model.controllerMode
                let texture, screenTexture = 
                    match model.controllerMode with 
                    | m when m = newContrlMode -> model.touchpadTexture, model.contrScreenTexture // if the  controller mode is the same then texture should not be loaded again   
                    | _ ->
                        let texName, screenTexName = 
                            match newContrlMode with
                            | ControllerMode.Probe -> "left", "probe" 
                            | ControllerMode.Ray -> "middle", "ray" 
                            | ControllerMode.Clipping -> "right", "clipping"
                            | _ -> "initial", "select-tool"
                        (texture texName), (texture screenTexName)
                {model with 
                    controllerMode = newContrlMode
                    touchpadTexture = texture
                    lastTouchpadModeTexture = texture
                    contrScreenTexture = screenTexture
                    lastContrScreenModeTexture = screenTexture}
            | _ -> model
        | SelectAttribute id ->
            match model.menuControllerId with  
            | Some i when i = id && model.menuLevel = 2 && not model.allowHeraScaling -> 
                let r, theta = convertCartesianToPolar model.currTouchPadPos
                let newAttribute = 
                    if r >= 0.5 then 
                        match theta with 
                        | t when t >= 0.0   && t < 60.0  -> RenderValue.Energy
                        | t when t >= 60.0  && t < 120.0 -> RenderValue.CubicRoot
                        | t when t >= 120.0 && t < 180.0 -> RenderValue.Strain
                        | t when t >= 180.0 && t < 240.0 -> RenderValue.AlphaJutzi
                        | t when t >= 240.0 && t < 300.0 -> RenderValue.Pressure
                        | t when t >= 300.0 && t < 360.0 -> RenderValue.Density
                        | _ -> model.attribute
                    else 
                        model.attribute

                let texture, screenTexture = 
                    match model.attribute with 
                    | a when a = newAttribute -> model.touchpadTexture, model.contrScreenTexture // if the attribute is the same then texture should not be loaded again   
                    | _ ->
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
                {model with 
                    attribute = newAttribute
                    touchpadTexture = texture
                    contrScreenTexture = screenTexture}
                | _ -> model
        | ChangeTouchpadPos (id, pos) -> 
           // printf "CHANGE TOUCHPAD POS %A \n" pos
            let currTouchDevice = model.devicesTrafos.TryFind(id)
            match model.rayDeviceId with 
            | Some i when i = id && model.screenIntersection -> 
                printf "client SCROLL \n" 
                client.Mouse.Scroll(model.screenCoordsHitPos, pos.Y * 50.0)
            | _ -> ()
            let newId = if pos.X = 0.0 && pos.Y = 0.0 then None else Some id //when both X and Y are equal to 0.0 it means we are currently not touching the touchpad
            {model with 
                currTouchPadPos = pos
                touchpadDeviceId = newId
                touchpadDeviceTrafo = trafoOrIdentity currTouchDevice}
        | UntouchDevice id ->
            let tex = if model.allowHeraScaling then (texture "initial-scaling") else model.touchpadTexture
            match model.touchpadDeviceId with 
            | Some i when i = id -> 
                {model with 
                    touchpadDeviceId = None
                    touchpadTexture = tex} 
            | _ -> model
        | ResetHera -> initial runtime frames