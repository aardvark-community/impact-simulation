namespace ImpactVisualization

open System
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
open Aardvark.Cef

type ThreeDMessage = Nop

type Message =
    | ThreeD of ThreeDMessage
    | TwoD of AardVolume.Message 
    | Nop
    | StartVr
    | GrabHera of int
    | UngrabHera of int
    | ScaleUpHera of float 
    | ScaleDownHera of float 
    | MoveController of int * Trafo3d
    | ToggleControllerMenu
    | OpenControllerMenu of int
    | ChangeTouchpadPos of int * V2d
    | ChangeControllerMode of int
    | ActivateControllerMode of int
    | CreateProbe of int * Option<Trafo3d>
    | CreateRay of int * Trafo3d
    | CreateClipping of int * Option<Trafo3d> * Trafo3d
    | DeactivateControllerMode of int
    | UntouchDevice of int 
    | MouseClick
    | ResetHera 
 
module Demo =
    open Aardvark.UI.Primitives
    open Aardvark.Base.Rendering

    let quadRightUp =   V3d(0.732, 0.432, -0.013)
    let quadLeftUp =    V3d(-0.732, 0.432, -0.013)
    let quadLeftDown =  V3d(-0.732, -0.41483, -0.013)
    let quadRightDown = V3d(0.732, -0.41483, -0.013)

    let flatScreenTrafo rotY rotZ upZ = 
        let scale = Trafo3d(Scale3d(2.0))
        let rotation = Trafo3d.RotationEulerInDegrees(90.0, rotY, rotZ)
        let translation = Trafo3d.Translation(2.5, 1.0, upZ)
        scale * rotation * translation

    let tvQuadTrafo = flatScreenTrafo 180.0 90.0 1.535
    let tvTrafo = flatScreenTrafo 0.0 -90.0 1.5

    let screenResolution = V2i(1008,729)

    let texturesPath = @"..\..\..\src\ImpactVisualization\resources\textures\"

    let initial runtime frames = 
        {   
            twoDModel = AardVolume.App.initial frames
            text = "hello" 
            devicesTrafos = HashMap.Empty
            controllerTrafo = None
            heraTrafo = None
            heraToControllerTrafo = None
            grabberId = None
            allowHeraScaling = false
            sphereControllerTrafo = None
            sphereControllerId = None
            sphereScalerTrafo = None
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
            //tvQuad = Quad3d(V3d(-25,-20,-8), V3d(-25,10,-8), V3d(-25,10,12), V3d(-25,-20,12))
            //tvQuad = Quad3d(V3d(0.732, 0.432, 0.013), V3d(-0.732, 0.432, 0.013), V3d(-0.732, -0.41483, 0.013), V3d(0.732, -0.41483, 0.013))
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
            clippingPlaneDeviceTrafo = None
            clippingPlaneDeviceId = None
            planeCorners = Quad3d(V3d(), V3d(), V3d(), V3d())
            controllerMenuOpen = false
            menuControllerTrafo = None
            menuControllerId = None
            controllerMode = ControllerMode.Probe
            currTouchPadPos = V2d.OO
            touchpadDeviceId = None
            touchpadDeviceTrafo = None
            touchpadTexture = "initial.png"
            heraBox = Box3d.Infinite
            heraTransformations = Trafo3d.Identity
        }

    //let updateController (m : Model) : Model = 
    //    failwith ""

    let createProbe (pos : V3d) (rad : float) (posToHera : V3d) (radToHera : float) (inside : bool) (statistics : string )  : Probe = 
        {
            id = Guid.NewGuid().ToString()
            center = pos
            radius = rad
            centerRelToHera = posToHera
            radiusRelToHera = radToHera
            insideHera = inside
            currStatistics = statistics
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
        
    let rec update (runtime : IRuntime) (client : Browser) (frames : Frame[]) (state : VrState) (vr : VrActions) (model : Model) (msg : Message) =
        let planePos0 = V3d(-0.7, 0.05, -0.5)
        let planePos1 = V3d(-0.7, 0.05, 0.5)
        let planePos2 = V3d(0.7, 0.05, 0.5)

        let planePos3 = V3d(0.7, 0.05, -0.5)

        //let res = model.client.LoadUrl "http://localhost:4321"
        let mTwoD = model.twoDModel


        let trafoOrIdentity trafo = 
            match trafo with 
            | Some t -> t
            | None -> Trafo3d.Identity

        let newOrOldTrafo (id : int) (trafo : Trafo3d) (contrId : Option<int>) (contrTrafo : Option<Trafo3d>) =
            match contrId with 
            | Some i -> if i = id then Some(trafo) else contrTrafo
            | None -> contrTrafo

        let idIsSet (contrId : Option<int>) = 
            match contrId with
            | Some id -> true
            | None -> false
            
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
                    let heraT = trafoOrIdentity model.heraTrafo
                    let controlHeraT = heraT * controlT.Inverse
                    {model with 
                                grabberId = Some id
                                controllerTrafo = currentContrTr
                                heraToControllerTrafo = Some(controlHeraT)
                                allowHeraScaling = true}
        | UngrabHera id -> 
            match model.grabberId with 
                | Some i -> 
                    let controlT = trafoOrIdentity model.controllerTrafo
                    let heraToControlT = trafoOrIdentity model.heraToControllerTrafo
                    let heraT = heraToControlT * controlT
                    if i = id then 
                        {model with 
                            grabberId = None
                            heraTrafo = Some(heraT)
                            allowHeraScaling = false}
                    else model
                | None -> model
        | ScaleUpHera f -> 
            let maxScale = 1.0
            let currScale = if model.allowHeraScaling then model.scalingFactorHera * (f*f/5.0 + 1.0) else model.scalingFactorHera
            let newScale = if currScale >= maxScale then maxScale else currScale
            {model with scalingFactorHera = newScale}
        | ScaleDownHera f -> 
            let minScale = 0.001
            let currScale = if model.allowHeraScaling then model.scalingFactorHera * (1.0 - f*f/5.0) else model.scalingFactorHera
            let newScale = if currScale <= minScale then minScale else currScale
            {model with scalingFactorHera = newScale}
        | MoveController (id, (trafo : Trafo3d)) -> 
            let newInput = model.devicesTrafos.Add(id, trafo)
            let contrTrafo = newOrOldTrafo id trafo model.grabberId model.controllerTrafo
            let sphereContrTrafo = newOrOldTrafo id trafo model.sphereControllerId model.sphereControllerTrafo
            let sphereScTrafo = 
                if model.sphereControllerId.IsSome then 
                    newOrOldTrafo id trafo model.sphereScalerId model.sphereScalerTrafo
                else model.sphereScalerTrafo
            let scalingFactor = 
                if not (idIsSet model.sphereControllerId && idIsSet model.sphereScalerId) then
                    model.sphereScale
                else 
                    let mainContrTrafo = trafoOrIdentity model.sphereControllerTrafo 
                    let scaleContrTrafo = trafoOrIdentity model.sphereScalerTrafo
                    let contrPos = mainContrTrafo.Forward.TransformPos(V3d.OOO)
                    let scalerPos = scaleContrTrafo.Forward.TransformPos(V3d.OOO)
                    let distance = contrPos.Distance(scalerPos)
                    let newScale = (4.0/3.0)*Math.PI*Math.Pow((distance + model.sphereRadius), 3.0)
                    newScale + model.sphereRadius/2.0
            //printf "scale %f \n" scalingFactor
            let initRay = 
                match model.rayDeviceId with    
                | Some id ->
                    let currDevice = model.devicesTrafos.TryFind(id)
                    let currDeviceTrafo = trafoOrIdentity currDevice
                    let origin = currDeviceTrafo.Forward.TransformPos(V3d(0.0, 0.02, 0.0))
                    let direction = currDeviceTrafo.Forward.TransformPos(V3d.OIO * 1000.0)
                    Ray3d(origin, direction)
                | None -> Ray3d.Invalid    
            let intersect = initRay.Intersects(model.tvQuad)
            let mutable hit = RayHit3d.MaxRange
            let hitPoint = initRay.Hits(model.tvQuad, &hit)
            let currRay =
                if intersect then
                    client.SetFocus true
                    Ray3d(initRay.Origin, hit.Point)
                else
                    client.SetFocus false
                    initRay
           // if hitPoint then printf "Hit Point %A \n" hit.Point

            let heraTrafo = 
                let cT = trafoOrIdentity contrTrafo 
                let heraCT = trafoOrIdentity model.heraToControllerTrafo
                heraCT * cT
            let heraScaleTrafo = Trafo3d(Scale3d(model.scalingFactorHera))
            let heraTranslation = Trafo3d.Translation(0.0, 0.0, 0.7)
            let heraTrafos = heraScaleTrafo * heraTranslation * heraTrafo

            let heraBBox = model.twoDModel.currHeraBBox.Transformed(heraTrafos)

            let sphereIntersection (controllerSphere : Sphere3d) (probe : Sphere3d) = controllerSphere.Intersects(probe)
            let probeIntersection = 
                match model.intersectionControllerId with 
                | Some i ->
                    if i = id && not model.currentProbeManipulated then 
                        let intersectionContrTrafo = trafoOrIdentity (model.devicesTrafos.TryFind(i))
                        let intersectionContrTrafoPos = intersectionContrTrafo.Forward.TransformPos(V3d(0.0, 0.0, 0.0))
                        let controllerSphere = Sphere3d(intersectionContrTrafoPos, 0.05)
                        let probeId =  
                            model.allProbes
                            |> HashMap.filter (fun key probe ->
                                //let newCenter = heraTrafos.Forward.TransformPos(probe.centerRelToHera)
                                //let newRadius = probe.radiusRelToHera * heraTrafos.Forward.GetScaleVector3().X
                                let sphere = Sphere3d(probe.center, probe.radius)
                                sphereIntersection controllerSphere sphere)
                            |> HashMap.toSeq
                            |> Seq.map (fun (key, probe) -> key)
                        if probeId.IsEmpty() then 
                            None 
                        else 
                            Some (Seq.last probeId)
                    else
                        model.probeIntersectionId
                | None -> None
            let rColor = 
                if intersect then C4b.Green else C4b.Red
            let screenCoordsHitPos = 
                if intersect then
                    let screenCoords = (V2d(hit.Coord.Y * screenResolution.ToV2d().X, hit.Coord.X * screenResolution.ToV2d().Y)).ToV2i()
                    let screenPos = PixelPosition(screenCoords, screenResolution.X, screenResolution.Y)
                    if model.rayTriggerClicked then client.Mouse.Move(screenPos)
                    screenPos
                else 
                    PixelPosition()
            let clippingContrTrafo = newOrOldTrafo id trafo model.clippingPlaneDeviceId model.clippingPlaneDeviceTrafo
            let currCorners = 
                match model.clippingPlaneDeviceId with
                | Some id ->
                    let currDeviceTrafo = trafoOrIdentity clippingContrTrafo
                    let p0 = currDeviceTrafo.Forward.TransformPos(planePos0)
                    let p1 = currDeviceTrafo.Forward.TransformPos(planePos1)
                    let p2 = currDeviceTrafo.Forward.TransformPos(planePos2)
                    let p3 = currDeviceTrafo.Forward.TransformPos(planePos3)
                    Quad3d(p0, p1, p2, p3)
                | None -> model.planeCorners
            let currMenuTrafo = newOrOldTrafo id trafo model.menuControllerId model.menuControllerTrafo

            let allProbesUpdated =
                if model.grabberId.IsSome && not model.allProbes.IsEmpty then 
                    model.allProbes
                    |> HashMap.map (fun key probe -> 
                        let newCenter = heraTrafos.Forward.TransformPos(probe.centerRelToHera)
                        let newRadius = probe.radiusRelToHera * heraTrafos.Forward.GetScaleVector3().X
                        let sphere = Sphere3d(newCenter, newRadius)
                        let intersection = heraBBox.Intersects(sphere)
                        let updatedProbe = 
                            {
                                center = newCenter
                                radius = newRadius
                                centerRelToHera = probe.centerRelToHera
                                radiusRelToHera = probe.radiusRelToHera
                                insideHera = intersection
                                id = probe.id
                                currStatistics = probe.currStatistics
                            }
                        updatedProbe
                        )
                else 
                    model.allProbes

            let newTouchpadDeviceTrafo = newOrOldTrafo id trafo model.touchpadDeviceId model.touchpadDeviceTrafo 

            {model with 
                controllerTrafo = contrTrafo
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
                heraBox = heraBBox
                allProbes = allProbesUpdated
                heraTransformations = heraTrafos
                touchpadDeviceTrafo = newTouchpadDeviceTrafo}
        | ActivateControllerMode id ->
            let currDevice = model.devicesTrafos.TryFind(id)
            let currDeviceTrafo = trafoOrIdentity currDevice
            if not model.controllerMenuOpen then 
                match model.controllerMode with
                | ControllerMode.Probe -> update runtime client frames state vr model (CreateProbe (id, currDevice))
                | ControllerMode.Ray -> update runtime client frames state vr model (CreateRay (id, currDeviceTrafo))
                | ControllerMode.Clipping -> update runtime client frames state vr model (CreateClipping (id, currDevice, currDeviceTrafo))
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
                | Some i -> if i = id then 
                                if model.deletionControllerId.IsSome then // if deletion controller is set then DELETE current probe with main controller 
                                    //TODO: Check if the deleted probbe is the last one used for filtering
                                    let filterProbe = 
                                        match model.lastFilterProbe with 
                                            | Some pr -> if pr.id == probe then None else model.lastFilterProbe
                                            | None -> None
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
                                        | Some pr -> 
                                            //let newRadius = pr.radiusRelToHera * model.heraTransformations.Forward.GetScaleVector3().X
                                            (pr.radius / model.sphereRadius)
                                        | None -> model.sphereScale
                                    {model with 
                                        allProbes = model.allProbes.Remove(probe)
                                        lastFilterProbe = currProbe
                                        currentProbeManipulated = true
                                        sphereControllerTrafo = trafo
                                        sphereControllerId = Some id
                                        sphereScale = currScale
                                        probeIntersectionId = None}
                            else // if probe is interseecting but current controller is not the same as intersection controller -> set deletion controller
                                {model with deletionControllerId = Some id}
                | None -> {model with deletionControllerId = None} // if no controller is intersectiong, then the probe cannot be deleted
            | None -> // controller is not intersecting with a probe
                match model.sphereControllerId with // is the id of the controller creating spheres set 
                | Some i -> if i = id then // if the current id is the same as the previous -> set new trafos for the sphere
                                {model with 
                                    currentProbeManipulated = true
                                    sphereControllerTrafo = trafo
                                    sphereControllerId = Some id
                                    intersectionControllerId = Some id}
                            else // if not the same id -> sphere shold be scaled 
                                {model with
                                    currentProbeManipulated = true
                                    sphereScalerId = Some id
                                    sphereScalerTrafo = trafo} 
                | None -> //  set new trafos for the sphere
                        {model with 
                            currentProbeManipulated = true
                            sphereControllerTrafo = trafo
                            sphereControllerId = Some id
                            intersectionControllerId = Some id}
        | CreateRay (id, trafo) ->
            match model.rayDeviceId with 
            | Some i -> if i = id then 
                            client.Mouse.Down(model.screenCoordsHitPos, MouseButtons.Left)
                            {model with 
                                rayTriggerClicked = true
                                clickPosition = Some model.screenHitPoint}
                        else 
                            model
            | None ->
                let origin = trafo.Forward.TransformPos(V3d(0.0, 0.02, 0.0))
                let direction = trafo.Forward.TransformPos(V3d.OIO * 1000.0) 
                {model with 
                        rayDeviceId = Some id
                        ray = Ray3d(origin, direction)}
        | CreateClipping (id, trafoO, trafo) ->
            let p0 = trafo.Forward.TransformPos(planePos0)
            let p1 = trafo.Forward.TransformPos(planePos1)
            let p2 = trafo.Forward.TransformPos(planePos2)
            let p3 = trafo.Forward.TransformPos(planePos3)
            match model.clippingPlaneDeviceId with 
            | Some i -> if i = id then 
                            {model with 
                                clippingPlaneDeviceTrafo = trafoO
                                clippingPlaneDeviceId = Some id}
                        else 
                            model
            | None -> 
                    {model with 
                            planeCorners = Quad3d(p0, p1, p2, p3)
                            clippingPlaneDeviceTrafo = trafoO
                            clippingPlaneDeviceId = Some id 
                            }
        | DeactivateControllerMode id ->
            if not model.controllerMenuOpen then 
                match model.controllerMode with 
                | ControllerMode.Probe -> 
                    let delControllerId = 
                        match model.deletionControllerId with 
                        | Some i -> if i = id then None else model.deletionControllerId
                        | None -> None
                    match model.sphereControllerId with
                    | Some i -> if i = id then 
                                    let t = trafoOrIdentity model.sphereControllerTrafo
                                  //  let sphereTrafo = Trafo3d(Scale3d(model.sphereScale)) * t
                                    let heraInvMatrix = model.heraTransformations.Backward

                                    let spherePos = t.Forward.TransformPos(V3d.OOO)
                                    let spherePosTransformed = heraInvMatrix.TransformPos(spherePos)

                                    let sphereRadius = model.sphereRadius * model.sphereScale
                                    //let posToRadius = spherePos + V3d(sphereRadius, 0.0, 0.0)
                                    //let posToRadiusTransformed = heraInvMatrix.TransformPos(posToRadius)
                                    //let radiusTransformed = posToRadiusTransformed - spherePosTransformed
                                    let heraScale = heraInvMatrix.GetScaleVector3()
                                    let radiusTransformed = sphereRadius * heraScale.X

                                    let sphere = Sphere3d(spherePos, sphereRadius)
                                    let sphereTransformed = Sphere3d(spherePosTransformed, radiusTransformed)

                                    //printf "hera Scale %A \n" heraScale
                                    //printf "sphere Scale %A \n" model.sphereScale
                                    //printf "sphere Pos %A \n" spherePos
                                    //printf "sphere Pos Transformed %A \n" spherePosTransformed
                                    //printf "sphere Radius %A \n" sphereRadius
                                    //printf "sphere Radius Transformed %A \n" radiusTransformed

                                    //TODO: use only the relative Pos and Radius to spare code and storage
                                    let intersection = model.heraBox.Intersects(sphere)
                                    
                                    let result = filterDataForOneFrameSphere frames.[mTwoD.frame] (Some sphereTransformed) mTwoD.renderValue
                                    let stats = snd result

                                    let probe = createProbe spherePos sphereRadius spherePosTransformed radiusTransformed intersection stats

                                    printf "Statistics: \n %A" stats

                                    let filteredData = 
                                        if intersection then
                                            let array = fst result
                                            array
                                        else 
                                            mTwoD.data.arr

                                    let updatedTwoDmodel = 
                                        { mTwoD with
                                            sphereFilter = Some sphereTransformed
                                            data = { version = mTwoD.data.version + 1; arr = filteredData}
                                        }

                                    let temp = model.sphereScale
                                    printf "sphere Scale %A \n" temp

                                    {model with
                                        currentProbeManipulated = false
                                        allProbes = model.allProbes.Add(probe.id, probe)
                                        lastSphereScale = temp
                                        lastFilterProbe = Some probe
                                        statistics = stats
                                        sphereScale = 1.0
                                        sphereControllerId = None
                                        sphereScalerId = None
                                        deletionControllerId = delControllerId
                                        twoDModel = updatedTwoDmodel}
                                else
                                    {model with 
                                        sphereScalerId = None
                                        deletionControllerId = delControllerId}
                    | None -> {model with 
                                    sphereScalerId = None
                                    deletionControllerId = delControllerId}
                | ControllerMode.Ray ->     
                    match model.rayDeviceId with
                    | Some i -> if i = id then
                                    client.Mouse.Up(model.screenCoordsHitPos, MouseButtons.Left)
                                    client.Mouse.Click(model.screenCoordsHitPos, MouseButtons.Left)
                                    {model with 
                                        rayTriggerClicked = false
                                        clickPosition = None}
                                else 
                                    model 
                    | None -> model
                | ControllerMode.Clipping -> 
                    match model.clippingPlaneDeviceId with 
                    | Some i -> if i = id then {model with clippingPlaneDeviceId = None} else model
                    | None -> model
                | _ -> model
            else 
                { model with 
                    sphereControllerId = None
                    sphereScalerId = None
                    rayDeviceId = None
                    clippingPlaneDeviceId = None 
                }
        | ToggleControllerMenu -> 
            let pos = convertCartesianToPolar model.currTouchPadPos
            let r = pos |> fst
            if r < 0.5 then
                {model with 
                    controllerMenuOpen = not model.controllerMenuOpen
                    sphereControllerId = None
                    sphereScalerId = None
                    rayDeviceId = None
                    clippingPlaneDeviceId = None }
            else  
                model
        | OpenControllerMenu id ->
            let currDevice = model.devicesTrafos.TryFind(id)
            let currDeviceTrafo = trafoOrIdentity currDevice
            if model.controllerMenuOpen then 
                {model with 
                    menuControllerTrafo = Some currDeviceTrafo
                    menuControllerId = Some id}
            else    
                {model with 
                    menuControllerId = None}
        | ChangeControllerMode id -> 
            match model.menuControllerId with  
            | Some i -> 
                if i = id then 
                    let pos = convertCartesianToPolar model.currTouchPadPos
                    let r = pos |> fst
                    let theta = pos |> snd
                    if r >= 0.5 then 
                        if theta >= 0.0 && theta < 60.0 then 
                            {model with 
                                controllerMode = ControllerMode.Clipping
                                touchpadTexture = "right.png"}
                        else if theta >= 60.0 && theta < 120.0 then 
                            {model with 
                                controllerMode = ControllerMode.Ray
                                touchpadTexture = "middle.png"}
                        else if theta >= 120.0 && theta < 180.0 then 
                            {model with 
                                controllerMode = ControllerMode.Probe
                                touchpadTexture = "left.png"}
                        else model
                    else 
                        model

                    //let posX = model.currTouchPadPos.X
                    //let mode = 
                    //    match model.controllerMode with 
                    //    | ControllerMode.Probe -> 0 
                    //    | ControllerMode.Ray -> 1 
                    //    | ControllerMode.Clipping -> 2
                    //    | _ -> 0
                    //let nextModeInt = 
                    //    if posX <= -0.5 then
                    //        if mode = 0 then 2 else (mode - 1)
                    //    else if posX >= 0.5 then         
                    //        if mode = 2 then 0 else (mode + 1)
                    //    else
                    //        mode
                    //let nextMode = 
                    //    match nextModeInt with 
                    //    | 0 -> ControllerMode.Probe
                    //    | 1 -> ControllerMode.Ray
                    //    | 2 -> ControllerMode.Clipping
                    //    | _ -> ControllerMode.Probe
                    //printf "nextmMode %A" nextMode
                    //{model with controllerMode = nextMode}
                else 
                    model
            | None -> model
        | ChangeTouchpadPos (id, pos) -> 
            let currTouchDevice = model.devicesTrafos.TryFind(id)
            match model.rayDeviceId with 
            | Some i -> 
                if i = id && model.screenIntersection then 
                    client.Mouse.Scroll(model.screenCoordsHitPos, pos.Y * 50.0)
            | None -> ()
            let newId = if pos.X = 0.0 && pos.Y = 0.0 then None else Some id //when both X and Y are equal to 0.0 it means we are currently not touching the touchpad
            {model with 
                currTouchPadPos = pos
                touchpadDeviceId = newId
                touchpadDeviceTrafo = currTouchDevice}
        | UntouchDevice id ->
            match model.touchpadDeviceId with 
            | Some i ->
                if i = id then 
                    {model with touchpadDeviceId = None}
                else
                    model
            | None -> model
        | ResetHera -> initial runtime frames
            

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
                [ToggleControllerMenu; OpenControllerMenu controllerId]
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
                let x = value.X
                if x >=  0.0 then 
                    [ChangeTouchpadPos (controllerId, value); ScaleUpHera x; ChangeControllerMode controllerId]
                else 
                    [ChangeTouchpadPos (controllerId, value); ScaleDownHera x; ChangeControllerMode controllerId]
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

        let trafoOrIdentity trafo = 
            match trafo with 
            | Some t -> t
            | None -> Trafo3d.Identity

        let touching = m.touchpadDeviceId |> AVal.map (fun id -> id.IsSome)

        let tdf = m.touchpadDeviceTrafo |> AVal.map (fun t -> trafoOrIdentity t)

        let touchpadPos = m.currTouchPadPos |> AVal.map (fun pos -> let z = if pos.Y >= 0.6 then 0.0025 else if pos.Y >= 0.0 then 0.001 else 0.0 
                                                                    V3d(pos * 0.019, z))

        let touchpadSphereSg = 
            Sg.sphere' 9 C4b.LightGreen 0.002
            |> Sg.noEvents
            |> Sg.translate 0.0 -0.05 0.0035 // translation so that it is in the middle of the touchpad
            |> Sg.translate' touchpadPos
            |> Sg.trafo tdf
            |> Sg.onOff touching

        //TODO: Why not directly use hera trafo?!?!?!
        let trafo = m.heraTrafo |> AVal.map (fun t -> trafoOrIdentity t)
            //AVal.map2 (fun contr heraContr -> 
            //    let contrTrafo = trafoOrIdentity contr 
            //    let heraContrTrafo = trafoOrIdentity heraContr
            //    heraContrTrafo * contrTrafo) m.controllerTrafo m.heraToControllerTrafo


        let heraScaleTrafo = m.scalingFactorHera |> AVal.map(fun s -> Trafo3d (Scale3d(s)))


        let sphereTrafo = 
            AVal.map2 (fun sphereContrTrafo (sphereScale : float) -> 
                let spContrT = trafoOrIdentity sphereContrTrafo
                let scale = Trafo3d(Scale3d(sphereScale))
                scale * spContrT
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
                ) heraScaleTrafo trafo 

        let probeScale = 
            AVal.map3 (fun currProbMan currScale lastScale ->
                if currProbMan then 
                    currScale
                else
                    lastScale
                ) m.currentProbeManipulated m.sphereScale m.lastSphereScale


        let sphereProbe = 
            AVal.map3 (fun trafo scale initRadius -> 
                match trafo with 
                | Some (t : Trafo3d) -> 
                    if t.Forward.IsIdentity() then 
                        Sphere3d.Invalid
                    else 
                        let spherePos = t.Forward.TransformPos(V3d.OOO)
                        let sphereRadius : float = initRadius * scale 
                        Sphere3d(spherePos, sphereRadius)
                | None -> Sphere3d.Invalid
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

        let touchpadTex = 
            m.touchpadTexture |> AVal.map (fun tex -> 
                let path = texturesPath + tex
                FileTexture(path, true) :> ITexture) 

        let clipPlaneSg = planeSg planePositions (C4f(0.0,0.0,1.0,0.1)) FillMode.Fill (AVal.constant mode) pass1
        let quadSg = planeSg quadPositions C4f.Gray10 FillMode.Fill (AVal.constant BlendMode.None) pass0

        let touchpadPlaneSg = 
            Sg.draw IndexedGeometryMode.TriangleList
            |> Sg.vertexAttribute DefaultSemantic.Positions texturePositions
            |> Sg.vertexAttribute DefaultSemantic.Normals (AVal.constant [| V3f.OOI; V3f.OOI; V3f.OOI; V3f.OOI |])
            |> Sg.vertexAttribute DefaultSemantic.DiffuseColorCoordinates  (AVal.constant  [| V2f.OO; V2f.IO; V2f.II; V2f.OI |])
            |> Sg.index (AVal.constant [|0;1;2; 0;2;3|])
            |> Sg.scale 0.0205
            |> Sg.transform (Trafo3d.RotationXInDegrees(6.5))
            |> Sg.translate 0.0 -0.05 0.0051
            |> Sg.trafo tdf
            |> Sg.onOff touching
            |> Sg.diffuseTexture touchpadTex
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.diffuseTexture
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

        let contrOrientation = 
            m.menuControllerTrafo  
            |> AVal.map (fun t -> 
                let trafo = trafoOrIdentity t
                trafo.GetOrthoNormalOrientation()
                ) 

        let controllerPos position = 
            m.menuControllerTrafo  
            |> AVal.map (fun t -> 
                let trafo = trafoOrIdentity t
                trafo.Forward.TransformPos(position)
                ) 

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
              
        let controllerSg path rotX rotY rotZ scTrafo contrPos = 
            Loader.Assimp.load (Path.combine path)
            |> Sg.adapter
            |> Sg.transform (Trafo3d.Scale(1.0, 1.0, -1.0))
            |> Sg.transform (Trafo3d.RotationEulerInDegrees(rotX, rotY, rotZ))
            |> Sg.trafo scTrafo 
            |> Sg.trafo contrOrientation
            |> Sg.translate' contrPos
            |> Sg.onOff m.controllerMenuOpen
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
                |> Sg.trafo (m.controllerTrafo |> AVal.map (fun mtw -> (trafoOrIdentity mtw).Forward.TransformPos(V3d.OOO) |> Trafo3d.Translation))
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