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

open FSharp.Data.Adaptive

open AardVolume.Model
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
    | ChangeTouchpadPos of float
    | ChangeControllerMode of int
    | ActivateControllerMode of int
    | CreateProbe of int * Option<Trafo3d>
    | CreateRay of int * Trafo3d
    | CreateClipping of int * Option<Trafo3d> * Trafo3d
    | DeactivateControllerMode of int
    | MouseClick
    | ResetHera 
 
module Demo =
    open Aardvark.UI.Primitives
    open Aardvark.Base.Rendering

    let quadRightUp =   V3d(0.732, 0.432, -0.013)
    let quadLeftUp =    V3d(-0.732, 0.432, -0.013)
    let quadLeftDown =  V3d(-0.732, -0.41483, -0.013)
    let quadRightDown = V3d(0.732, -0.41483, -0.013)

    let flatScreenTrafo rotY rotZ = 
        let scale = Trafo3d(Scale3d(2.0))
        let rotation = Trafo3d.RotationEulerInDegrees(90.0, rotY, rotZ)
        let translation = Trafo3d.Translation(2.5, 1.0, 1.5)
        scale * rotation * translation

    let tvQuadTrafo = flatScreenTrafo 180.0 90.0
    let tvTrafo = flatScreenTrafo 0.0 -90.0

    let screenResolution = V2i(1008,729)

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
            sphereRadius = 0.2
            sphereColor = C4b(1.0,1.0,1.0,0.4)
            currentProbeManipulated = false
            currentProbe = None
            allProbes = HashMap.Empty
            probeIntersectionId = None
            intersectionControllerId = None
            deletionControllerId = None
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
            clippingPlaneDeviceTrafo = None
            clippingPlaneDeviceId = None
            planeCorners = Quad3d(V3d(), V3d(), V3d(), V3d())
            controllerMenuOpen = false
            menuControllerTrafo = None
            menuControllerId = None
            controllerMode = ControllerMode.Ray
            touchPadCurrPosX = 0.0
            client = 
                let br = new Browser(null,AVal.constant System.DateTime.Now,runtime, true, AVal.constant (screenResolution))
                let res = br.LoadUrl "http://localhost:4321"
                br
        }

    //let updateController (m : Model) : Model = 
    //    failwith ""

    let createProbe (pos : V3d) (radius : float) : Probe = 
        {
            id = Guid.NewGuid().ToString()
            center = pos
            radius = radius
        }
        
    let rec update (runtime : IRuntime) (frames : Frame[]) (state : VrState) (vr : VrActions) (model : Model) (msg : Message) =
        let planePos0 = V3d(-0.7, 0.05, -0.5)
        let planePos1 = V3d(-0.7, 0.05, 0.5)
        let planePos2 = V3d(0.7, 0.05, 0.5)

        let planePos3 = V3d(0.7, 0.05, -0.5)

        //let res = model.client.LoadUrl "http://localhost:4321"

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
                    Ray3d(initRay.Origin, hit.Point)
                else 
                    initRay
           // if hitPoint then printf "Hit Point %A \n" hit.Point
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
                probeIntersectionId = probeIntersection}
        | ActivateControllerMode id ->
            let currDevice = model.devicesTrafos.TryFind(id)
            let currDeviceTrafo = trafoOrIdentity currDevice
            if not model.controllerMenuOpen then 
                match model.controllerMode with
                | ControllerMode.Probe -> update runtime frames state vr model (CreateProbe (id, currDevice))
                | ControllerMode.Ray -> update runtime frames state vr model (CreateRay (id, currDeviceTrafo))
                | ControllerMode.Clipping -> update runtime frames state vr model (CreateClipping (id, currDevice, currDeviceTrafo))
                | _ -> model
            else 
                { model with 
                    sphereControllerId = None
                    sphereScalerId = None
                    rayDeviceId = None
                    clippingPlaneDeviceId = None 
                }
        | CreateProbe (id, trafo) ->
            match model.probeIntersectionId with 
            | Some probe -> 
                match model.intersectionControllerId with 
                | Some i -> if i = id then 
                                if model.deletionControllerId.IsSome then 
                                    {model with allProbes = model.allProbes.Remove(probe)} 
                                else 
                                    let currProbe = model.allProbes.TryFind(probe)
                                    let currScale = 
                                        match currProbe with
                                        | Some pr -> (pr.radius / model.sphereRadius)
                                        | None -> model.sphereScale
                                    {model with 
                                        allProbes = model.allProbes.Remove(probe)
                                        currentProbeManipulated = true
                                        sphereControllerTrafo = trafo
                                        sphereControllerId = Some id
                                        sphereScale = currScale
                                        probeIntersectionId = None}
                            else 
                                {model with deletionControllerId = Some id}
                | None -> {model with deletionControllerId = None}
            | None -> 
                match model.sphereControllerId with 
                | Some i -> if i = id then 
                                {model with 
                                    currentProbeManipulated = true
                                    sphereControllerTrafo = trafo
                                    sphereControllerId = Some id
                                    intersectionControllerId = Some id}
                            else
                                {model with
                                    currentProbeManipulated = true
                                    sphereScalerId = Some id
                                    sphereScalerTrafo = trafo} 
                | None -> 
                        {model with 
                            currentProbeManipulated = true
                            sphereControllerTrafo = trafo
                            sphereControllerId = Some id
                            intersectionControllerId = Some id}
        | CreateRay (id, trafo) ->
            match model.rayDeviceId with 
            | Some i -> if i = id then 
                            printf "CLICKED \n"
                            let hitPoint = model.screenHitPoint
                            let clickPos = V2d(hitPoint.Y, hitPoint.X)
                            //printf "Click Pos: %A \n" clickPos
                            let screenCoords = V2d(clickPos.X * screenResolution.ToV2d().X, clickPos.Y * screenResolution.ToV2d().Y)
                            let screenCoordsInt = screenCoords.ToV2i()
                            printf "Screen Coords: %A \n" screenCoordsInt
                            let screenPos = PixelPosition(screenCoordsInt, screenResolution.X, screenResolution.Y)
                            printf "Screen Pos: %A \n" screenPos.Position
                            printf "Screen Bounds: %A \n" screenPos.Bounds
                            //printf "Screen Pos: %A \n" screenPos
                            model.client.SetFocus true
                            model.client.Mouse.Down(screenPos, MouseButtons.Left)
                            model.client.Mouse.Up(screenPos, MouseButtons.Left)
                            model.client.Mouse.Click(screenPos, MouseButtons.Left)
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
                                    let spherePos = t.Forward.TransformPos(V3d.OOO)
                                    let sphereRadius = model.sphereRadius * model.sphereScale
                                    let probe = createProbe spherePos sphereRadius
                                    {model with
                                        currentProbeManipulated = false
                                        allProbes = model.allProbes.Add(probe.id, probe)
                                        sphereScale = 1.0
                                        sphereControllerId = None
                                        sphereScalerId = None
                                        deletionControllerId = delControllerId}
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
            if model.touchPadCurrPosX >= -0.3 && model.touchPadCurrPosX <= 0.3 then
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
                    let pos = model.touchPadCurrPosX
                    let mode = 
                        match model.controllerMode with 
                        | ControllerMode.Probe -> 0 
                        | ControllerMode.Ray -> 1 
                        | ControllerMode.Clipping -> 2
                        | _ -> 0
                    let nextModeInt = 
                        if pos <= -0.5 then
                            if mode = 0 then 2 else (mode - 1)
                        else if pos >= 0.5 then         
                            if mode = 2 then 0 else (mode + 1)
                        else
                            mode
                    let nextMode = 
                        match nextModeInt with 
                        | 0 -> ControllerMode.Probe
                        | 1 -> ControllerMode.Ray
                        | 2 -> ControllerMode.Clipping
                        | _ -> ControllerMode.Probe
                    printf "nextmMode %A" nextMode
                    {model with controllerMode = nextMode}
                else 
                    model
            | None -> model
        | ChangeTouchpadPos pos -> {model with touchPadCurrPosX = pos}
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
                [ToggleControllerMenu; OpenControllerMenu controllerId; ChangeControllerMode controllerId]
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
            []
        | VrMessage.ValueChange(controllerId, buttonId, value) ->
            //printf "value change: %A " (controllerId, buttonId, value)
            if buttonId = 0 then 
                let x = value.X
                if x >=  0.0 then 
                    [ChangeTouchpadPos x; ScaleUpHera x]
                else 
                    [ChangeTouchpadPos x; ScaleDownHera x]
            else 
                []
        | VrMessage.UpdatePose(controllerId, pose) ->
            if pose.isValid then [MoveController (controllerId, pose.deviceToWorld)] else []
        | _ -> []

    let ui (runtime : IRuntime) (data : Frame[] * Box3f * int) (info : VrSystemInfo) (m : AdaptiveModel) : DomNode<Message> = // 2D UI
        div [] [
            AardVolume.App.view runtime data m.twoDModel |> UI.map TwoD
        ]

    let vr (runtime : IRuntime) (data : Frame[] * Box3f * int) (info : VrSystemInfo) (m : AdaptiveModel) : ISg<Message> = // HMD Graphics

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

        let trafo = 
            AVal.map2 (fun contr heraContr -> 
                let contrTrafo = trafoOrIdentity contr 
                let heraContrTrafo = trafoOrIdentity heraContr
                heraContrTrafo * contrTrafo) m.controllerTrafo m.heraToControllerTrafo


        let heraScaleTrafo = m.scalingFactorHera |> AVal.map(fun s -> Trafo3d (Scale3d(s)))

        let sphereScaleTrafo = m.sphereScale |> AVal.map(fun s -> Trafo3d (Scale3d(s)))

        let sphereTrafo = m.sphereControllerTrafo |> AVal.map (fun t -> trafoOrIdentity t)

        let contrClippingPlane = m.planeCorners |> AVal.map (fun c -> Plane3d(c.P0, c.P1, c.P2))

        let mutable mode = BlendMode(true)
        mode.Enabled <- true
        mode.Operation <- BlendOperation.Add
        mode.AlphaOperation <- BlendOperation.Add
        mode.SourceFactor <- BlendFactor.SourceAlpha
        mode.DestinationFactor <- BlendFactor.InvSourceAlpha
        mode.SourceAlphaFactor <- BlendFactor.One
        mode.DestinationAlphaFactor <- BlendFactor.InvSourceAlpha

        //let sphereProbe = 
        //    AVal.map3 (fun trafo scale initRadius -> 
        //        match trafo with 
        //        | Some (t : Trafo3d) -> 
        //            if t.Forward.IsIdentity() then 
        //                Sphere3d.Invalid
        //            else 
        //                let spherePos = t.Forward.TransformPos(V3d.OOO)
        //                let sphereRadius : float = initRadius * scale
        //                Sphere3d(spherePos, sphereRadius)
        //        | None -> Sphere3d.Invalid
        //        ) m.sphereControllerTrafo m.sphereScale m.sphereRadius

        let sphereProbe = Sphere3d.Invalid |> AVal.constant
        
        let heraSg =    
            let m = m.twoDModel
            data
            |> Hera.Hera.createAnimatedVrSg 
                m.frame m.pointSize m.discardPoints m.renderValue m.currentMap 
                m.domainRange m.clippingPlane contrClippingPlane m.filter m.currFilters m.dataRange m.colorValue.c 
                sphereProbe
                m.cameraState.view
                runtime
            |> Sg.noEvents
            |> Sg.trafo heraScaleTrafo
            |> Sg.translate 0.0 0.0 0.7
            |> Sg.trafo trafo
            |> Sg.pass pass0
            //|> Sg.blendMode (AVal.constant mode)

        let currentSphereProbeSg = 
            Sg.sphere 9 m.sphereColor m.sphereRadius
            |> Sg.noEvents
            |> Sg.trafo sphereScaleTrafo
            |> Sg.trafo sphereTrafo
            |> Sg.onOff m.currentProbeManipulated
           // |> Sg.fillMode (FillMode.Fill |> AVal.constant)
            |> Sg.cullMode (CullMode.Front |> AVal.constant)
            |> Sg.blendMode (AVal.constant mode)
            |> Sg.pass pass1
            
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
                                            C4b(1.0,1.0,1.0,0.4)
                            | None -> C4b(1.0,1.0,1.0,0.4)
                        ) m.probeIntersectionId m.deletionControllerId
                    Sg.sphere 9 color (AVal.constant p.radius)
                    |> Sg.noEvents
                    |> Sg.transform (Trafo3d.Translation(p.center))
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

        let clipPlaneSg = planeSg planePositions (C4f(0.0,0.0,1.0,0.1)) FillMode.Fill (AVal.constant mode) pass1
        let quadSg = planeSg quadPositions C4f.Gray10 FillMode.Fill (AVal.constant BlendMode.None) pass0

        let browserSg = 
            if true then
                Sg.draw IndexedGeometryMode.TriangleList
                    |> Sg.vertexAttribute DefaultSemantic.Positions quadPositions
                    |> Sg.vertexAttribute DefaultSemantic.Normals (AVal.constant [| V3f.OOI; V3f.OOI; V3f.OOI; V3f.OOI |])
                    |> Sg.vertexAttribute DefaultSemantic.DiffuseColorCoordinates  (AVal.constant  [| V2f.OO; V2f.OI; V2f.II; V2f.IO |])
                    |> Sg.index (AVal.constant [|0;1;2; 0;2;3|])
                    |> Sg.diffuseTexture m.client.Texture 
                    |> Sg.shader {
                        do! DefaultSurfaces.trafo
                        do! DefaultSurfaces.diffuseTexture
                    }
                    //|> Sg.fillMode (fillmode |> AVal.constant)
                    //|> Sg.blendMode blendmode
                    //|> Sg.pass renderPass                
            else Sg.empty


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
            
        Sg.ofSeq [
            deviceSgs; currentSphereProbeSg; probesSgs; heraSg; clipPlaneSg; tvSg;
            billboardSg; probeContrSg; laserContrSg; clippingContrSg; ray
            browserSg
        ] |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.simpleLighting
            }    
        //Sg.ofSeq [billboardSg]
        //|> Sg.shader {
        //    do! DefaultSurfaces.trafo
        //    do! DefaultSurfaces.simpleLighting
        //}
        //   // |> Sg.blendMode (AVal.constant mode)
        //Sg.ofSeq [tvSg; quadSg]
        //    |> Sg.shader {
        //        do! DefaultSurfaces.trafo
        //        do! DefaultSurfaces.simpleLighting
        //    }
           // |> Sg.blendMode (AVal.constant mode)


        
    let pause (info : VrSystemInfo) (m : AdaptiveModel) =
        Sg.box' C4b.Red Box3d.Unit
        |> Sg.noEvents
        |> Sg.shader {
            do! DefaultSurfaces.trafo
            do! DefaultSurfaces.vertexColor
            do! DefaultSurfaces.simpleLighting
        }

    let app (runtime : IRuntime) : ComposedApp<Model,AdaptiveModel,Message> =
        let data = DataLoader.loadData runtime 0 7
        let frames = data |> (fun (elem, _, _) ->  elem)
        {
            unpersist = Unpersist.instance
            initial = initial runtime frames
            update = update runtime frames
            threads = threads
            input = input 
            ui = ui runtime data
            vr = vr runtime data
            pauseScene = Some pause
        }