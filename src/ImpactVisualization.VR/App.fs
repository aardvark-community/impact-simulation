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
open Aardvark.Application.OpenVR

open FSharp.Data.Adaptive

open AardVolume.Model
open ImpactVisualization

type ThreeDMessage = Nop

type Message =
    | ThreeD of ThreeDMessage
    | TwoD of AardVolume.Message 
    | Nop
    | StartVr
    | Grab of int
    | Ungrab of int
    | ScaleUp of float 
    | ScaleDown of float 
    | MoveController of int * Trafo3d
    //| ControlSphere of int
    //| ReleaseSphere of int 
    //| ActivateRay of int
    //| DeactivateRay of int
    //| ActivatePlane of int
    | ToggleControllerMenu
    | OpenControllerMenu of int
    | ChangeTouchpadPos of float
    | ChangeControllerMode of int
    | ActivateControllerMode of int
    | DeactivateControllerMode of int
    | ResetHera 
 
module Demo =
    open Aardvark.UI.Primitives
    open Aardvark.Base.Rendering

    let initial frames = 
        {   
            twoDModel = AardVolume.App.initial frames
            text = "hello" 
            devicesTrafos = HashMap.Empty
            controllerTrafo = None
            heraTrafo = Some Trafo3d.Identity
            heraToControllerTrafo = None
            grabberId = None
            allowHeraScaling = false
            sphereControllerTrafo = Some Trafo3d.Identity
            sphereControllerId = None
            sphereScalerTrafo = None
            sphereScalerId = None
            scalingFactorHera = 0.05
            sphereScale = 1.0
            sphereRadius = 0.2
            sphereColor = C4b(1.0,1.0,1.0,0.4)
            sphereProbeCreated = false
            rayDeviceId = None
            ray = Ray3d.Invalid
            flatScreen = Quad3d(V3d(-25,-20,-8), V3d(-25,10,-8), V3d(-25,10,12), V3d(-25,-20,12))
            rayColor = new C4b(178, 223, 138)
            screenIntersection = false
            clippingPlaneDeviceId = None
            planeCorners = Quad3d(V3d(), V3d(), V3d(), V3d())
            controllerMenuOpen = false
            menuControllerTrafo = None
            menuControllerId = None
            controllerMode = ControllerMode.Probe
            touchPadCurrPosX = 0.0
        }
        
    let update (frames : Frame[]) (state : VrState) (vr : VrActions) (model : Model) (msg : Message) =
        let planePos0 = V3d(-0.7, 0.05, -0.5)
        let planePos1 = V3d(-0.7, 0.05, 0.5)
        let planePos2 = V3d(0.7, 0.05, 0.5)
        let planePos3 = V3d(0.7, 0.05, -0.5)

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
        | Grab id ->
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
        | Ungrab id -> 
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
        | ScaleUp f -> 
            let maxScale = 1.0
            let currScale = if model.allowHeraScaling then model.scalingFactorHera * (f*f/5.0 + 1.0) else model.scalingFactorHera
            let newScale = if currScale >= maxScale then maxScale else currScale
            {model with scalingFactorHera = newScale}
        | ScaleDown f -> 
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
            let currRay = 
                match model.rayDeviceId with    
                | Some id ->
                    let currDevice = model.devicesTrafos.TryFind(id)
                    let currDeviceTrafo = trafoOrIdentity currDevice
                    let origin = currDeviceTrafo.Forward.TransformPos(V3d(0.0, 0.02, 0.0))
                    let direction = currDeviceTrafo.Forward.TransformPos(V3d.OIO * 10.0)
                    Ray3d(origin, direction)
                | None -> Ray3d.Invalid    
            let intersect = currRay.Intersects(model.flatScreen)
            //if intersect then printf "INTERSECT"
            let currCorners = 
                match model.clippingPlaneDeviceId with
                | Some id ->
                    let currDevice = model.devicesTrafos.TryFind(id)
                    let currDeviceTrafo = trafoOrIdentity currDevice
                    let p0 = currDeviceTrafo.Forward.TransformPos(planePos0)
                    let p1 = currDeviceTrafo.Forward.TransformPos(planePos1)
                    let p2 = currDeviceTrafo.Forward.TransformPos(planePos2)
                    let p3 = currDeviceTrafo.Forward.TransformPos(planePos3)
                    Quad3d(p0, p1, p2, p3)
                | None -> Quad3d(V3d(), V3d(), V3d(), V3d())
            let currMenuTrafo = newOrOldTrafo id trafo model.menuControllerId model.menuControllerTrafo
            {model with 
                controllerTrafo = contrTrafo
                sphereControllerTrafo = sphereContrTrafo
                sphereScalerTrafo = sphereScTrafo
                sphereScale = scalingFactor
                devicesTrafos = newInput
                ray = currRay
                planeCorners = currCorners
                screenIntersection = intersect
                menuControllerTrafo = currMenuTrafo}
        | ActivateControllerMode id ->
            let currDevice = model.devicesTrafos.TryFind(id)
            let currDeviceTrafo = trafoOrIdentity currDevice
            if not model.controllerMenuOpen then 
                match model.controllerMode with
                | ControllerMode.Probe ->
                    match model.sphereControllerId with 
                    | Some i -> if i = id then 
                                    {model with 
                                        sphereControllerTrafo = currDevice
                                        sphereControllerId = Some id}
                                else
                                    {model with 
                                        sphereScalerId = Some id
                                        sphereScalerTrafo = currDevice} 
                    | None -> 
                            {model with 
                                sphereProbeCreated = true
                                sphereControllerTrafo = currDevice
                                sphereControllerId = Some id}
                | ControllerMode.Ray ->
                    let origin = currDeviceTrafo.Forward.TransformPos(V3d(0.0, 0.02, 0.0))
                    let direction = currDeviceTrafo.Forward.TransformPos(V3d.OIO * 10.0) 
                    {model with 
                            rayDeviceId = Some id
                            ray = Ray3d(origin, direction)}
                | ControllerMode.Clipping -> 
                    let p0 = currDeviceTrafo.Forward.TransformPos(planePos0)
                    let p1 = currDeviceTrafo.Forward.TransformPos(planePos1)
                    let p2 = currDeviceTrafo.Forward.TransformPos(planePos2)
                    let p3 = currDeviceTrafo.Forward.TransformPos(planePos3)
                    {model with 
                            planeCorners = Quad3d(p0, p1, p2, p3)
                            clippingPlaneDeviceId = Some id 
                            }
                | _ -> model
            else 
                { model with 
                    sphereControllerId = None
                    sphereScalerId = None
                    rayDeviceId = None
                    clippingPlaneDeviceId = None 
                }
        | DeactivateControllerMode id ->
            if not model.controllerMenuOpen then 
                match model.controllerMode with 
                | ControllerMode.Probe -> 
                    match model.sphereControllerId with
                    | Some i -> if i = id then 
                                    {model with
                                        sphereControllerId = None
                                        sphereScalerId = None}
                                else
                                    {model with sphereScalerId = None}
                    | None -> {model with sphereScalerId = None}
                | ControllerMode.Ray -> model
                | ControllerMode.Clipping -> model
                | _ -> model
            else 
                { model with 
                    sphereControllerId = None
                    sphereScalerId = None
                    rayDeviceId = None
                    clippingPlaneDeviceId = None 
                }
            
        //| ControlSphere id -> 
        //    let currTrafo = model.devicesTrafos.TryFind(id)
        //    match model.sphereControllerId with 
        //    | Some i -> if i = id then 
        //                    {model with 
        //                        sphereControllerTrafo = currTrafo
        //                        sphereControllerId = Some id}
        //                else
        //                    {model with 
        //                        sphereScalerId = Some id
        //                        sphereScalerTrafo = currTrafo} 
        //    | None -> 
        //            {model with 
        //                sphereProbeCreated = true
        //                sphereControllerTrafo = currTrafo
        //                sphereControllerId = Some id}
        //| ReleaseSphere id ->
        //    match model.sphereControllerId with
        //    | Some i -> if i = id then 
        //                    {model with
        //                        sphereControllerId = None
        //                        sphereScalerId = None}
        //                else
        //                    {model with sphereScalerId = None}
        //    | None -> {model with sphereScalerId = None}
        //| ActivateRay id -> 
        //    let currDevice = model.devicesTrafos.TryFind(id)
        //    let currDeviceTrafo = trafoOrIdentity currDevice
        //    let origin = currDeviceTrafo.Forward.TransformPos(V3d(0.0, 0.02, 0.0))
        //    let direction = currDeviceTrafo.Forward.TransformPos(V3d.OIO * 10.0) 
        //    {model with 
        //            rayDeviceId = Some id
        //            ray = Ray3d(origin, direction)}
        //| ActivatePlane id ->
        //    let currDevice = model.devicesTrafos.TryFind(id)
        //    let currDeviceTrafo = trafoOrIdentity currDevice
        //    let p0 = currDeviceTrafo.Forward.TransformPos(planePos0)
        //    let p1 = currDeviceTrafo.Forward.TransformPos(planePos1)
        //    let p2 = currDeviceTrafo.Forward.TransformPos(planePos2)
        //    let p3 = currDeviceTrafo.Forward.TransformPos(planePos3)
        //    {model with 
        //            planeCorners = Quad3d(p0, p1, p2, p3)
        //            clippingPlaneDeviceId = Some id 
        //            }
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
        | ResetHera -> initial frames
            

    let threads (model : Model) =
        AardVolume.App.threads model.twoDModel |> ThreadPool.map TwoD
        
    let input (msg : VrMessage) =
        match msg with
        | VrMessage.PressButton(controllerId, buttonId) ->
           // printf "press button: %A " (controllerId, buttonId)
            if buttonId = 1 then 
                [ResetHera]
            else if buttonId = 2 then
                [Grab controllerId]
            else 
                []
        | VrMessage.UnpressButton(controllerId, buttonId) ->
            //printf "unpress button: %A " (controllerId, buttonId)
            if buttonId = 2 then
                [Ungrab controllerId]
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
            printf "touch: %A " (controllerId, buttonId)
            if buttonId = 0 then 
                []
                //[ActivatePlane controllerId]
            else 
                []
        | VrMessage.Untouch(controllerId, buttonId) ->
            printf "untouch: %A " (controllerId, buttonId)
            []
        | VrMessage.ValueChange(controllerId, buttonId, value) ->
            //printf "value change: %A " (controllerId, buttonId, value)
            if buttonId = 0 then 
                let x = value.X
                if x >=  0.0 then 
                    [ChangeTouchpadPos x; ScaleUp x]
                else 
                    [ChangeTouchpadPos x; ScaleDown x]
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

        let world = 
            Sg.textWithConfig TextConfig.Default m.text
            |> Sg.noEvents
            |> Sg.andAlso deviceSgs

        let trafoOrIdentity trafo = 
            match trafo with 
            | Some t -> t
            | None -> Trafo3d.Identity

        let trafo = 
            AVal.map2 (fun contr heraContr -> 
                let contrTrafo = trafoOrIdentity contr 
                let heraContrTrafo = trafoOrIdentity heraContr
                heraContrTrafo * contrTrafo) m.controllerTrafo m.heraToControllerTrafo


        let scaleTrafo = m.scalingFactorHera |> AVal.map(fun s -> Trafo3d (Scale3d(s)))

        let sphereScaleTrafo = 
            m.sphereScale 
            |> AVal.map(fun s -> 
               // printf "radius %A \n" (0.2 * s)
                Trafo3d (Scale3d(s)))

        //TODO : use trafoOrIdentity func
        let sphereTrafo =
            m.sphereControllerTrafo |> AVal.map (fun t ->
                match t with 
                | Some trafo -> 
                    //printf "spherePos %A \n" (trafo.Forward.TransformPos(V3d.OOO))
                    trafo
                | None -> Trafo3d.Identity
                )

        let contrClippingPlane = m.planeCorners |> AVal.map (fun c -> Plane3d(c.P0, c.P1, c.P2))

        let mutable mode = BlendMode(true)
        mode.Enabled <- true
        mode.Operation <- BlendOperation.Add
        mode.AlphaOperation <- BlendOperation.Add
        mode.SourceFactor <- BlendFactor.SourceAlpha
        mode.DestinationFactor <- BlendFactor.InvSourceAlpha
        mode.SourceAlphaFactor <- BlendFactor.One
        mode.DestinationAlphaFactor <- BlendFactor.InvSourceAlpha
        
        let heraSg =    
            let m = m.twoDModel
            data
            |> Hera.Hera.createAnimatedSg 
                m.frame m.pointSize m.discardPoints m.renderValue m.currentMap 
                m.domainRange m.clippingPlane contrClippingPlane m.filter m.currFilters m.dataRange m.colorValue.c m.cameraState.view
                 runtime
            |> Sg.noEvents
            |> Sg.trafo scaleTrafo
            |> Sg.translate 0.0 0.0 0.7
            |> Sg.trafo trafo
            //|> Sg.blendMode (AVal.constant mode)


        let sphereProbeSg = 
            Sg.sphere 9 m.sphereColor m.sphereRadius
            |> Sg.noEvents
            |> Sg.trafo sphereScaleTrafo
            |> Sg.trafo sphereTrafo
            |> Sg.onOff m.sphereProbeCreated
            |> Sg.fillMode (FillMode.Fill |> AVal.constant)
            |> Sg.cullMode (CullMode.Back |> AVal.constant)
            |> Sg.blendMode (AVal.constant mode)
            


        let lines = m.ray |> AVal.map (fun r -> [|Line3d(r.Origin, r.Direction)|]) 

        let ray =
            lines
                |> Sg.lines (AVal.constant C4b.Red)
                |> Sg.noEvents
                |> Sg.uniform "LineWidth" (AVal.constant 5)
                |> Sg.effect [
                    toEffect DefaultSurfaces.trafo
                    toEffect DefaultSurfaces.vertexColor
                    toEffect DefaultSurfaces.thickLine
                    ]
                //|> Sg.pass (RenderPass.after "lines" RenderPassOrder.Arbitrary RenderPass.main)
                //|> Sg.depthTest (AVal.constant DepthTestMode.None)
      
        let planeSg positions color fillmode =
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

        let planePositions = m.planeCorners |> AVal.map (fun q -> [|q.P0.ToV3f(); q.P1.ToV3f(); q.P2.ToV3f(); q.P3.ToV3f()|])
        let quadPositions = m.flatScreen |> AVal.map (fun q -> [|q.P0.ToV3f(); q.P1.ToV3f(); q.P2.ToV3f(); q.P3.ToV3f()|])

        let clipPlaneSg = planeSg planePositions C4f.Blue FillMode.Line
        let quadSg = planeSg quadPositions C4f.White FillMode.Fill

        let tvSg = 
            Loader.Assimp.load (Path.combine [__SOURCE_DIRECTORY__; "..";"..";"models";"tv";"tv.obj"])
                |> Sg.adapter

                |> Sg.transform (Trafo3d.Scale(1.0, 1.0, -1.0))
                |> Sg.scale 2.0
                |> Sg.transform (Trafo3d.RotationEulerInDegrees(90.0, 0.0, -90.0))
                |> Sg.translate 2.5 1.0 1.5

                |> Sg.shader {
                    do! DefaultSurfaces.trafo
                    do! DefaultSurfaces.diffuseTexture
                    do! DefaultSurfaces.normalMap
                    do! DefaultSurfaces.simpleLighting
                }

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

        let probeContrSg = 
            let path = [__SOURCE_DIRECTORY__; "..";"..";"models";"menuControllers";"probe";"probe.obj"]
            controllerSg path 90.0 0.0 30.0 probeScaleTrafo probeContrPos
           
        let laserContrSg = 
            let path = [__SOURCE_DIRECTORY__; "..";"..";"models";"menuControllers";"laser";"laser.obj"]
            controllerSg path 90.0 0.0 0.0 rayScaleTrafo rayContrPos

        let clippingContrSg = 
            let path = [__SOURCE_DIRECTORY__; "..";"..";"models";"menuControllers";"clipping";"clipping.obj"]
            controllerSg path 90.0 0.0 -30.0 clippingScaleTrafo clippingContrPos
            
        Sg.ofSeq [world; heraSg; sphereProbeSg; tvSg; clipPlaneSg; quadSg; probeContrSg; laserContrSg; clippingContrSg; ray]
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.simpleLighting
            }
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
            initial = initial frames
            update = update frames
            threads = threads
            input = input 
            ui = ui runtime data
            vr = vr runtime data
            pauseScene = Some pause
        }