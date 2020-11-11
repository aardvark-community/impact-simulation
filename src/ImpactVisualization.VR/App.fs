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
    | ControlSphere of int
    | ReleaseSphere of int 
    | ActivateRay of int
    | DeactivateRay of int
    | ActivatePlane of int
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
            sphereRadius = 1.0
            sphereColor = C4b.White
            sphereProbeCreated = false
            rayDeviceId = None
            ray = Ray3d.Invalid
            flatScreen = Quad3d(V3d(-25,-20,-8), V3d(-25,10,-8), V3d(-25,10,12), V3d(-25,-20,12))
            rayColor = new C4b(178, 223, 138)
            screenIntersection = false
            clippingPlaneDeviceId = None
            planeCorners = Quad3d(V3d(), V3d(), V3d(), V3d())
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
            {model with 
                controllerTrafo = contrTrafo
                sphereControllerTrafo = sphereContrTrafo
                sphereScalerTrafo = sphereScTrafo
                sphereScale = scalingFactor
                devicesTrafos = newInput
                ray = currRay
                planeCorners = currCorners
                screenIntersection = intersect}
        | ControlSphere id -> 
            let currTrafo = model.devicesTrafos.TryFind(id)
            match model.sphereControllerId with 
            | Some i -> if i = id then 
                            {model with 
                                sphereControllerTrafo = currTrafo
                                sphereControllerId = Some id}
                        else
                            {model with 
                                sphereScalerId = Some id
                                sphereScalerTrafo = currTrafo} 
            | None -> 
                    {model with 
                        sphereProbeCreated = true
                        sphereControllerTrafo = currTrafo
                        sphereControllerId = Some id}
        | ReleaseSphere id ->
            match model.sphereControllerId with
            | Some i -> if i = id then 
                            {model with
                                sphereControllerId = None
                                sphereScalerId = None}
                        else
                            {model with sphereScalerId = None}
            | None -> {model with sphereScalerId = None}
        | ActivateRay id -> 
            let currDevice = model.devicesTrafos.TryFind(id)
            let currDeviceTrafo = trafoOrIdentity currDevice
            let origin = currDeviceTrafo.Forward.TransformPos(V3d(0.0, 0.02, 0.0))
            let direction = currDeviceTrafo.Forward.TransformPos(V3d.OIO * 10.0) 
            {model with 
                    rayDeviceId = Some id
                    ray = Ray3d(origin, direction)}
        | ActivatePlane id ->
            let currDevice = model.devicesTrafos.TryFind(id)
            let currDeviceTrafo = trafoOrIdentity currDevice
            let p0 = currDeviceTrafo.Forward.TransformPos(planePos0)
            let p1 = currDeviceTrafo.Forward.TransformPos(planePos1)
            let p2 = currDeviceTrafo.Forward.TransformPos(planePos2)
            let p3 = currDeviceTrafo.Forward.TransformPos(planePos3)
            {model with 
                    planeCorners = Quad3d(p0, p1, p2, p3)
                    clippingPlaneDeviceId = Some id 
                    }
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
                [ActivateRay controllerId]
            else if buttonId = 1 then
                [ControlSphere controllerId]
            else
                []
        | VrMessage.Unpress(controllerId, buttonId) ->
          //  printf "unpress: %A " (controllerId, buttonId)
            if buttonId = 1 then
                [ReleaseSphere controllerId]
            else
                []
        | VrMessage.Touch(controllerId, buttonId) ->
           // printf "touch: %A " (controllerId, buttonId)
            if buttonId = 0 then 
                [ActivatePlane controllerId]
            else 
                []
        | VrMessage.Untouch(controllerId, buttonId) ->
           // printf "untouch: %A " (controllerId, buttonId)
            []
        | VrMessage.ValueChange(controllerId, buttonId, value) ->
            //printf "value change: %A " (controllerId, buttonId, value)
            if buttonId = 0 then 
                let x = value.X
                if x >=  0.0 then 
                    [ScaleUp x]
                else 
                    [ScaleDown x]
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
                printf "radius %A \n" (0.2 * s)
                Trafo3d (Scale3d(s)))

        let sphereTrafo =
            m.sphereControllerTrafo |> AVal.map (fun t ->
                match t with 
                | Some trafo -> 
                    //printf "spherePos %A \n" (trafo.Forward.TransformPos(V3d.OOO))
                    trafo
                | None -> Trafo3d.Identity
                )

        let contrClippingPlane = 
            m.planeCorners |> AVal.map (fun c ->
                Plane3d(c.P0, c.P1, c.P2))
        
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

        
        let sphereProbeSg = 
            Sg.sphere 7 m.sphereColor m.sphereRadius
            |> Sg.noEvents
            |> Sg.trafo sphereScaleTrafo
            |> Sg.trafo sphereTrafo
            |> Sg.onOff m.sphereProbeCreated
            |> Sg.fillMode (FillMode.Line |> AVal.constant)


        let lines = m.ray |> AVal.map (fun r -> 
                                    let line = new Line3d(r.Origin, r.Direction)
                                    [|line|]) 

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
                |> Sg.pass (RenderPass.after "lines" RenderPassOrder.Arbitrary RenderPass.main)
                |> Sg.depthTest (AVal.constant DepthTestMode.None)

        //let quadSg = 
        //    let quad =  
        //        IndexedGeometry(
        //            Mode = IndexedGeometryMode.TriangleList,
        //            IndexArray = ([|0;1;2; 0;2;3|] :> System.Array),
        //            IndexedAttributes = 
        //                SymDict.ofList [
        //                    DefaultSemantic.Positions,                  [| V3f(-25,-20,-8); V3f(-25,10,-8); V3f(-25,10,12); V3f(-25,-20,12) |] :> Array
        //                    DefaultSemantic.Normals,                    [| V3f.OOI; V3f.OOI; V3f.OOI; V3f.OOI |] :> Array
        //                    DefaultSemantic.DiffuseColorCoordinates,    [| V2f.OO; V2f.IO; V2f.II; V2f.OI |] :> Array
        //                    ]
        //        )
        //    quad 
        //    |> Sg.ofIndexedGeometry
        //    |> Sg.shader {
        //        do! DefaultSurfaces.trafo
        //        do! DefaultSurfaces.simpleLighting
        //        do! DefaultSurfaces.constantColor C4f.White
        //    }

        let planePositions = m.planeCorners |> AVal.map (fun q -> [|q.P0.ToV3f(); q.P1.ToV3f(); q.P2.ToV3f(); q.P3.ToV3f()|])

        let clipPlaneSg = 
            Sg.draw IndexedGeometryMode.TriangleList
                |> Sg.vertexAttribute DefaultSemantic.Positions planePositions
                |> Sg.vertexAttribute DefaultSemantic.Normals (AVal.constant [| V3f.OOI; V3f.OOI; V3f.OOI; V3f.OOI |])
                |> Sg.vertexAttribute DefaultSemantic.DiffuseColorCoordinates  (AVal.constant  [| V2f.OO; V2f.IO; V2f.II; V2f.OI |])
                |> Sg.index (AVal.constant [|0;1;2; 0;2;3|])
                |> Sg.shader {
                    do! DefaultSurfaces.trafo
                    do! DefaultSurfaces.simpleLighting
                    do! DefaultSurfaces.constantColor C4f.Blue
                }
                |> Sg.fillMode (FillMode.Line |> AVal.constant)

        let quadPositions = m.flatScreen |> AVal.map (fun q -> [|q.P0.ToV3f(); q.P1.ToV3f(); q.P2.ToV3f(); q.P3.ToV3f()|])

        let quadSg = 
            Sg.draw IndexedGeometryMode.TriangleList
                |> Sg.vertexAttribute DefaultSemantic.Positions quadPositions
                |> Sg.vertexAttribute DefaultSemantic.Normals (AVal.constant [| V3f.OOI; V3f.OOI; V3f.OOI; V3f.OOI |])
                |> Sg.vertexAttribute DefaultSemantic.DiffuseColorCoordinates  (AVal.constant  [| V2f.OO; V2f.IO; V2f.II; V2f.OI |])
                |> Sg.index (AVal.constant [|0;1;2; 0;2;3|])
                |> Sg.shader {
                    do! DefaultSurfaces.trafo
                    do! DefaultSurfaces.simpleLighting
                    do! DefaultSurfaces.constantColor C4f.White
                }

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
            
        Sg.ofSeq [world; heraSg; sphereProbeSg; tvSg; clipPlaneSg; quadSg; ray]
            |> Sg.shader {
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

