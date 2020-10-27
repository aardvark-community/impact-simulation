﻿namespace ImpactVisualization

open Aardvark.Base
open Aardvark.Rendering.Text
open Aardvark.Vr
open Aardvark.SceneGraph
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
    | ScaleUp
    | ScaleDown 
    | MoveController of int * Trafo3d
    | CreateSphere 
 
module Demo =
    open Aardvark.UI.Primitives
    open Aardvark.Base.Rendering

    let initial frames = 
        {   
            twoDModel = AardVolume.App.initial frames
            text = "hello" 
            grabTrafo = None
            grabberId = None
            modelTrafo = None
            scalingFactor = 0.05
            sphereProbe = false
        }

        
    let update (frames : Frame[]) (state : VrState) (vr : VrActions) (model : Model) (msg : Message) =
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
                | None -> {model with grabberId = Some id}
        | Ungrab id -> 
            match model.grabberId with 
                | Some i -> 
                    if i = id then 
                        {model with 
                            grabberId = None
                            modelTrafo = model.grabTrafo} 
                    else model
                | None -> model
        | ScaleUp -> {model with scalingFactor = model.scalingFactor * 1.05}
        | ScaleDown -> {model with scalingFactor = model.scalingFactor * 0.95}
        | MoveController (id, trafo) -> 
            match model.grabberId with 
                | Some i -> if i = id then {model with grabTrafo = Some(trafo)} else model
                | None -> model
        | CreateSphere -> {model with sphereProbe = not model.sphereProbe}
            

    let threads (model : Model) =
        AardVolume.App.threads model.twoDModel |> ThreadPool.map TwoD
        
    let input (msg : VrMessage) =
        match msg with
        | VrMessage.PressButton(controllerId, buttonId) ->
           // printf "press button: %A " (controllerId, buttonId)
            if buttonId = 1 then 
                [CreateSphere]
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
            []
        | VrMessage.Unpress(controllerId, buttonId) ->
          //  printf "unpress: %A " (controllerId, buttonId)
            []
        | VrMessage.Touch(controllerId, buttonId) ->
           // printf "touch: %A " (controllerId, buttonId)
            []
        | VrMessage.Untouch(controllerId, buttonId) ->
           // printf "untouch: %A " (controllerId, buttonId)
            []
        | VrMessage.ValueChange(controllerId, buttonId, value) ->
            //printf "value change: %A " (controllerId, buttonId, value)
            if buttonId = 0 then 
                if value.X >= 0.0 then 
                    [ScaleUp]
                else 
                    [ScaleDown]
            else 
                []
        | VrMessage.UpdatePose(controllerId, pose) ->
            if pose.isValid then [MoveController (controllerId, pose.deviceToWorld)] else []
        | _ -> []

    let ui (runtime : IRuntime) (data : Frame[] * Box3f * int) (info : VrSystemInfo) (m : AdaptiveModel) : DomNode<Message> = // 2D UI
        div [] [
            //button [style "z-index : 1000; position: absolute"; onClick (fun _ -> StartVr)] [text "start vr"]
            //br []
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


        let grabTrafo =
            m.grabTrafo |> AVal.map (fun t ->
                match t with 
                | Some trafo -> trafo
                | None -> Trafo3d.Identity
                )

        let modelTrafo =
            m.modelTrafo |> AVal.map (fun t ->
                match t with 
                | Some trafo -> trafo
                | None -> Trafo3d.Identity
                )

        let scaleTrafo = m.scalingFactor |> AVal.map(fun s -> Trafo3d (Scale3d(s)))
        
        let scaledTrafo = AVal.map2 (fun t s -> s * t) grabTrafo scaleTrafo

        let heraSg =    
            let m = m.twoDModel
            data
            |> Hera.Hera.createAnimatedSg 
                m.frame m.pointSize m.discardPoints m.renderValue m.currentMap 
                m.domainRange m.clippingPlane m.filter m.currFilters m.dataRange m.colorValue.c m.cameraState.view
                 runtime
            |> Sg.noEvents
            //|> Sg.translate 0.0 0.0 0.7
            |> Sg.scale 0.05
            |> Sg.trafo modelTrafo
            |> Sg.trafo grabTrafo

        //let sphereProbe = 
        //    Sg.sphere' 9 C4b.White 1.0
        //    |> Sg.noEvents
        //    |> Sg.onOff

        //    Trafo3d.fr

        //let objects = 
        //    m.sphereProbe |> AVal.map (fun s ->
        //        if s then 
        //            [world ; sphereProbe]
        //        else 
        //            [world])

        //Sg.dynamic objects

        Sg.ofSeq [ world; heraSg]
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

