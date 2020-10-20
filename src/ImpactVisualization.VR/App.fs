namespace ImpactVisualization

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
 
module Demo =
    open Aardvark.UI.Primitives
    open Aardvark.Base.Rendering

    let initial frames = 
        {   
            twoDModel = AardVolume.App.initial frames
            text = "hello" 
        }

        
    let update (frames : Frame[]) (state : VrState) (vr : VrActions) (model : Model) (msg : Message) =
        match msg with
        | ThreeD _ -> model
        | Nop -> model
        | TwoD msg -> 
            let sup = AardVolume.App.update frames model.twoDModel msg
            { model with twoDModel = sup }

    let threads (model : Model) =
        ThreadPool.empty
        
    let input (msg : VrMessage) =
        match msg with
        | VrMessage.PressButton(_,1) ->
            []
        | _ -> 
            []

    let ui (runtime : IRuntime) (data : Frame[] * Box3f * int) (info : VrSystemInfo) (m : AdaptiveModel) : DomNode<Message> = // 2D UI
        AardVolume.App.view runtime data m.twoDModel |> UI.map TwoD

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

        let heraSg =    
            let m = m.twoDModel
            data
            |> Hera.Hera.createAnimatedSg 
                m.frame m.pointSize m.discardPoints m.renderValue m.currentMap 
                m.domainRange m.clippingPlane m.filter m.currFilters m.dataRange m.colorValue.c m.cameraState.view
                 runtime
            |> Sg.noEvents

        Sg.ofSeq [ world; heraSg ]

        
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

