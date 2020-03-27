namespace AardVolume

open System
open Aardvark.Base
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Base.Rendering
open AardVolume.Model

open FSharp.Data.Adaptive

type Message =
    | ToggleModel
    | CameraMessage of FreeFlyController.Message
    | StepTime
    | VolumeVis
    | HeraSimVis

module App =
    
    let initial = { currentModel = Box; cameraState = FreeFlyController.initial; frame = 0; vis = Vis.HeraSimVis }

    let sw = System.Diagnostics.Stopwatch.StartNew()

    let update (m : Model) (msg : Message) =
        match msg with
            | HeraSimVis -> { m with vis = Vis.HeraSimVis }
            | VolumeVis -> { m with vis = Vis.VolumeVis}
            | StepTime -> 
                { m with frame = sw.Elapsed.TotalSeconds / 0.5 |> int }
            | ToggleModel -> 
                match m.currentModel with
                    | Box -> { m with currentModel = Sphere }
                    | Sphere -> { m with currentModel = Box }

            | CameraMessage msg ->
                { m with cameraState = FreeFlyController.update m.cameraState msg }

    let view (runtime : IRuntime) (m : AdaptiveModel) =

        let frustum = 
            Frustum.perspective 60.0 0.1 100.0 1.0 
                |> AVal.constant

        let heraData,volumeData = 
            match Environment.UserName with
            | "hs" -> @"F:\notebooks\sim",  @"F:\notebooks\hechtkopfsalamander male - Copy"
            | _ -> failwith "add your data"

        let heraSg = 
            lazy
                Hera.loadOldCacheFiles runtime heraData 10
                |> Hera.createAnimatedSg m.frame
                |> Sg.noEvents

        let volSg = 
            lazy
                SimpleVolumeRenderer.createSg runtime volumeData
                |> Sg.noEvents

        let sg =
            m.vis |> AVal.map (fun v -> 
                match v with   
                | Vis.HeraSimVis -> heraSg.Value
                | Vis.VolumeVis -> volSg.Value
            ) |> Sg.dynamic

        let att =
            [
                style "position: fixed; left: 0; top: 0; width: 100%; height: 100%"
            ]

        body [] [
            FreeFlyController.controlledControl m.cameraState CameraMessage frustum (AttributeMap.ofList att) sg

            div [style "position: fixed; left: 20px; top: 20px"] [
                button [onClick (fun _ -> VolumeVis)] [text "volume vis"]
                button [onClick (fun _ -> HeraSimVis)] [text "hera sim vis"]
            ]

        ]

    let threads (state : Model) =
         let pool = ThreadPool.empty
    
         let rec time() =
             proclist {
                 do! Proc.Sleep 10
                 yield StepTime
                 yield! time()
             }

         ThreadPool.add "timer" (time()) pool


    let app (runtime : IRuntime) =
        {
            initial = initial
            update = update
            view = view runtime
            threads = threads
            unpersist = Unpersist.instance
        }