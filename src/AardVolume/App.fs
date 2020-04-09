namespace AardVolume

open System
open Aardvark.Base
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Base.Rendering
open AardVolume.Model

open FSharp.Data.Adaptive

//{
//	"data": "data",
//	"mark": "splat",
//	"encoding": {
//		"position": {
//			"x": "datum.u - 0.5",
//			"y": "(Math.cos(datum.u * Math.PI * 4) + Math.sin(datum.v * Math.PI * 4)) / 20",
//			"z": "datum.v - 0.5"
//		},
//		"color": {
//			"r": "datum.velocity.x",
//			"g": "datum.velocity.y",
//			"b": "1",
//			"mapping": "color + 0.5"
//		},
//		"size": "y * 2.05"
//	},
//	"selection": {
//		"raycast": {
//			"size": "0.5",
//			"label": "position.y.toFixed(3) + ' m'"
//		}
//	}
//}

//module DSLExp = 

//    open FSharp.Data
    
//    [<Literal>]
//    let data="""x(float),y(float),z(float),vx(float),vy(float),vz(float),my(float)
//0.0f,0.0f,0.f0,0.0f,1.0f,0.0f,10.0f"""

//    type Points = CsvProvider<data>

//    let a = Points.Load("")

//    type Mark = Splat

//    let vis () = 
//        {|
//            data = Points.Load ""
//            mark = Splat
//            encoding = 
//                fun (d : CsvProvider<data>) ->
//                    {|
//                        position = 
//                            d.Rows |> Seq.map (fun r -> V3d(r.X,r.Y,r.Z))
//                    |}
//        |}
    

//    type Data<'d> = 
//        | Columns of Map<string, Type * System.Array>
//        | Computation of amap<string, Type * aval<Object[]>>
//        | Dense of Volume<'d>

//    type Vis<'a> =
//        {
//            data : Data<'a>
//        }

//    type Encoding<'d,'v> = 
//        | ForEachRow of ('d -> 'v)

//    type Vis<'d,'v> = 
//        {
//            data : Data<'d>
//            encoding : list<string * Encoding<'d,'v>>
//        }

//    let array (arr : array<'a>) = 
//        typeof<'a>, arr :> System.Array

//    let vis2 = 
//        {
//            data = Data<V3d * V3d>.Columns <| Map.ofList 
//                        [ 
//                            "x", array [| 0.0, 0.0 |] 
//                            "y", array [| 0.0, 0.0 |] 
//                            "z", array [| 0.0, 0.0 |] 
//                            "vx", array [| 0.0, 0.0 |] 
//                            "vy", array [| 0.0, 0.0 |] 
//                            "vz", array [| 0.0, 0.0 |] 
//                        ]
//            encoding = 
//                [
//                    "colors", ForEachRow (fun (vertex, velocity) -> 
//                        velocity * 0.5 + V3d.Half
//                    )
//                ]
//        }



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
            | "hs" -> @"C:\Users\hs\OneDrive\notebookdata\sim",  @"C:\Users\hs\OneDrive\notebookdata\hechtkopfsalamander male - Copy"
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