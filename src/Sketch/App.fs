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

module DSLExp = 

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
    

    type Data<'d> = 
        | Columns of Map<string, Type * System.Array>
        | Computation of amap<string, Type * aval<Object[]>>
        | Dense of Volume<'d>


    type Encoding<'d,'v> = 
        | ForEachRow of ('d -> 'v)

    type VisualProperties = 
        {
            camera : CameraView
            mouseDown : bool
        }

    type Message<'msg> = 
        | Msg of 'msg
        | Continue

    type Vis<'d,'v, 'msg> = 
        {
            data : Data<'d>
            encoding : list<string * Encoding<'d,'v>>
            interactions : Message<'msg> -> Vis<'d,'v, 'msg> -> Vis<'d,'v, 'msg> 
            //continuation : Vis<'d,'v,'msg> -> Vis<'d,'v,'msg>
            properties : VisualProperties
        }

    let orbitController (v : Vis<'d,'v,'msg>) : Vis<'d,'v,'msg> = failwith ""

    let array (arr : array<'a>) = 
        typeof<'a>, arr :> System.Array

    type Message = 
        | CenterScene
        | OnMouseMouve

    let vis2 = 
        {
            data = Data<V3d * V3d>.Columns <| Map.ofList 
                        [ 
                            "x", array [| 0.0, 0.0 |] 
                            "y", array [| 0.0, 0.0 |] 
                            "z", array [| 0.0, 0.0 |] 
                            "vx", array [| 0.0, 0.0 |] 
                            "vy", array [| 0.0, 0.0 |] 
                            "vz", array [| 0.0, 0.0 |] 
                        ]
            encoding = 
                [
                    "colors", ForEachRow (fun (vertex, velocity) -> 
                        velocity * 0.5 + V3d.Half
                    )
                ]

            properties = { camera = CameraView.lookAt V3d.III V3d.OOO V3d.OII; mouseDown = false }

            interactions = 
                fun (msg : Message<Message>) (self) -> 
                    match msg with
                        | Msg(CenterScene) -> // update storage, return new vis
                            //{ self with continuation = fun self -> freeFlyController self }
                            self
                        //| OnMouseMouve -> { self with properties = { self.properties with mouseDown = true }}
                        | Continue -> if self.properties.mouseDown then failwith "gah" else failwith "other"

        } 



type Message =
    | ToggleModel
    | CameraMessage of FreeFlyController.Message
    | StepTime
    | VolumeVis
    | HeraSimVis
    | TogglePointSize
    | SetPointSize of float

module App =
    
    let initial = { 
        currentModel = Box; cameraState = FreeFlyController.initial; 
        frame = 0; vis = Vis.HeraSimVis
        pointSize = 8.0
    }

    let sw = System.Diagnostics.Stopwatch.StartNew()

    let update (m : Model) (msg : Message) =
        match msg with
            | SetPointSize s -> { m with pointSize = s }
            | TogglePointSize -> 
                let newPointSize = if m.pointSize = 8.0 then 20.0 elif m.pointSize = 20.0 then 8.0 else m.pointSize
                { m with pointSize = newPointSize }
            | HeraSimVis -> { m with vis = Vis.HeraSimVis }
            | VolumeVis -> { m with vis = Vis.VolumeVis;  }
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
            @"..\..\..\data", @"C:\Users\hs\OneDrive\notebookdata\hechtkopfsalamander male - Copy"
            //match Environment.UserName with
            //| "hs" -> @"C:\Users\hs\OneDrive\notebookdata\sim",  @"C:\Users\hs\OneDrive\notebookdata\hechtkopfsalamander male - Copy"
            //| _ -> failwith "add your data"

        let heraSg = 
            Hera.loadOldCacheFiles runtime heraData 10
            |> Hera.createAnimatedSg m.frame m.pointSize
            |> Sg.noEvents

        let volSg = 
            lazy
                SimpleVolumeRenderer.createSg runtime volumeData
                |> Sg.noEvents

        let sg =
            m.vis |> AVal.map (fun v -> 
                match v with   
                | Vis.HeraSimVis -> heraSg
                | Vis.VolumeVis -> volSg.Value
            ) |> Sg.dynamic

        let att =
            [
                style "position: fixed; left: 0; top: 0; width: 100%; height: 100%"
            ]

        let dynamicUI = 
            alist {
                let! p = m.pointSize

                if p = 8.0 || p = 20.0 then 
                    yield button [onClick (fun _ -> TogglePointSize)] [text "toggle point size"]
                else 
                    yield text "cannot toogle you superstar"
            }


        body [] [
            FreeFlyController.controlledControl m.cameraState CameraMessage frustum (AttributeMap.ofList att) sg

            div [style "position: fixed; left: 20px; top: 20px"] [
                button [onClick (fun _ -> VolumeVis)] [text "volume vis"]
                button [onClick (fun _ -> HeraSimVis)] [text "hera sim vis"]
                br []
                br []
                br []
                numeric { min = 1.0; max = 30.0; smallStep = 1.0; largeStep = 5.0 } AttributeMap.empty m.pointSize SetPointSize

                Incremental.div AttributeMap.empty dynamicUI


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