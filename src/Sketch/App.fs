namespace AardVolume

open System
open System.IO
open Aardvark.Base
open Aardvark.SceneGraph
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Base.Rendering
open AardVolume.Model
open Hera

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
    | ChangeAnimation
    | SetRenderValue of RenderValue
    | SetTransferFunc of option<string>
    | SetMinX of float
    | SetMaxX of float
    | SetMinY of float
    | SetMaxY of float
    | SetMinZ of float
    | SetMaxZ of float
    | SetSlideX of float
    | SetSlideY of float
    | SetSlideZ of float
    | Generate 
    | ChangeCount of Numeric.Action
    

module App =

    let tfsDir = @"..\..\..\data\transfer"

    let transferFunctions = 
        Directory.EnumerateFiles tfsDir  
        |> Seq.toList

    let listWithValues =
        transferFunctions
        |> List.mapi (fun i t ->
                let text = "Color Map " + string i
                (t, text)
                )
        |> HashMap.ofList


    let initial = { 
        currentModel = Box; cameraState = FreeFlyController.initial; 
        frame = 0; vis = Vis.HeraSimVis
        pointSize = 8.0
        playAnimation = true
        renderValue = RenderValue.Energy
        colorMaps = listWithValues
        currentMap = @"..\..\..\data\transfer\map-03.png"
        minX = -16.0
        maxX = 16.0
        minY = -16.0
        maxY = 30.0 
        minZ = -16.0
        maxZ = 16.0
        slideX = 16.0
        slideY = 30.0
        slideZ = 16.0
        data = []
        count =  { min = 100.0; max = 5000.0; value = 1000.0; step = 100.0; format = "{0:0}" }
    }

    let sw = System.Diagnostics.Stopwatch.StartNew()

    let rnd = RandomSystem()
    let normal = RandomGaussian(rnd)

    let update (m : Model) (msg : Message) =
        match msg with
            | SetPointSize s -> { m with pointSize = s }
            | TogglePointSize -> 
                let newPointSize = if m.pointSize = 8.0 then 20.0 elif m.pointSize = 20.0 then 8.0 else m.pointSize
                { m with pointSize = newPointSize }
            | HeraSimVis -> { m with vis = Vis.HeraSimVis }
            | VolumeVis -> { m with vis = Vis.VolumeVis;  }
            | ChangeAnimation -> { m with playAnimation = not m.playAnimation}
            | StepTime -> 
                if m.playAnimation then sw.Start() else sw.Stop()
                { m with frame = sw.Elapsed.TotalSeconds / 0.5 |> int }
            | ToggleModel -> 
                match m.currentModel with
                    | Box -> { m with currentModel = Sphere }
                    | Sphere -> { m with currentModel = Box }
            | CameraMessage msg ->
                { m with cameraState = FreeFlyController.update m.cameraState msg }
            | SetRenderValue v -> {m with renderValue = v}
            | SetTransferFunc t -> 
                match t with    
                | None -> m
                | Some(s) -> {m with currentMap = s} 
            | SetMinX x -> {m with minX = x}
            | SetMaxX x -> {m with maxX = x}
            | SetMinY y -> {m with minY = y}
            | SetMaxY y -> {m with maxY = y}
            | SetMinZ z -> {m with minZ = z}
            | SetMaxZ z -> {m with maxZ = z}
            | SetSlideX x -> {m with slideX = x}
            | SetSlideY y -> {m with slideY = y}
            | SetSlideZ z -> {m with slideZ = z}
            | Generate -> 
                { m with 
                    data = 
                        Array.init (m.count.value |> round |> int) (fun _ -> normal.GetDouble(20.0,5.0)) |> Array.toList 
                }
            | ChangeCount n -> { m with count = Numeric.update m.count n }



    let renderValues = AMap.ofArray((Enum.GetValues typeof<RenderValue> :?> (RenderValue [])) |> Array.map (fun c -> (c, text (Enum.GetName(typeof<RenderValue>, c)) )))

    let view (runtime : IRuntime) (m : AdaptiveModel) =

        let frustum = 
            Frustum.perspective 60.0 0.1 100.0 1.0 
                |> AVal.constant

        let heraData,volumeData = 
            @"..\..\..\data", @"C:\Users\hs\OneDrive\notebookdata\hechtkopfsalamander male - Copy"
            //match Environment.UserName with
            //| "hs" -> @"C:\Users\hs\OneDrive\notebookdata\sim",  @"C:\Users\hs\OneDrive\notebookdata\hechtkopfsalamander male - Copy"
            //| _ -> failwith "add your data"

        let serializer = MBrace.FsPickler.FsPickler.CreateBinarySerializer()

        let prepareData (d : Parser.Data) : Hera.Frame = 
            let frame = {
                vertices = runtime.PrepareBuffer (ArrayBuffer d.vertices) :> IBuffer
                velocities = runtime.PrepareBuffer (ArrayBuffer d.velocities) :> IBuffer
                energies = runtime.PrepareBuffer (ArrayBuffer (Array.map float32 d.internalEnergies)) :> IBuffer
                cubicRoots = runtime.PrepareBuffer (ArrayBuffer (Array.map float32 d.cubicRootsOfDamage)) :> IBuffer
                strains = runtime.PrepareBuffer (ArrayBuffer (Array.map float32 d.localStrains)) :> IBuffer
                alphaJutzis = runtime.PrepareBuffer (ArrayBuffer (Array.map float32 d.alphaJutzis)) :> IBuffer
                pressures = runtime.PrepareBuffer (ArrayBuffer (Array.map float32 d.pressures)) :> IBuffer
                positions = d.vertices }
            frame

        let cachedFileEnding = "_cache_2"

        let convertToCacheFile (fileName : string) =
            let cacheName = fileName + cachedFileEnding
            if not (File.Exists cacheName) then
                let d,b = Parser.parseFile fileName
                let data = serializer.Pickle((d,b))
                File.writeAllBytes cacheName data
        
        let loadDataAndCache (fileName : string) =
            if fileName.EndsWith(cachedFileEnding) then
                let bytes = File.readAllBytes fileName
                let (d, b : Box3f) = serializer.UnPickle(bytes) // exception hier wenn nicht richtiger inhalt... 
                
                let buffer = prepareData(d)
                let vertexCount = d.vertices.Length

                Some(buffer, b, vertexCount)                 
                
            else
                convertToCacheFile fileName
                None           


        let allFiles = 
            Directory.EnumerateFiles heraData |> Seq.toArray 
            |> Array.map loadDataAndCache
            |> Array.choose (fun elem -> elem)

        //TODO: Check for all possible exceptions! -> no data, endFrame > startFrame 
        let framesToAnimate startFrame frames =
            let l = allFiles.Length
            if l = 0 then
                Log.warn "There is no data!"
                Array.empty, Box3f.Invalid, 0
            else
                let mutable endFrame = 0
                if frames > l then endFrame <- l - 1 else endFrame <- frames - 1 //check if out of bounds
                let framesToAnimate = allFiles.[startFrame..endFrame]
                let buffers = framesToAnimate |> Array.map (fun (elem, _, _) ->  elem)
                let box = framesToAnimate.[0] |> (fun (_, b, _) -> b)
                let vertexCount = framesToAnimate.[0] |> (fun (_, _, c) -> c)
                buffers, box, vertexCount

        let axis = {
            minX = m.minX
            maxX = m.maxX
            minY = m.minY
            maxY = m.maxY
            minZ = m.minZ
            maxZ = m.maxZ
        } 

        let slide = {
            slideX = m.slideX
            slideY = m.slideY
            slideZ = m.slideZ
        }
    
        let heraSg = 
            framesToAnimate 0 7
            |> Hera.createAnimatedSg m.frame m.pointSize m.renderValue m.currentMap axis slide
            |> Sg.noEvents

        //let heraSg = 
        //    Hera.loadOldCacheFiles runtime heraData 10
        //    |> Hera.createAnimatedSg m.frame m.pointSize
        //    |> Sg.noEvents

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
            
              
       
        //let clipPlanes = Sg.empty

        //let sg = Sg.ofList [sg; clipPlanes]

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

        let dynamicNameChange =
            alist {
                let! animation = m.playAnimation

                if animation = true then
                    yield button [onClick (fun _ -> ChangeAnimation)] [text "Pause"]
                else 
                    yield button [onClick (fun _ -> ChangeAnimation)] [text "Play"]
            }

        let colorMaps = AMap.map (fun k v -> text v) m.colorMaps

        let dependencies = 
            [
                { kind = Script; name = "d3"; url = "http://d3js.org/d3.v3.min.js" }
                { kind = Stylesheet; name = "histogramStyle"; url = "resources\Histogram.css" }
                { kind = Script; name = "histogramScript"; url = "resources\Histogram.js" }
            ]

        let dataChannel = m.data.Channel
        let updateChart =
            "data.onmessage = function (values) { if(values.length > 0) refresh(values); };"

        body [] [
            FreeFlyController.controlledControl m.cameraState CameraMessage frustum (AttributeMap.ofList att) sg

            div [style "position: fixed; left: 20px; top: 20px; width: 218px"] [
                button [onClick (fun _ -> VolumeVis)] [text "volume vis"]
                button [onClick (fun _ -> HeraSimVis)] [text "hera sim vis"]
                br []
                br []
                Incremental.div AttributeMap.empty dynamicNameChange
                br []
                numeric { min = 1.0; max = 30.0; smallStep = 1.0; largeStep = 5.0 } AttributeMap.empty m.pointSize SetPointSize
                Incremental.div AttributeMap.empty dynamicUI
                br []
                dropdown { placeholder = "ColorMaps"; allowEmpty = false} [ clazz "ui selection" ] colorMaps (m.currentMap |> AVal.map Some) SetTransferFunc
                br []
                br []
                dropdown1 [ clazz "ui selection" ] renderValues m.renderValue SetRenderValue
                br []
                br []
                div [ style "color: white" ] [

                    div [ clazz "item" ] [
                        span [style "padding: 2px"] [text "min X: "]
                        simplenumeric {
                            attributes [style "width: 25%; padding: 2px"]
                            value m.minX
                            update SetMinX
                            step 1.0
                            largeStep 5.0
                            min -25.0
                            max 0.0
                        }
                        span [style "padding: 2px"] [text "max X: "]
                        simplenumeric {
                            attributes [style "width: 25%; padding: 2px"]
                            value m.maxX
                            update SetMaxX
                            step 1.0
                            largeStep 5.0
                            min 0.0
                            max 25.0
                        }
                        br []
                        br []
                        span [style "padding: 2px"] [text "min Y: "]
                        simplenumeric {
                            attributes [style "width: 25%; padding: 2px"]
                            value m.minY
                            update SetMinY
                            step 1.0
                            largeStep 5.0
                            min -25.0
                            max 0.0
                        }
                        span [style "padding: 2px"] [text "max Y: "]
                        simplenumeric {
                            attributes [style "width: 25%; padding: 2px"]
                            value m.maxY
                            update SetMaxY
                            step 1.0
                            largeStep 5.0
                            min 0.0
                            max 30.0
                        }
                        br []
                        br []
                        span [style "padding: 2px"] [text "min Z: "]
                        simplenumeric {
                            attributes [style "width: 25%; padding: 2px"]
                            value m.minZ
                            update SetMinZ
                            step 1.0
                            largeStep 5.0
                            min -25.0
                            max 0.0
                        }
                        span [style "padding: 2px"] [text "max Z: "]
                        simplenumeric {
                            attributes [style "width: 25%; padding: 2px"]
                            value m.maxZ
                            update SetMaxZ
                            step 1.0
                            largeStep 5.0
                            min 0.0
                            max 25.0
                        }
                    ]
                    
                    div [ clazz "item"; style "width: 90%" ] [
                        span [style "padding: 2px"] [text "X: "]
                        slider { min = -16.0; max = 16.0; step = 0.5 } [clazz "ui inverted slider"] m.slideX SetSlideX
                        span [style "padding: 2px"] [text "Y: "]
                        slider { min = -16.0; max = 30.0; step = 0.5 } [clazz "ui inverted slider"] m.slideY SetSlideY
                        span [style "padding: 2px"] [text "Z: "]
                        slider { min = -16.0; max = 16.0; step = 0.5 } [clazz "ui inverted slider"] m.slideZ SetSlideZ

                    ]
                ]
            ]

            div [clazz "histogram"; style "position: fixed; bottom: 20px; right: 20px; border: 3px solid #ffffff"] [
                require dependencies (
                    onBoot' ["data", dataChannel] updateChart (
                        div [] [
                            Numeric.view m.count |> UI.map ChangeCount
                            text "  "
                            button [onClick (fun _ -> Generate)] [text "Generate"]
                        ]
                    )
                )                        
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