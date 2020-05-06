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

//type Dim = X | Y | Z
//type M = | Set of Dim * float

type Message =
    | ToggleModel
    | CameraMessage of FreeFlyController.Message
    | StepTime
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
        cameraState = FreeFlyController.initial; 
        frame = 0;
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
        clippingPlaneX = 16.0
        clippingPlaneY = 30.0
        clippingPlaneZ = 16.0
        data = [-1.25; -10.5; -1.0; -0.25; 0.0; 0.65; 1.2; 2.3; 1.7; 2.9; 25.7; 4.4; 5.31; 4.1]
        // 5.0; 1.2; 3.4; 2.1; 3.6; 5.6; 4.4; 0.5; 0.2; 1.9; 5.2; 2.7; 50.0
        count =  { min = 100.0; max = 5000.0; value = 1000.0; step = 100.0; format = "{0:0}" }
    }

    let sw = System.Diagnostics.Stopwatch.StartNew()

    let rnd = RandomSystem()
    let normal = RandomGaussian(rnd)

    let mutable energyPts = {
        energyPoints1 = List.empty<float>
        energyPoints2 = List.empty<float>
    }
    
    let mutable pts1 = true

    let update (m : Model) (msg : Message) =
        match msg with
            | SetPointSize s -> { m with pointSize = s }
            | TogglePointSize -> 
                let newPointSize = if m.pointSize = 8.0 then 20.0 elif m.pointSize = 20.0 then 8.0 else m.pointSize
                { m with pointSize = newPointSize }
            | ChangeAnimation -> { m with playAnimation = not m.playAnimation}
            | StepTime -> 
                let frameId =  sw.Elapsed.TotalSeconds / 0.5 |> int
                // update filtered data using frameId and filter
                if m.playAnimation then sw.Start() else sw.Stop()
                { m with frame = frameId }
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
            | SetSlideX x -> {m with clippingPlaneX = x}
            | SetSlideY y -> {m with clippingPlaneY = y}
            | SetSlideZ z -> {m with clippingPlaneZ = z}
            | Generate -> 
                let points = if pts1 then energyPts.energyPoints1 else energyPts.energyPoints2
                pts1 <- not pts1
                { m with 
                    data = points;
                }
            | ChangeCount n -> { m with count = Numeric.update m.count n }



    let renderValues = AMap.ofArray((Enum.GetValues typeof<RenderValue> :?> (RenderValue [])) |> Array.map (fun c -> (c, text (Enum.GetName(typeof<RenderValue>, c)) )))

    let view (runtime : IRuntime) (m : AdaptiveModel) =

        let frustum = 
            Frustum.perspective 60.0 0.1 100.0 1.0 
                |> AVal.constant

        let heraData = @"..\..\..\data"

        let serializer = MBrace.FsPickler.FsPickler.CreateBinarySerializer()

        let prepareData (d : Parser.Data) : Hera.Frame = 
            let frame = {
                vertices    = runtime.PrepareBuffer (ArrayBuffer d.vertices) :> IBuffer
                velocities  = runtime.PrepareBuffer (ArrayBuffer d.velocities) :> IBuffer
                energies    = runtime.PrepareBuffer (ArrayBuffer (Array.map float32 d.internalEnergies)) :> IBuffer
                cubicRoots  = runtime.PrepareBuffer (ArrayBuffer (Array.map float32 d.cubicRootsOfDamage)) :> IBuffer
                strains     = runtime.PrepareBuffer (ArrayBuffer (Array.map float32 d.localStrains)) :> IBuffer
                alphaJutzis = runtime.PrepareBuffer (ArrayBuffer (Array.map float32 d.alphaJutzis)) :> IBuffer
                pressures   = runtime.PrepareBuffer (ArrayBuffer (Array.map float32 d.pressures)) :> IBuffer
                positions   = d.vertices }
            frame

        let cachedFileEnding = "_cache_2"

        let mutable energyPoints = Array.empty<float>;

        let convertToCacheFile (fileName : string) =
            let cacheName = fileName + cachedFileEnding
            if not (File.Exists cacheName) then
                let d,b = Parser.parseFile fileName
                let data = serializer.Pickle((d,b))
                File.writeAllBytes cacheName data
        
        let loadDataAndCache (fileName : string) =
            if fileName.EndsWith(cachedFileEnding) then
                let bytes = File.readAllBytes fileName
                let (d : Parser.Data, b : Box3f) = serializer.UnPickle(bytes) // exception hier wenn nicht richtiger inhalt... 

                if fileName.EndsWith("impact.0110_cache_2") then
                    energyPoints <- d.internalEnergies

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

        let domainRange = {
            minX = m.minX
            maxX = m.maxX
            minY = m.minY
            maxY = m.maxY
            minZ = m.minZ
            maxZ = m.maxZ
        } 

        let clippingPlane = {
            clippingPlaneX = m.clippingPlaneX
            clippingPlaneY = m.clippingPlaneY
            clippingPlaneZ = m.clippingPlaneZ
        }
    
        let heraSg = 
            framesToAnimate 0 7
            |> Hera.createAnimatedSg m.frame m.pointSize m.renderValue m.currentMap domainRange clippingPlane
            |> Sg.noEvents

        energyPts <- {
            energyPoints1 = energyPoints.[1..100] |> Array.toList
            energyPoints2 = energyPoints.[200..300] |> Array.toList
        }

        let sg = heraSg
            
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
                { kind = Script; name = "d3"; url = "http://d3js.org/d3.v4.min.js" }
                { kind = Stylesheet; name = "histogramStyle"; url = "Histogram.css" }
                { kind = Script; name = "histogramScript"; url = "Histogram.js" }
            ]
        
        let dataChannel = m.data.Channel
        let updateChart =
            "initHisto(__ID__); data.onmessage = function (values) { refresh(values); };"  // subscribe to m.data

        body [] [
            FreeFlyController.controlledControl m.cameraState CameraMessage frustum (AttributeMap.ofList att) sg
            require dependencies ( // here. we executit Histogram.js.... all values, functions defined in Histogram.js are further down accessible...

                div[] [
                    div [style "position: fixed; left: 20px; top: 20px; width: 218px"] [
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
                            slider { min = -16.0; max = 16.0; step = 0.5 } [clazz "ui inverted slider"] m.clippingPlaneX SetSlideX
                            span [style "padding: 2px"] [text "Y: "]
                            slider { min = -16.0; max = 30.0; step = 0.5 } [clazz "ui inverted slider"] m.clippingPlaneY SetSlideY
                            span [style "padding: 2px"] [text "Z: "]
                            slider { min = -16.0; max = 16.0; step = 0.5 } [clazz "ui inverted slider"] m.clippingPlaneZ SetSlideZ

                        ]
                    ]
                    ]
                
                    onBoot' ["data", dataChannel] updateChart ( // when div [histogram etc] is constructed updateChart is called.
                        div [clazz "histogram"; style "position: fixed; bottom: 20px; right: 20px; width:300px; height: 200px; background-color: #ffffff; border: 3px solid #ffffff; z-index: 1000"] [                       
                            div [] [
                                Numeric.view m.count |> UI.map ChangeCount
                                text "  "
                                button [onClick (fun _ -> Generate)] [text "Generate"]
                            ]
                        ]
                    )
                ]
                
            )  
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
        // load data....
        {
            initial = initial // store data Hera frame as array to model....
            update = update
            view = view runtime
            threads = threads
            unpersist = Unpersist.instance
        }