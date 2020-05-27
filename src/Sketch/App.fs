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

type Message =
    | ToggleModel
    | CameraMessage of FreeFlyController.Message
    | StepTime
    | TogglePointSize
    | SetPointSize of float
    | ChangeAnimation
    | SetRenderValue of RenderValue
    | SetTransferFunc of option<string>
    | SetRange of Dim * Value * float
    | SetDataRange of Value * float
    | SetClippingPlane of Dim * float
    | SetFilter of int
    | ResetFilter 

    
module App =    

    let tfsDir = @"..\..\..\src\Sketch\resources\transfer"

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


    let initial (frames : Frame[]) = 
        let initValues = frames.[0].values.energies
        let range = initValues.GetBoundingRange()
        let minValue = range.Min
        let maxValue = range.Max

        let model = 
            { 
            cameraState = FreeFlyController.initial; 
            frame = 0;
            pointSize = 8.0
            playAnimation = true
            renderValue = RenderValue.Energy
            colorMaps = listWithValues
            currentMap = @"..\..\..\src\Sketch\resources\transfer\map-03.png"
            domainRange = {
                x = {min = -16.0; max = 16.0}; 
                y = {min = -16.0; max = 30.0}; 
                z = {min = -16.0; max = 16.0}
                }            
            clippingPlane = {
                x = 16.0
                y = 30.0
                z = 16.0
                }
            dataRange = {
                min = minValue
                max = maxValue
            }
            initDataRange = {
                min = minValue
                max = maxValue
            }
            values = initValues
            data = []
            // -1.25; -10.5; -1.0; -0.25; 0.0; 0.65; 1.2; 2.3; 1.7; 2.9; 25.7; 4.4; 5.31; 4.1
            // 5.0; 1.2; 3.4; 2.1; 3.6; 5.6; 4.4; 0.5; 0.2; 1.9; 5.2; 2.7; 50.0
            filter = None
            filtered = []
            }
        model

    let sw = System.Diagnostics.Stopwatch.StartNew()

    let filteri condition values = 
        values
        |> Array.mapi (fun i v -> (v, i))
        |> Array.filter (fun (v, i) -> condition v)
        |> Array.map (fun (v, i) -> i)

    let filterValue condition (pos : V3f[]) (value : float[]) = 
        Array.zip pos value
        |> Array.filter (fun (pos, v) -> condition pos)
        |> Array.map (fun (pos, v) -> v)
  
    let filterData (filter : option<Box3f>) (positions : V3f[]) (values : float[]) = 
        let filter_ = filter |> (fun elem -> defaultArg elem Box3f.Invalid)
        let isInsideBox (pos : V3f) = filter_.Contains(pos)    
        let filteredValues = filterValue isInsideBox positions values
        filteredValues

    let update (frames : Frame[]) (m : Model) (msg : Message) =

        let positions = frames.[0].positions
        //let temp = frames.[0].values.energies
        //let range = temp.GetBoundingRange()
        //let minValue = range.Min
        //let maxValue = range.Max
        //let normalized = 
        //    temp 
        //    |> Array.map (fun elem -> (elem - minValue)/(maxValue - minValue))

        //let norm = normalized

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
            | SetRenderValue v -> 
                let renderValues = 
                    match v with
                    | RenderValue.Energy ->  frames.[0].values.energies
                    | RenderValue.CubicRoot -> frames.[0].values.cubicRoots
                    | RenderValue.Strain -> frames.[0].values.strains
                    | RenderValue.AlphaJutzi -> frames.[0].values.alphaJutzis
                    | RenderValue.Pressure -> frames.[0].values.pressures
                let range = renderValues.GetBoundingRange()
                let minValue = range.Min
                let maxValue = range.Max
                let newDataRange = {
                    min = minValue
                    max = maxValue
                    }
                let filteredData = filterData m.filter positions renderValues

                {m with 
                    renderValue = v
                    values = renderValues
                    dataRange = newDataRange
                    initDataRange = newDataRange
                    data = Array.toList filteredData }
            | SetTransferFunc t -> 
                match t with    
                | None -> m
                | Some(s) -> {m with currentMap = s} 
            | SetRange (d, r, v) -> 
                let range = m.domainRange
                let newDomainRange = 
                    match d with
                    | X ->
                        {range with
                            x =  
                            match r with 
                            | Min -> {range.x with min = v}
                            | Max -> {range.x with max = v}
                        }
                    | Y ->
                        {range with
                            y =  
                            match r with 
                            | Min -> {range.y with min = v}
                            | Max -> {range.y with max = v}
                        }
                    | Z ->
                        {range with
                            z =  
                            match r with 
                            | Min -> {range.z with min = v}
                            | Max -> {range.z with max = v}
                        }
                {m with domainRange = newDomainRange}
            | SetDataRange (r, v) ->
                let dRange = m.dataRange
                let newDataRange = 
                    match r with
                    | Min -> {dRange with min = v}
                    | Max -> {dRange with max = v}
                {m with dataRange = newDataRange}
            | SetClippingPlane (d, v) ->
                let clipPlane = m.clippingPlane
                let newClipPlane = 
                    match d with
                    | X -> {clipPlane with x = v}
                    | Y -> {clipPlane with y = v}
                    | Z -> {clipPlane with z = v}
                {m with clippingPlane = newClipPlane}
            | SetFilter i ->
                let newFilter =
                    match i with
                    | 1 -> Some(Box3f(V3f(-3.0, -3.0, -3.0), V3f(3.0, 30.0, 3.0)))
                    | 2 -> Some(Box3f(V3f(-4.0, -4.0, -4.0), V3f(4.0, 30.0, 4.0)))
                    | 3 -> Some(Box3f(V3f(-5.0, -5.0, -5.0), V3f(5.0, 30.0, 5.0)))
                    | _ -> None

                let filteredData = filterData newFilter positions m.values

                {m with 
                    filter = newFilter
                    data = Array.toList filteredData }
            | ResetFilter -> {m with 
                                filter = None
                                data = []
                                dataRange = m.initDataRange}

    let renderValues = AMap.ofArray((Enum.GetValues typeof<RenderValue> :?> (RenderValue [])) |> Array.map (fun c -> (c, text (Enum.GetName(typeof<RenderValue>, c)) )))

    let view (runtime : IRuntime) (data : Frame[] * Box3f * int) (m : AdaptiveModel) =

        let frustum = 
            Frustum.perspective 60.0 0.1 100.0 1.0 
                |> AVal.constant

        let heraSg = 
            data
            |> Hera.createAnimatedSg m.frame m.pointSize m.renderValue m.currentMap m.domainRange m.clippingPlane m.filter m.dataRange
            |> Sg.noEvents

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

        let inputView dispText inValue updateFunc minV maxV = 
            span [] [
            span [style "padding: 3px"] [text dispText]
            simplenumeric {
                attributes [style "width: 38%; padding: 5px"]
                value inValue
                update updateFunc
                step 1.0
                largeStep 5.0
                min minV
                max maxV
            }]

        let rangeView (dim : Dim) (inRange : aval<Range>) minV maxV = 
            div [] [ 
                inputView (string dim + ":") (inRange |> AVal.map (fun r -> r.min)) (fun v -> SetRange (dim, Min, v)) minV 0.0
                inputView (string dim + ":") (inRange |> AVal.map (fun r -> r.max)) (fun v -> SetRange (dim, Max, v)) 0.0 maxV
                br []
            ]
        require Html.semui (
            body [] [
                FreeFlyController.controlledControl m.cameraState CameraMessage frustum (AttributeMap.ofList att) sg
                require dependencies ( // here we execute Histogram.js.... all values, functions defined in Histogram.js are further down accessible...

                    div[] [
                        div [style "position: fixed; left: 20px; top: 20px; width: 220px"] [
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
                              
                            div [style "width: 90%"] [  
                                Html.SemUi.accordion "Clipping Box" "plus" false [
                                    Incremental.div (AttributeMap.ofAMap (amap {
                                        yield clazz "item"
                                        yield style "width: 100%"
                                    })) (alist {
                                        span [style "padding: 3px; padding-inline-start: 0px; padding-inline-end: 50px"] [text "Min:"]
                                        span [style "padding: 3px; padding-inline-start: 15px; padding-inline-end: 50px"] [text "Max:"]
                                        br []
                                        rangeView X (m.domainRange |> AVal.map (fun r -> r.x)) -25.0 25.0 
                                        rangeView Y (m.domainRange |> AVal.map (fun r -> r.y)) -25.0 30.0 
                                        rangeView Z (m.domainRange |> AVal.map (fun r -> r.z)) -25.0 25.0 
                                        }
                                        )
                                    ]
                            ]

                            br []
                       
                            div [ clazz "item"; style "width: 90%" ] [
                                span [style "padding: 2px"] [text "X: "]
                                slider { min = -16.0; max = 16.0; step = 0.5 } [clazz "ui inverted slider"; style "padding: 4px"] (m.clippingPlane |> AVal.map (fun cp -> cp.x)) (fun v -> SetClippingPlane (X, v))
                                span [style "padding: 2px"] [text "Y: "]
                                slider { min = -16.0; max = 30.0; step = 0.5 } [clazz "ui inverted slider"; style "padding: 4px"] (m.clippingPlane |> AVal.map (fun cp -> cp.y)) (fun v -> SetClippingPlane (Y, v))
                                span [style "padding: 2px"] [text "Z: "]
                                slider { min = -16.0; max = 16.0; step = 0.5 } [clazz "ui inverted slider"; style "padding: 4px"] (m.clippingPlane |> AVal.map (fun cp -> cp.z)) (fun v -> SetClippingPlane (Z, v))

                            ]

                            br []


                            Incremental.div (AttributeMap.ofAMap (amap {
                                yield style "width: 95%"
                            })) (alist {
                                let! initRange = m.initDataRange
                            
                                let steps = 
                                    let range = Math.Abs(initRange.max - initRange.min)
                                    if range <= 0.01 then
                                        0.001, 0.005
                                    else if range <= 0.1 then
                                        0.01, 0.05
                                    else if range <= 1.0 then
                                        0.1, 1.0
                                    else if (range <= 1000.0) then
                                        1.0, 5.0
                                    else if (range <= 10000.0) then
                                        10.0, 50.0
                                    else if (range <= 100000.0) then
                                        100.0, 500.0
                                    else if (range <= 1000000.0) then
                                        1000.0, 5000.0
                                    else 
                                        10000.0, 50000.0

                                yield span [style "padding: 4px"] [text "Min: "]
                                yield simplenumeric {
                                        attributes [style "width: 80%; padding: 2px"]
                                        value (m.dataRange |> AVal.map (fun r -> r.min))
                                        update (fun v -> SetDataRange (Min, v))
                                        step (fst steps)
                                        largeStep (snd steps)
                                        min initRange.min
                                        max initRange.max
                                        }

                                yield span [style "padding: 3px"] [text "Max: "]
                                yield simplenumeric {
                                        attributes [style "width: 80%; padding: 2px"]
                                        value (m.dataRange |> AVal.map (fun r -> r.max))
                                        update (fun v -> SetDataRange (Max, v))
                                        step (fst steps)
                                        largeStep (snd steps)
                                        min initRange.min
                                        max initRange.max
                                        }    
                               }
                            )

                            br []

                            div [] [
                                button [onClick (fun _ -> SetFilter 1); style "margin-inline-start: 2px; margin-inline-end: 2px"] [text "Probe 1"]
                                button [onClick (fun _ -> SetFilter 2); style "margin-inline-start: 2px; margin-inline-end: 2px"] [text "Probe 2"]
                                button [onClick (fun _ -> SetFilter 3); style "margin-inline-start: 2px; margin-inline-end: 2px"] [text "Probe 3"]
              
                            ]

                            button [onClick (fun _ -> ResetFilter); style "margin: 2px; margin-top: 4px"] [text "Reset"]

                        ]
                        ]   

                        Incremental.div (AttributeMap.ofAMap (amap {
                            yield style "position: fixed; top: 20px; right: 20px"
                        })) (alist {
                            let! src = m.currentMap

                            let elems = 
                                src.Split([|'\t'; '\\'|], StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun s -> s.Trim())

                            let currImage = elems.[elems.Length - 1]

                            yield img [
                                attribute "src" currImage
                                style "width: 170px; height: 20px"
                            ]            
                        
                            yield hr [
                                style "width: 168px; height: 0px; background-color: #ffffff; margin-block-start: 3px; margin-block-end: 0px; margin-inline-start: 0px"
                            ]

                            yield span [style "position: relative; font-size: 0.8em; right: 10px; color: #ffffff"] [Incremental.text (m.dataRange |> AVal.map (fun r -> r.min.ToString()))] 
                            yield span [style "position: inherit; font-size: 0.8em; right: 10px; color: #ffffff"] [Incremental.text (m.dataRange |> AVal.map (fun r -> r.max.ToString()))]
                            }
                        )

                        onBoot' ["data", dataChannel] updateChart ( // when div [histogram etc] is constructed updateChart is called.
                            div [clazz "histogram"; style "position: fixed; bottom: 20px; right: 20px; width:350px; height: 200px; background-color: #ffffff; border: 3px solid #ffffff; z-index: 1000"] [                       
                            ]
                        )
                    ]
                )  
            ]
            )

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
        // load data
        let data = DataLoader.loadData runtime 0 7
        let frames = data |> (fun (elem, _, _) ->  elem)
        {
            initial = initial frames // store data Hera frame as array to model....
            update = update frames
            view = view runtime data
            threads = threads
            unpersist = Unpersist.instance
        }