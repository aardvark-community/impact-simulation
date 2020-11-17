namespace AardVolume

open System
open System.IO
open Aardvark.Base
open Aardvark.SceneGraph
open Aardvark.SceneGraph.IO
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Base.Rendering
open AardVolume.Model
open Hera

open FSharp.Data.Adaptive

type EmbeddedRessource = EmbeddedRessource // THIS IS NECESSARY

type Message =
    | ToggleModel
    | CameraMessage of FreeFlyController.Message
    | StepTime
    | TogglePointSize
    | TogglePointDiscarded
    | SetPointSize of float
    | ChangeAnimation
    | AnimateAllFrames
    | SetRenderValue of RenderValue
    | SetColorValue of ColorPicker.Action
    | SetTransferFunc of option<string>
    | SetDomainRange of Dim * Value * float
    | SetDataRange of Value * float
    | SetClippingPlane of Dim * float
    | SetFilter of int
    | ResetFilter 
    | Brushed of Range
    | BrushedParallCoords of RenderValue * Range * bool
    
module App =    

    let tfsDir = @"..\..\..\src\ImpactVisualization\resources\transfer"

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

    let roundXdecimal (number : float) x = 
        let scaled = number * Math.Pow(10.0, x)
        let rounded = Math.Round(scaled)
        let rescale = rounded / Math.Pow(10.0, x)
        rescale

    let initial (frames : Frame[]) = 
        let initValues = frames.[0].values.energies
        let rangeEnergy = initValues.GetBoundingRange()
        let minValue = rangeEnergy.Min
        let maxValue = rangeEnergy.Max

        let rangeCubicRoot = frames.[0].values.cubicRoots.GetBoundingRange()
        let rangeStrain = frames.[0].values.strains.GetBoundingRange()
        let rangeAlphaJutzi = frames.[0].values.alphaJutzis.GetBoundingRange()
        let rangePressure = frames.[0].values.pressures.GetBoundingRange()

        let filters = {
            filterEnergy = {
                min = rangeEnergy.Min
                max = rangeEnergy.Max
            }
            filterCubicRoot = {
                min = rangeCubicRoot.Min
                max = rangeCubicRoot.Max
            }
            filterStrain = {
                min = rangeStrain.Min
                max = rangeStrain.Max
            }
            filterAlphaJutzi = {
                min = rangeAlphaJutzi.Min
                max = rangeAlphaJutzi.Max
            }
            filterPressure = {
                min = rangePressure.Min
                max = rangePressure.Max
            }
        }

        let model = 
            { 
            cameraState = FreeFlyController.initial; 
            frame = 0;
            pointSize = 8.0
            playAnimation = true
            animateAllFrames = false
            discardPoints = false
            transition = true
            renderValue = RenderValue.Energy
            colorValue = { c = C4b.Gray}
            colorMaps = listWithValues
            currentMap = @"..\..\..\src\ImpactVisualization\resources\transfer\transfer.jpg"
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
            initFilters = filters
            currFilters = filters
            values = initValues
            data = []
            filter = None
            filtered = []
            filteredAllFrames = Array.empty
            dataPath = "data.csv"
            boxColor = C4b.White
            }
        model

    let sw = System.Diagnostics.Stopwatch.StartNew()

    let filteri condition values = 
        values
        |> Array.mapi (fun i v -> (v, i))
        |> Array.filter (fun (v, i) -> condition v)
        |> Array.map (fun (v, i) -> i)

    let filterValue condition (pos : V3f[]) value = 
        Array.zip pos value
        |> Array.filter (fun (pos, v) -> condition pos)
        |> Array.map (fun (pos, v) -> v)

    // --------------------- Longer, but more sophisticated solution -----------------------

    let zip5 a (b : _ []) (c : _ []) (d : _ []) (e : _ [])=
        Array.init (Array.length a) (fun i -> a.[i], b.[i], c.[i], d.[i], e.[i])

    let unzip5 arr =
        let a = Array.zeroCreate (Array.length arr)
        let b = Array.zeroCreate (Array.length arr)
        let c = Array.zeroCreate (Array.length arr)
        let d = Array.zeroCreate (Array.length arr)
        let e = Array.zeroCreate (Array.length arr)
        arr
        |> Array.iteri (fun i (x, y, z, w, v) ->
            a.[i] <- x
            b.[i] <- y
            c.[i] <- z
            d.[i] <- w
            e.[i] <- v)
        a, b, c, d, e

    let filterAllValues condition (pos : V3f[]) (values : Values) = 
        let allValuesArray = zip5 values.energies values.cubicRoots values.strains values.alphaJutzis values.pressures |> Seq.distinct |> Seq.toArray
        let temp = Array.zip pos allValuesArray
        let filtered = 
            temp
            |> Array.filter (fun (pos, v) -> condition pos)
            |> Array.map (fun (pos, v) -> v)

        filtered

    let unzipValues filtered = 
        let e, c, s, a, p = unzip5 filtered
        let values : Values = { energies = e
                                cubicRoots = c
                                strains = s
                                alphaJutzis = a
                                pressures = p
                                }
        values

    // --------------------------------------------------------------------------------
  
    let filterData (filter : option<Box3f>) (positions : V3f[]) (values : float[]) = 
        let filter_ = filter |> (fun elem -> defaultArg elem Box3f.Invalid)
        let isInsideBox (pos : V3f) = filter_.Contains(pos)    
        let filteredValues = filterValue isInsideBox positions values
        filteredValues

    let filterValueFrames condition (pos : V3f[]) (values : Values) = 
        let filteredEnergies = filterValue condition pos values.energies
        let filteredCubicRoots = filterValue condition pos values.cubicRoots
        let filteredStrains = filterValue condition pos values.strains
        let filteredAlphaJutzis = filterValue condition pos values.alphaJutzis
        let filteredPressures = filterValue condition pos values.pressures
        let newValues : Values = { energies = filteredEnergies
                                   cubicRoots = filteredCubicRoots  
                                   strains = filteredStrains
                                   alphaJutzis = filteredAlphaJutzis
                                   pressures = filteredPressures }
        newValues

    let filterDataFrames (filter : Box3f) (positions : V3f[]) (frame : Frame) = 
        let isInsideBox (pos : V3f) = filter.Contains(pos)    
        let filteredValues = filterAllValues isInsideBox positions frame.values
        filteredValues

    let filterDataForAllFrames (frames : Frame[]) (filter : option<Box3f>) (renderVal : RenderValue) = 
        let temp = 
            frames
            |> Array.mapi (fun i frame -> 
                            let positions = frames.[i].positions
                            let renderValues = 
                                match renderVal with
                                | RenderValue.Energy ->  frames.[i].values.energies
                                | RenderValue.CubicRoot -> frames.[i].values.cubicRoots
                                | RenderValue.Strain -> frames.[i].values.strains
                                | RenderValue.AlphaJutzi -> frames.[i].values.alphaJutzis
                                | RenderValue.Pressure -> frames.[i].values.pressures 
                                | _ -> frames.[i].values.energies
                            let currFilteredData = filterData filter positions renderValues
                            currFilteredData
            )
        temp

    let update (frames : Frame[]) (m : Model) (msg : Message) =

        let numOfFrames = frames.Length
        let positions = frames.[m.frame].positions
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
            | TogglePointDiscarded -> {m with discardPoints = not m.discardPoints}
            | ChangeAnimation -> { m with playAnimation = not m.playAnimation}
            | AnimateAllFrames -> 
                let filteredDataAllFrames = filterDataForAllFrames frames m.filter m.renderValue

                {m with 
                    animateAllFrames = not m.animateAllFrames
                    transition = not m.transition
                    filteredAllFrames = filteredDataAllFrames}
            | StepTime -> 
                let frameId =  (sw.Elapsed.TotalSeconds / 0.5) |> int
                let currFrame = frameId % numOfFrames
                // update filtered data using frameId and filter
                let isCurrentFilterSet = 
                    match m.filter with 
                    | None -> false
                    | Some(_) -> true
                    

                let currData = 
                    if isCurrentFilterSet && m.animateAllFrames then  
                        let filteredData = m.filteredAllFrames.[currFrame]
                        filteredData
                    else 
                        List.toArray m.data

                //let currData = 
                //    if isCurrentFilterSet then  
                //        let filteredData = filterData m.filter positions m.values
                //        filteredData
                //    else 
                //        let temp : float[] = Array.empty
                //        temp

                if m.playAnimation then sw.Start() else sw.Stop()
                { m with 
                    frame = currFrame 
                    data = Array.toList currData}
            | CameraMessage msg ->
                { m with cameraState = FreeFlyController.update m.cameraState msg }
            | SetRenderValue v -> 
                let renderValues = 
                    match v with
                    | RenderValue.Energy ->  frames.[m.frame].values.energies
                    | RenderValue.CubicRoot -> frames.[m.frame].values.cubicRoots
                    | RenderValue.Strain -> frames.[m.frame].values.strains
                    | RenderValue.AlphaJutzi -> frames.[m.frame].values.alphaJutzis
                    | RenderValue.Pressure -> frames.[m.frame].values.pressures
                    | _ -> frames.[m.frame].values.energies
                let range = renderValues.GetBoundingRange()
                let minValue = range.Min
                let maxValue = range.Max
                let newDataRange = {
                    min = minValue
                    max = maxValue
                    }
                let filteredData = filterData m.filter positions renderValues

                let filteredDataAllFrames = 
                    if m.animateAllFrames then
                        let filtered = filterDataForAllFrames frames m.filter v
                        filtered
                    else
                        let temp : float[] [] = Array.empty
                        temp

                {m with 
                    renderValue = v
                    values = renderValues
                    dataRange = newDataRange
                    initDataRange = newDataRange
                    data = Array.toList filteredData
                    filteredAllFrames = filteredDataAllFrames}
            | SetColorValue a -> 
                let c = ColorPicker.update m.colorValue a
                {m with colorValue = c}
            | SetTransferFunc t -> 
                match t with    
                | None -> m
                | Some(s) -> {m with currentMap = s} 
            | SetDomainRange (d, r, v) -> 
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

                let newDataPath =
                    match i with
                    | 1 -> "dataFilter1.csv"
                    | 2 -> "dataFilter2.csv"
                    | 3 -> "dataFilter3.csv"
                    | _ -> "data.csv"

                let filteredDataAllFrames = 
                    if m.animateAllFrames then
                        let filtered = filterDataForAllFrames frames newFilter m.renderValue
                        filtered
                    else
                        let temp : float[] [] = Array.empty
                        temp
                        
                let filteredData = filterData newFilter positions m.values

                {m with 
                    filter = newFilter
                    data = Array.toList filteredData
                    filteredAllFrames = filteredDataAllFrames
                    dataPath = newDataPath}
            | ResetFilter -> {m with 
                                filter = None
                                data = []
                                dataRange = m.initDataRange
                                dataPath = "data.csv"
                                currFilters = m.initFilters}
            | Brushed r -> { m with dataRange = r}
            | BrushedParallCoords (v, r, b) ->
                let filters = m.currFilters
                let initFilters = m.initFilters
                let newFilters = 
                    match v with
                    | RenderValue.Energy ->  
                        if b then 
                            {filters with
                                filterEnergy =
                                {
                                    min = initFilters.filterEnergy.min
                                    max = initFilters.filterEnergy.max
                                }
                            }
                        else
                            {filters with
                                filterEnergy =
                                {
                                    min = r.min
                                    max = r.max
                                }
                            }
                    | RenderValue.CubicRoot -> 
                        if b then 
                            {filters with
                                filterCubicRoot =
                                {
                                    min = initFilters.filterCubicRoot.min
                                    max = initFilters.filterCubicRoot.max
                                }
                            }
                        else
                            {filters with
                                filterCubicRoot =
                                {
                                    min = r.min
                                    max = r.max
                                }
                            }
                    | RenderValue.Strain -> 
                        if b then 
                            {filters with
                                filterStrain =
                                {
                                    min = initFilters.filterStrain.min
                                    max = initFilters.filterStrain.max
                                }
                            }
                        else
                            {filters with
                                filterStrain =
                                {
                                    min = r.min
                                    max = r.max
                                }
                            }
                    | RenderValue.AlphaJutzi ->
                        if b then 
                            {filters with
                                filterAlphaJutzi =
                                {
                                    min = initFilters.filterAlphaJutzi.min
                                    max = initFilters.filterAlphaJutzi.max
                                }
                            }
                        else
                            {filters with
                                filterAlphaJutzi =
                                {
                                    min = r.min
                                    max = r.max
                                }
                            }
                    | RenderValue.Pressure -> 
                        if b then 
                            {filters with
                                filterPressure =
                                {
                                    min = initFilters.filterPressure.min
                                    max = initFilters.filterPressure.max
                                }
                            }
                        else
                            {filters with
                                filterPressure =
                                {
                                    min = r.min
                                    max = r.max
                                }
                            }
                    | _ -> filters
                {m with currFilters = newFilters}
                | _ -> m

    let renderValues = AMap.ofArray((Enum.GetValues typeof<RenderValue> :?> (RenderValue [])) |> Array.map (fun c -> (c, text (Enum.GetName(typeof<RenderValue>, c)) )))

    let view (runtime : IRuntime) (data : Frame[] * Box3f * int) (m : AdaptiveModel) =

        let frames = data |> (fun (elem, _, _) ->  elem)

        let shuffleR (r : Random) xs = xs |> Seq.sortBy (fun _ -> r.Next())
        
        let encodeToCSVData (data : Frame[]) =
            let builder = System.Text.StringBuilder()
            let initValues = data.[0].values
            let positions = data.[0].positions

            builder.AppendLine("energy,cubicRoot,strain,alphaJutzi,pressure") |> ignore

            let filter = Box3f(V3f(-5.0, -5.0, -5.0), V3f(5.0, 30.0, 5.0))

            let filteredValues = filterDataFrames filter positions data.[0]

            let randomlyPickedData = filteredValues |> shuffleR (Random ()) |> Seq.take 3800 |> Seq.toArray
            let values = unzipValues randomlyPickedData
            let valuesLength = randomlyPickedData.Length

            for i in 0 .. valuesLength - 1 do
                builder.AppendLine(
                    sprintf "%f,%f,%f,%f,%f" values.energies.[i] values.cubicRoots.[i] values.strains.[i] values.alphaJutzis.[i] values.pressures.[i]
                ) |> ignore

            builder.ToString()

        //______UNCOMMENT FOLLOWING LINES TO STORE THE .CSV DATA__________    
            
        //let csvData = encodeToCSVData frames
        //let path = @"..\..\..\src\ImpactVisualization\resources\dataFilter3.csv"
        //File.writeAllText path csvData

        let contrClippingPlane = Plane3d.Invalid |> (AVal.constant)
        let sphereProbe = Sphere3d.Invalid |> AVal.constant
        
        let frustum = 
            Frustum.perspective 60.0 0.1 100.0 1.0 
                |> AVal.constant

        let heraSg = 
            data
            |> Hera.createAnimatedSg m.frame m.pointSize m.discardPoints m.renderValue m.currentMap 
                m.domainRange m.clippingPlane contrClippingPlane m.filter m.currFilters m.dataRange m.colorValue.c 
                sphereProbe
                m.cameraState.view
                runtime
            |> Sg.noEvents

        let currentBox = 
            m.filter |> AVal.map (fun b ->
                match b with 
                    | Some box -> box.BoundingBox3d
                    | None -> Box3d.Infinite
                )

        let boxSg = 
            Sg.box m.boxColor currentBox
            |> Sg.noEvents
            |> Sg.fillMode (FillMode.Line |> AVal.constant)


        let sg = 
            Sg.ofSeq [heraSg; boxSg]
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.simpleLighting
                do! DefaultSurfaces.constantColor C4f.White
            }


        let att =
            [
                style "position: fixed; left: 0; top: 0; width: 100%; height: 100%;"// background-color:white"
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
                let! allFramesAnimation = m.animateAllFrames


                if animation = true then
                    yield button [onClick (fun _ -> ChangeAnimation)] [text "Pause"]
                else 
                    yield button [onClick (fun _ -> ChangeAnimation)] [text "Play"]

                if allFramesAnimation = true then
                    yield button [onClick (fun _ -> AnimateAllFrames); style "margin-inline-start: 5px"] [text "One Frame"]
                else 
                    yield button [onClick (fun _ -> AnimateAllFrames); style "margin-inline-start: 5px"] [text "All Frames"]

            }

        let colorMaps = AMap.map (fun k v -> text v) m.colorMaps

        let dependencies = 
            [
                { kind = Script; name = "d3"; url = "http://d3js.org/d3.v4.min.js" }
                { kind = Stylesheet; name = "histogramStyle"; url = "Histogram.css" }
                { kind = Script; name = "histogramScript"; url = "Histogram.js" }
                { kind = Stylesheet; name = "parallCoordStyle"; url = "ParallCoords.css" }
                { kind = Script; name = "parallCoordScript"; url = "ParallCoords.js" }

            ]

       // let a = m.data | AVal.map (fun data -> encodeToJSONData)
       // let multidimjsondescription = a.Channel
        
       // let da = AVal.map2 (fun values animatie -> sprintf "{values = %s; animate = %A}" values animate) m.data m.animateAllFrames
       // let guh = da.Channel

        let dataChannel = m.data.Channel
        let transitionChannel = m.transition.Channel
        let updateChart =
            "initHisto(__ID__); data.onmessage = function (values) { refresh(values);  }; transition.onmessage = function (v) { shouldUseTransition = v; }"  // subscribe to m.data

        let pathChannel = m.dataPath.Channel
        let updateParallCoords = 
            "initParallCoords(__ID__); dataPath.onmessage = function (path) { if (path != null) refreshPar(path); };"


        let onBrushed = 
            onEvent ("brushing") [] (( fun args ->
                match args with
                | [x;y] ->
                    let range = {
                        min = float x
                        max = float y
                    }
                    Brushed range
                | _ -> failwith ""
            ))

        let boolean (s : string) =
            match s with
            | "true" -> true
            | "false" -> false
            | _-> failwith ""

        let onParallCoordsBrushed = 
            onEvent ("parallcoords") [] (( fun args ->
                match args with
                | [dim; low; high; res] ->
                    let renderValue = 
                        match dim with
                        | "\"energy\"" -> RenderValue.Energy
                        | "\"cubicRoot\"" -> RenderValue.CubicRoot
                        | "\"strain\"" -> RenderValue.Strain
                        | "\"alphaJutzi\"" -> RenderValue.AlphaJutzi
                        | "\"pressure\"" -> RenderValue.Pressure
                        | _ -> failwith ""

                    let lower = float low
                    let upper = float high
                    let reset = boolean res
                    let range = {
                        min = lower
                        max = upper
                    }
                    BrushedParallCoords (renderValue, range, reset)
                | _ -> failwith ""
            ))
 
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
                inputView (string dim + ":") (inRange |> AVal.map (fun r -> r.min)) (fun v -> SetDomainRange (dim, Min, v)) minV 0.0
                inputView (string dim + ":") (inRange |> AVal.map (fun r -> r.max)) (fun v -> SetDomainRange (dim, Max, v)) 0.0 maxV
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

                            div [style "width: 90%"] [ 
                                simplecheckbox { 
                                    attributes [clazz "ui inverted checkbox"]
                                    state m.discardPoints
                                    toggle TogglePointDiscarded
                                    content [ text "Discard Out of Range?"]  
                                }
                            ]

                            br []

                            div [clazz "colorRange"; style "width: 90%"] [ 
                                span [style "padding: 2px; padding-inline-end: 20px"] [text "Out of Range Color:"]
                                ColorPicker.view m.colorValue |> UI.map SetColorValue
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
                                        value (m.dataRange |> AVal.map (fun r -> (roundXdecimal r.min 2.0)))
                                        update (fun v -> SetDataRange (Min, (roundXdecimal v 2.0)))
                                        step (fst steps)
                                        largeStep (snd steps)
                                        min (roundXdecimal initRange.min 2.0)
                                        max (roundXdecimal initRange.max 2.0)
                                        }

                                yield span [style "padding: 3px"] [text "Max: "]
                                yield simplenumeric {
                                        attributes [style "width: 80%; padding: 2px"]
                                        value (m.dataRange |> AVal.map (fun r -> (roundXdecimal r.max 2.0)))
                                        update (fun v -> SetDataRange (Max, (roundXdecimal v 2.0)))
                                        step (fst steps)
                                        largeStep (snd steps)
                                        min (roundXdecimal initRange.min 2.0)
                                        max (roundXdecimal initRange.max 2.0)
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

                            yield span [style "position: relative; font-size: 0.8em; right: 10px; color: #ffffff"] [Incremental.text (m.dataRange |> AVal.map (fun r -> (roundXdecimal r.min 2.0).ToString()))] 
                            yield span [style "position: inherit; font-size: 0.8em; right: 10px; color: #ffffff"] [Incremental.text (m.dataRange |> AVal.map (fun r -> (roundXdecimal r.max 2.0).ToString()))]
                            }
                        )
                        
                        let histogram = 
                                onBoot' [("data", dataChannel); ("transition", transitionChannel)] updateChart ( // when div [histogram etc] is constructed updateChart is called.
                                            div [onBrushed; clazz "histogram"; style "position: fixed; bottom: 20px; right: 20px; width:400px; height: 230px; z-index: 1000"] []          
                                )
                            

                        let parallCoords = 
                                    onBoot' [("dataPath", pathChannel)] updateParallCoords (
                                            div [onParallCoordsBrushed; clazz "parallCoords"; style "position: fixed; bottom: 20px; right: 20px; width:400px; height: 230px; z-index: 1000"] []          
                                    )

                        Html.SemUi.tabbed [clazz "temp"; style "position: fixed; bottom: 220px; right: 20px" ] [
                            ("Histogram", histogram)
                            ("ParallCoords", parallCoords)
                        ] "Histogram"
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