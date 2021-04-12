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
open HeraSg

open Hera
open FSharp.Data.Adaptive
open Aardvark.Data.Points
open Aardvark.Geometry.Points
open Uncodium.SimpleStore

type EmbeddedRessource = EmbeddedRessource // THIS IS NECESSARY

type Message =
    | ToggleModel
    | CameraMessage of FreeFlyController.Message
    | StepTime
    | TogglePointSize
    | TogglePointDiscarded
    | NormalizeData
    | EnableShading
    | ReconstructNormal
    | ReconstructDepth
    | DisplayLowerOutliers
    | DisplayHigherOutliers
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


    //TODO: definitely not the best solution
    let roundXdecimal (number : float) x = 
        let scaled = number * Math.Pow(10.0, x)
        let rounded = Math.Round(scaled)
        let rescale = rounded / Math.Pow(10.0, x)
        rescale

    let roundDecimal (number : float) = 
        let absNumber = abs number
        let decimalPlaces = 
            if absNumber < 1.0 then 9.0
            else if absNumber <= 100.0 then 2.0 
            else if absNumber <= 10000.0 then 1.0
            else 0.0
        roundXdecimal number decimalPlaces

    let initial (frames : Frame[]) = 
        let initValues = frames.[0].energies
        let rangeEnergy = initValues.GetBoundingRange()
        let minValue = rangeEnergy.Min
        let maxValue = rangeEnergy.Max
        
        let rangeCubicRoot = frames.[0].cubicRoots.GetBoundingRange()
        let rangeStrain = frames.[0].strains.GetBoundingRange()
        let rangeAlphaJutzi = frames.[0].alphaJutzis.GetBoundingRange()
        let rangePressure = frames.[0].pressures.GetBoundingRange()

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
            currHeraBBox = Box3d.Infinite
            pointSize = 8.0
            playAnimation = false
            animateAllFrames = false
            discardPoints = false
            transition = true
            normalizeData = false
            enableShading = false
            reconstructNormal = false
            reconstructDepth = false
            lowerOutliers = false
            higherOutliers = false
            outliersRange = {
                min = -infinity
                max = infinity
            }
            renderValue = RenderValue.Energy
            attributeText = ""
            colorValue = { c = C4b.Gray}
            colorMaps = listWithValues
            currentMap = @"..\..\..\src\ImpactVisualization\resources\transfer\transfer.jpg"
            domainRange = {
                x = {min = -16.0; max = 16.0}; 
                y = {min = -16.0; max = 30.0}; 
                z = {min = -16.0; max = 16.0}
                }            
            clippingPlane = {
                x = infinity
                y = infinity
                z = infinity
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
            values = VersionedArray.ofArray initValues
            data = VersionedArray.ofArray [||]
            boxPlotData = [| [|2.0; 4.0; 1.0; 3.5; 7.0; 10.0; 11.0; 6.0; 6.5; 2.7; 19.0; 0.0|] |]
            boxPlotAttribute = ""
            currFilter = None
            boxFilter = None
            sphereFilter = None 
            filtered = []
            filteredAllFrames = Array.empty
            dataPath = "data.csv"
            boxColor = C4b.White
            }
        model

    let sw = System.Diagnostics.Stopwatch.StartNew()

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

    let unzipValues filtered = 
        let e, c, s, a, p = unzip5 filtered
        let values : Values = { energies = e
                                cubicRoots = c
                                strains = s
                                alphaJutzis = a
                                pressures = p
                                }
        values

    let sphereContainsPoint (sphere : Sphere3d) (point : V3d) =
        sphere.Center.Distance(point) <= sphere.Radius

    let sphereContainsBox (sphere : Sphere3d) (box : Box3d) =
        box.Corners |> Seq.forall (fun corner -> sphereContainsPoint sphere corner)

    let createBoxQuery (box : Box3d) (frame : Frame) =
        let chunk = 
            Queries.QueryPointsCustom frame.pointSet.Root.Value
                (fun n -> box.Contains n.BoundingBoxExactGlobal) 
                (fun n -> not (box.Intersects n.BoundingBoxExactGlobal)) 
                (fun p -> box.Contains p)
                Hera.Defs.all
                None
            |> Seq.toArray
        chunk

    let pointIsNotDiscardedByAPlane (point : V3d) (discardProps : DiscardProperties) = 
        let plane = discardProps.plane
        let controllerPlane = discardProps.controllerPlane
        let pointInsideAxisAlignedPlanes = point.X <= plane.X && point.Y <= plane.Y && point.Z <= plane.Z
        let pointInsideControllerPlane = if controllerPlane.IsInvalid then true else (Vec.Dot(controllerPlane.Normal, point) - controllerPlane.Distance >= 0.0)
        pointInsideAxisAlignedPlanes && pointInsideControllerPlane

    //TODO: Must be improved to really check intersection between Sphere and Box
    let createSphereQuery (sphere : Sphere3d) (frame : Frame) (discardProps : DiscardProperties) =
        let chunk = 
            let sphereBox = sphere.BoundingBox3d
            let squaredR = sphere.Radius * sphere.Radius
            Queries.QueryPointsCustom frame.pointSet.Root.Value
                (fun n -> sphereBox.Contains n.BoundingBoxExactGlobal) 
                (fun n -> not (sphereBox.Intersects n.BoundingBoxExactGlobal)) 
                (fun p -> (sphere.Center.DistanceSquared(p) <= squaredR) && (pointIsNotDiscardedByAPlane p discardProps))
                Hera.Defs.all
                None
            |> Seq.toArray
        chunk


    //TODO: Find the comutation time of NSmallestIndex -> is it O(n)? Is there a faster solution?
    let findQuartiles1And3 (listOfPoints : float[]) = 
        let N = listOfPoints.Length
        let listIsEven = if N % 2 = 0 then true else false
        let halfList = if listIsEven then N / 2 else (N - 1) / 2
        let halfListIsEven = if halfList % 2 = 0 then true else false
        if halfListIsEven then 
            let orderedIdxQ1_1 = halfList / 2
            let orderedIdxQ1_2 = orderedIdxQ1_1 + 1
            let idxQ1_1 = listOfPoints.NSmallestIndex(orderedIdxQ1_1 - 1)
            let idxQ1_2 = listOfPoints.NSmallestIndex(orderedIdxQ1_2 - 1)
            let quartile1 = (listOfPoints.[idxQ1_1] + listOfPoints.[idxQ1_2]) / 2.0

            let orderedIdxQ3_1 = if listIsEven then (halfList * 3/2) else (halfList * 3/2) + 1
            let orderedIdxQ3_2 = orderedIdxQ3_1 + 1
            let idxQ3_1 = listOfPoints.NSmallestIndex(orderedIdxQ3_1 - 1)
            let idxQ3_2 = listOfPoints.NSmallestIndex(orderedIdxQ3_2 - 1)
            let quartile3 = (listOfPoints.[idxQ3_1] + listOfPoints.[idxQ3_2]) / 2.0

            quartile1, quartile3
        else 
            let orderedIdxQ1 = (halfList + 1) / 2
            let idxQ1 = listOfPoints.NSmallestIndex(orderedIdxQ1 - 1)
            let quartile1 = listOfPoints.[idxQ1]

            let orderedIdxQ3 = if listIsEven then halfList + (halfList + 1)/2 else (halfList + 1) * 3/2
            let idxQ3 = listOfPoints.NSmallestIndex(orderedIdxQ3 - 1)
            let quartile3 = listOfPoints.[idxQ3]

            quartile1, quartile3

    let findOutliers (listOfPoints : float[]) = 
        let quartieles1And3 = findQuartiles1And3 listOfPoints
        let quartile1 = fst quartieles1And3
        let quartile3 = snd quartieles1And3
        let IQR = quartile3 - quartile1
        let lowerOutliersBoundary = quartile1 - 1.5 * IQR
        let higherOutliersBoundary = quartile3 + 1.5 * IQR
        lowerOutliersBoundary, higherOutliersBoundary

    let renderValueToString (renderValue : RenderValue) = 
        match renderValue with 
            | RenderValue.Energy -> "Energy"
            | RenderValue.CubicRoot -> "Cubic Root"
            | RenderValue.Strain -> "Strain"
            | RenderValue.AlphaJutzi -> "Alpha Jutzi"
            | RenderValue.Pressure -> "Pressure"
            | RenderValue.Mass -> "Mass"
            | RenderValue.Density -> "Density"
            | _ -> "Energy"

    let computeStatistics (filteredPoints : float[]) (renderValue : RenderValue) = 
        let renderVal = renderValueToString renderValue
        let numOfPoints = filteredPoints.Length
        let max = roundDecimal (filteredPoints.Max())
        let min = roundDecimal (filteredPoints.Min())
        let mean = roundDecimal (filteredPoints.Mean())
        let median = if numOfPoints < 1 then nan else roundDecimal (filteredPoints.Median())
        let variance = roundDecimal (filteredPoints.Variance()) 
        let standardDeviation = roundDecimal (filteredPoints.StandardDeviation())
        let statisticsText = 
            renderVal.Capitalized() + " | " + string numOfPoints + " points \n" + 
            "Min: " + string min + "\n" +
            "Max: " + string max + "\n" + 
            "Mean: " + string mean + "\n" + 
            "Median: " + string median + "\n" + 
            "Var.: " + string variance + "\n" + 
            "SD: " + string standardDeviation
        statisticsText

    let computeAllAttributesAndStatistics (allAttributes : HashMap<RenderValue, float[]>) = 
        allAttributes |> HashMap.map (fun renderVal array -> (array, computeStatistics array renderVal))

    let filterDataForOneFrame (chunk : GenericChunk[]) (renderVal : RenderValue) =
        let extract f = chunk |> Array.map f |> Array.concat
        let currFilteredData = 
            match renderVal with
            | RenderValue.Energy -> extract (fun c -> c.Data.[Hera.Defs.InternalEnergies] :?> float32[]) 
            | RenderValue.CubicRoot -> extract (fun c -> c.Data.[Hera.Defs.CubicRootsOfDamage] :?> float32[])
            | RenderValue.Strain -> extract (fun c -> c.Data.[Hera.Defs.LocalStrains] :?> float32[])
            | RenderValue.AlphaJutzi -> extract (fun c -> c.Data.[Hera.Defs.AlphaJutzi] :?> float32[])
            | RenderValue.Pressure -> extract (fun c -> c.Data.[Hera.Defs.Pressures] :?> float32[]) 
            | RenderValue.Mass -> extract (fun c -> c.Data.[Hera.Defs.Masses] :?> float32[]) 
            | RenderValue.Density -> extract (fun c -> c.Data.[Hera.Defs.Densities] :?> float32[]) 
            | _ -> extract (fun c -> c.Data.[Hera.Defs.InternalEnergies] :?> float32[])
        let result = currFilteredData |> Array.map (fun v -> float v)
        result

    let filterDataForOneFrameBox (frame : Frame) (filter : option<Box3f>) (renderVal : RenderValue) = 
         let box = filter |> (fun elem -> defaultArg elem Box3f.Invalid) |> (fun b -> b.BoundingBox3d)
         let chunk = createBoxQuery box frame
         filterDataForOneFrame chunk renderVal
        
    //TODO: Connect the two filtering funnctions!!!!!!!!!!!!
    let filterDataForOneFrameSphere (frame : Frame) (filter : option<Sphere3d>) (renderVal : RenderValue) (discardProps : DiscardProperties) = 
         let sphere = filter |> (fun elem -> defaultArg elem Sphere3d.Invalid)
         let chunk = createSphereQuery sphere frame discardProps
         let result = filterDataForOneFrame chunk renderVal 
         let statistics = computeStatistics result renderVal
         result, statistics

    let filterAllDataForOneFrameSphere (frame : Frame) (filter : option<Sphere3d>) (discardProps : DiscardProperties) = 
        let sphere = filter |> (fun elem -> defaultArg elem Sphere3d.Invalid)
        let chunk = createSphereQuery sphere frame discardProps
        let extract f = chunk |> Array.map f |> Array.concat
        let energies = extract (fun c -> c.Data.[Hera.Defs.InternalEnergies] :?> float32[]) |> Array.map (fun v -> float v)
        let cubicRoots = extract (fun c -> c.Data.[Hera.Defs.CubicRootsOfDamage] :?> float32[]) |> Array.map (fun v -> float v)
        let strains = extract (fun c -> c.Data.[Hera.Defs.LocalStrains] :?> float32[]) |> Array.map (fun v -> float v)
        let alphaJutzis = extract (fun c -> c.Data.[Hera.Defs.AlphaJutzi] :?> float32[]) |> Array.map (fun v -> float v)
        let pressures = extract (fun c -> c.Data.[Hera.Defs.Pressures] :?> float32[]) |> Array.map (fun v -> float v)
        let masses = extract (fun c -> c.Data.[Hera.Defs.Masses] :?> float32[]) |> Array.map (fun v -> float v)
        let densities = extract (fun c -> c.Data.[Hera.Defs.Densities] :?> float32[]) |> Array.map (fun v -> float v)
        let allAttributes = 
             HashMap.ofList [RenderValue.Energy, energies; RenderValue.CubicRoot, cubicRoots; RenderValue.Strain, strains; 
                            RenderValue.AlphaJutzi, alphaJutzis; RenderValue.Pressure, pressures; RenderValue.Mass, masses; RenderValue.Density, densities]
        let allAttribsAndStats = computeAllAttributesAndStatistics allAttributes
        allAttribsAndStats

    let filterAllDataForOneFrame (filter : Box3f) (frame : Frame) = 
        let box = filter.BoundingBox3d
        let chunk = createBoxQuery box frame
        let extract f = chunk |> Array.map f |> Array.concat
        let energies =  extract (fun c -> c.Data.[Hera.Defs.InternalEnergies] :?> float32[]) |> Array.map (fun v -> float v)
        let cubicRoots = extract (fun c -> c.Data.[Hera.Defs.CubicRootsOfDamage] :?> float32[]) |> Array.map (fun v -> float v)
        let strain = extract (fun c -> c.Data.[Hera.Defs.LocalStrains] :?> float32[]) |> Array.map (fun v -> float v)
        let alphaJutzis = extract (fun c -> c.Data.[Hera.Defs.AlphaJutzi] :?> float32[]) |> Array.map (fun v -> float v)
        let pressures = extract (fun c -> c.Data.[Hera.Defs.Pressures] :?> float32[]) |> Array.map (fun v -> float v)
        zip5 energies cubicRoots strain alphaJutzis pressures

    let filterDataForAllFramesBox (frames : Frame[]) (filter : option<Box3f>) (renderVal : RenderValue) = 
        frames |> Array.map (fun frame -> filterDataForOneFrameBox frame filter renderVal)

    let filterDataForAllFramesSphere (frames : Frame[]) (filter : option<Sphere3d>) (renderVal : RenderValue) = 
        frames |> Array.map (fun frame -> filterDataForOneFrameSphere frame filter renderVal)

    let update (frames : Frame[]) (m : Model) (msg : Message) =

        let numOfFrames = frames.Length
        let positions = frames.[m.frame].positions
        match msg with
            | SetPointSize s -> { m with pointSize = s }
            | TogglePointSize -> 
                let newPointSize = if m.pointSize = 8.0 then 20.0 elif m.pointSize = 20.0 then 8.0 else m.pointSize
                { m with pointSize = newPointSize }
            | TogglePointDiscarded -> {m with discardPoints = not m.discardPoints}
            | NormalizeData -> {m with normalizeData = not m.normalizeData}
            | EnableShading -> {m with enableShading = not m.enableShading}
            | ReconstructNormal -> {m with reconstructNormal = not m.reconstructNormal}
            | ReconstructDepth -> {m with reconstructDepth = not m.reconstructDepth}
            | DisplayLowerOutliers -> 
                let currData = m.values.arr
                let lowerOutlierBoundary = findOutliers currData |> fst
                let range = m.outliersRange
                let newMin = if m.lowerOutliers then -infinity else lowerOutlierBoundary
                let newOutliersRange = {range with min = newMin}
                {m with 
                    lowerOutliers = not m.lowerOutliers
                    outliersRange = newOutliersRange}
            | DisplayHigherOutliers -> 
                let currData = m.values.arr
                let higherOutlierBoundary = findOutliers currData |> snd
                let range = m.outliersRange
                let newMax = if m.higherOutliers then infinity else higherOutlierBoundary
                let newOutliersRange = {range with max = newMax}
                {m with 
                    higherOutliers = not m.higherOutliers
                    outliersRange = newOutliersRange}
            | ChangeAnimation -> { m with playAnimation = not m.playAnimation}
            | AnimateAllFrames -> 
                let filteredDataAllFrames = filterDataForAllFramesBox frames m.boxFilter m.renderValue

                {m with 
                    animateAllFrames = not m.animateAllFrames
                    transition = not m.transition
                    filteredAllFrames = filteredDataAllFrames}
            | StepTime -> 
                let frameId =  (sw.Elapsed.TotalSeconds / 0.5) |> int
                let currFrame = frameId % numOfFrames
                // update filtered data using frameId and filter
                let isCurrentFilterSet = 
                    match m.boxFilter with 
                    | None -> false
                    | Some(_) -> true

                let currBBox = frames.[currFrame].pointSet.BoundingBox

                let currData = 
                    if isCurrentFilterSet && m.animateAllFrames then  
                        let filteredData = m.filteredAllFrames.[currFrame]
                        filteredData
                    else 
                        m.data.arr

                if m.playAnimation then sw.Start() else sw.Stop()
                { m with 
                    frame = currFrame 
                    currHeraBBox = currBBox
                    data = { version = m.data.version + 1; arr = currData }
                }
            | CameraMessage msg ->
                { m with cameraState = FreeFlyController.update m.cameraState msg }
            | SetRenderValue v -> 
                let renderValues = 
                    match v with
                    | RenderValue.Energy ->  frames.[m.frame].energies
                    | RenderValue.CubicRoot -> frames.[m.frame].cubicRoots
                    | RenderValue.Strain -> frames.[m.frame].strains
                    | RenderValue.AlphaJutzi -> frames.[m.frame].alphaJutzis
                    | RenderValue.Pressure -> frames.[m.frame].pressures
                    | RenderValue.Mass -> frames.[m.frame].masses
                    | RenderValue.Density -> frames.[m.frame].densities
                    | _ -> frames.[m.frame].energies
                let range = renderValues.GetBoundingRange()
                let minValue = range.Min
                let maxValue = range.Max
                let newDataRange = {
                    min = minValue
                    max = maxValue
                    }

                let filteredData = filterDataForOneFrameBox frames.[m.frame] m.boxFilter v

                let filteredDataAllFrames = 
                    if m.animateAllFrames then
                        let filtered = filterDataForAllFramesBox frames m.boxFilter v
                        filtered
                    else
                        let temp : float[] [] = Array.empty
                        temp

                {m with 
                    renderValue = v
                    values = { version = m.values.version + 1; arr = renderValues }
                    dataRange = newDataRange
                    initDataRange = newDataRange
                    data = { version = m.data.version + 1; arr = filteredData }
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
                    | 3 -> Some(Box3f(V3f(-16.0, -16.0, -16.0), V3f(16.0, 30.0, 16.0)))
                    | _ -> None

                let newDataPath =
                    match i with
                    | 1 -> "dataFilter1.csv"
                    | 2 -> "dataFilter2.csv"
                    | 3 -> "dataFilter3.csv"
                    | _ -> "data.csv"

                let filteredDataAllFrames = 
                    if m.animateAllFrames then
                        let filtered = filterDataForAllFramesBox frames newFilter m.renderValue
                        filtered
                    else
                        let temp : float[] [] = Array.empty
                        temp
                        
                let filteredData = filterDataForOneFrameBox frames.[m.frame] newFilter m.renderValue

                {m with 
                    boxFilter = newFilter
                    data = { version = m.data.version + 1 ; arr = filteredData }
                    filteredAllFrames = filteredDataAllFrames
                    dataPath = newDataPath}
            | ResetFilter -> {m with 
                                currFilter = None
                                boxFilter = None
                                sphereFilter = None
                                data = VersionedArray.ofArray [||]
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

    let view (runtime : IRuntime) (data : Frame[]) (m : AdaptiveModel) =
        let shuffleR (r : Random) xs = xs |> Seq.sortBy (fun _ -> r.Next())

        let temp = findOutliers data.[0].energies  
        
        let encodeToCSVData (data : Frame[]) =
            let builder = System.Text.StringBuilder()
            //let initValues = data.[0].values
            //let positions = data.[0].positions

            builder.AppendLine("energy,cubicRoot,strain,alphaJutzi,pressure") |> ignore

            let filter = Box3f(V3f(-5.0, -5.0, -5.0), V3f(5.0, 30.0, 5.0))

            let filteredValues = filterAllDataForOneFrame filter data.[0]

           // let filteredValues = filterDataFrames newProbeFilter data.[0]

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
        
        let frustum = 
            Frustum.perspective 60.0 0.1 100.0 1.0 
                |> AVal.constant

        //let filter = 
        //    m.filter |> AVal.map (fun filter ->
        //        match filter with 
        //        | Some f -> 
        //            match f.probe with 
        //            | Box b -> b
        //            | Sphere s -> Box3d.Infinite 
        //        | None -> Box3d.Infinite)

       
        let heraSg = 
            data
            |> HeraSg.createAnimatedSg m.frame m.pointSize m.discardPoints m.normalizeData 
                m.enableShading m.reconstructNormal m.reconstructDepth
                m.lowerOutliers m.higherOutliers m.outliersRange
                m.renderValue m.currentMap 
                m.domainRange m.clippingPlane m.boxFilter m.currFilters m.dataRange m.colorValue.c 
                m.cameraState.view
                runtime
            |> Sg.noEvents

        let currentBox = 
            m.boxFilter |> AVal.map (fun b ->
                match b with 
                    | Some box -> box.BoundingBox3d
                    | None -> Box3d.Infinite
                )

        let currentSpherePosTrafo = 
            m.sphereFilter |> AVal.map (fun s ->
                match s with 
                    | Some sphere -> Trafo3d.Translation(sphere.Center)
                    | None -> Trafo3d.Identity)

        let currentSphereRadius = 
            m.sphereFilter |> AVal.map (fun s ->
                match s with 
                    | Some sphere -> sphere.Radius
                    | None -> 0.0)

        let sphereSg = 
            Sg.sphere 6 m.boxColor currentSphereRadius
            |> Sg.noEvents
            |> Sg.trafo currentSpherePosTrafo
            |> Sg.fillMode (FillMode.Line |> AVal.constant)

        let boxSg = 
            Sg.box m.boxColor currentBox
            |> Sg.noEvents
            |> Sg.fillMode (FillMode.Line |> AVal.constant)

        let sg = 
            Sg.ofSeq [heraSg; boxSg; sphereSg]
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
                    yield button [onClick (fun _ -> AnimateAllFrames); style "margin-inline-start: 5px; margin-top: 5px; margin-bottom: 5px"] [text "One Frame"]
                else 
                    yield button [onClick (fun _ -> AnimateAllFrames); style "margin-inline-start: 5px; margin-top: 5px; margin-bottom: 5px"] [text "All Frames"]

            }

        let colorMaps = AMap.map (fun k v -> text v) m.colorMaps

        let dependencies = 
            [
                { kind = Script; name = "d3"; url = "http://d3js.org/d3.v4.min.js" }
                { kind = Stylesheet; name = "histogramStyle"; url = "Histogram.css" }
                { kind = Script; name = "histogramScript"; url = "Histogram.js" }
                { kind = Stylesheet; name = "parallCoordStyle"; url = "ParallCoords.css" }
                { kind = Script; name = "parallCoordScript"; url = "ParallCoords.js" }
                { kind = Script; name = "boxPlotScript"; url = "BoxPlot.js" }
            ]

       // let a = m.data | AVal.map (fun data -> encodeToJSONData)
       // let multidimjsondescription = a.Channel
        
       // let da = AVal.map2 (fun values animatie -> sprintf "{values = %s; animate = %A}" values animate) m.data m.animateAllFrames
       // let guh = da.Channel

        let dataChannel = m.data.arr.Channel
        let transitionChannel = m.transition.Channel
        let updateChart =
            "initHisto(__ID__); data.onmessage = function (values) { refresh(values);  }; transition.onmessage = function (v) { shouldUseTransition = v; }"  // subscribe to m.data
        let updateChart1 =
            "initHisto(__ID__); data.onmessage = function (values) { refresh(values);  }; transition.onmessage = function (v) { shouldUseTransition = false; }"  // subscribe to m.data

        let pathChannel = m.dataPath.Channel
        let updateParallCoords = 
            "initParallCoords(__ID__); dataPath.onmessage = function (path) { if (path != null) refreshPar(path); };"

        //let boxplotData = m.boxPlotData1 |> AMap.toAVal
        let boxPlotChannel = m.boxPlotData.Channel
        let attributeChannel = m.boxPlotAttribute.Channel
        let updateBoxPlot =
            "initBoxPlot(__ID__); boxPlotAttribute.onmessage = function (a) { attribute = a }; boxPlotData.onmessage = function (data) { refreshBoxPlot(data); }"

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
        page (fun request -> 
            match Map.tryFind "page" request.queryParams with 
            | Some "mainPage" -> 
                require Html.semui (
                    body [] [
                        FreeFlyController.controlledControl m.cameraState CameraMessage frustum (AttributeMap.ofList att) sg
                        require dependencies ( // here we execute Histogram.js.... all values, functions defined in Histogram.js are further down accessible...
                            div[style "margin: 7px"] [
                                div [style "position: fixed; left: 10px; top: 10px; width: 220px"] [
                                Incremental.div AttributeMap.empty dynamicNameChange
                                //  br []
                                div [style "margin-top: 3px; margin-bottom: 3px"] [
                                    span [style "padding: 4px; color: white"] [text "Point Size: "]
                                    numeric { min = 1.0; max = 20.0; smallStep = 1.0; largeStep = 5.0 } [style "width: 55%; padding: 2px"] m.pointSize SetPointSize
                                ]
                                // Incremental.div AttributeMap.empty dynamicUI
                                // br []
                                dropdown { placeholder = "ColorMaps"; allowEmpty = false} [ clazz "ui selection"; style "margin-top: 5px; margin-bottom: 5px"] colorMaps (m.currentMap |> AVal.map Some) SetTransferFunc
                                // br []
                                dropdown1 [ clazz "ui selection"; style "margin-top: 5px; margin-bottom: 5px"] renderValues m.renderValue SetRenderValue
                                // br []
                                div [ style "color: white" ] [

                                    div [style "width: 90%; margin-top: 6px; margin-bottom: 8px"] [ 
                                        simplecheckbox { 
                                            attributes [clazz "ui inverted checkbox"]
                                            state m.normalizeData
                                            toggle NormalizeData
                                            content [ text "Normalize Data"]  
                                        }
                                    ]

                                    div [style "width: 90%; margin-top: 6px; margin-bottom: 8px"] [ 
                                        simplecheckbox { 
                                            attributes [clazz "ui inverted checkbox"]
                                            state m.enableShading
                                            toggle EnableShading
                                            content [ text "Enable Shading"]  
                                        }
                                    ]

                                    div [style "width: 90%; margin-top: 6px; margin-bottom: 8px"] [ 
                                        simplecheckbox { 
                                            attributes [clazz "ui inverted checkbox"]
                                            state m.reconstructNormal
                                            toggle ReconstructNormal
                                            content [ text "Reconstruct Normal"]  
                                        }
                                    ]

                                    div [style "width: 90%; margin-top: 6px; margin-bottom: 8px"] [ 
                                        simplecheckbox { 
                                            attributes [clazz "ui inverted checkbox"]
                                            state m.reconstructDepth
                                            toggle ReconstructDepth
                                            content [ text "Reconstruct Depth"]  
                                        }
                                    ]
                              
                                    div [style "width: 90%; margin-top: 5px; margin-bottom: 5px"] [  
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

                                    //br []
                       
                                    div [ clazz "item"; style "width: 90%; margin-top: 7px; margin-bottom: 5px" ] [
                                        span [style "padding: 2px"] [text "X: "]
                                        slider { min = -16.0; max = 16.0; step = 0.5 } [clazz "ui inverted slider"; style "padding: 3px"] (m.clippingPlane |> AVal.map (fun cp -> cp.x)) (fun v -> SetClippingPlane (X, v))
                                        span [style "padding: 2px"] [text "Y: "]
                                        slider { min = -16.0; max = 30.0; step = 0.5 } [clazz "ui inverted slider"; style "padding: 3px"] (m.clippingPlane |> AVal.map (fun cp -> cp.y)) (fun v -> SetClippingPlane (Y, v))
                                        span [style "padding: 2px"] [text "Z: "]
                                        slider { min = -16.0; max = 16.0; step = 0.5 } [clazz "ui inverted slider"; style "padding: 3px"] (m.clippingPlane |> AVal.map (fun cp -> cp.z)) (fun v -> SetClippingPlane (Z, v))

                                    ]
                            

                                    //br []

                                    div [style "width: 90%; margin-top: 6px; margin-bottom: 8px"] [ 
                                        simplecheckbox { 
                                            attributes [clazz "ui inverted checkbox"]
                                            state m.discardPoints
                                            toggle TogglePointDiscarded
                                            content [ text "Discard Out of Range?"]  
                                        }
                                    ]

                                    //br []

                                    div [clazz "colorRange"; style "width: 90%; margin-top: 9px; margin-bottom: 9px"] [ 
                                        span [style "padding: 2px; padding-inline-end: 20px"] [text "Out of Range Color:"]
                                        ColorPicker.view m.colorValue |> UI.map SetColorValue
                                    ]

                                    //br []

                                    div [style "width: 90%; margin-top: 6px; margin-bottom: 8px"] [ 
                                        simplecheckbox { 
                                            attributes [clazz "ui inverted checkbox"]
                                            state m.lowerOutliers
                                            toggle DisplayLowerOutliers
                                            content [ text "Lower"]  
                                        }
                                        simplecheckbox { 
                                            attributes [clazz "ui inverted checkbox"; style "padding-inline-start: 6px"]
                                            state m.higherOutliers
                                            toggle DisplayHigherOutliers
                                            content [ text "Higher Outliers"]  
                                        }
                                    ]

                                    Incremental.div (AttributeMap.ofAMap (amap {
                                        yield style "width: 95%; margin-top: 5px; margin-bottom: 8px"
                                    })) (alist {
                                        let! initRange = m.initDataRange
                            
                                        //TODO: not very good way... loosing some precision this way!!!
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
                                                value (m.dataRange |> AVal.map (fun r -> (roundDecimal r.min)))
                                                update (fun v -> SetDataRange (Min, (roundDecimal v)))
                                                step (fst steps)
                                                largeStep (snd steps)
                                                min (roundDecimal initRange.min)
                                                max (roundDecimal initRange.max)
                                                }

                                        yield span [style "padding: 3px"] [text "Max: "]
                                        yield simplenumeric {
                                                attributes [style "width: 80%; padding: 2px"]
                                                value (m.dataRange |> AVal.map (fun r -> (roundDecimal r.max)))
                                                update (fun v -> SetDataRange (Max, (roundDecimal v)))
                                                step (fst steps)
                                                largeStep (snd steps)
                                                min (roundDecimal initRange.min)
                                                max (roundDecimal initRange.max)
                                                }    
                                        }
                                    )

                                    // br []

                                    div [style "margin-top: 5px; margin-bottom: 5px"] [
                                        button [onClick (fun _ -> SetFilter 1); style "margin-inline-start: 2px; margin-inline-end: 2px"] [text "Probe 1"]
                                        button [onClick (fun _ -> SetFilter 2); style "margin-inline-start: 2px; margin-inline-end: 2px"] [text "Probe 2"]
                                        button [onClick (fun _ -> SetFilter 3); style "margin-inline-start: 2px; margin-inline-end: 2px"] [text "Probe 3"]
              
                                    ]
                                    button [onClick (fun _ -> ResetFilter); style "margin: 2px; margin-top: 0px; margin-bottom: 5px"] [text "Reset"]
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

                                    yield span [style "position: relative; font-size: 0.8em; right: 10px; color: #ffffff"] [Incremental.text (m.dataRange |> AVal.map (fun r -> (roundDecimal r.min).ToString()))] 
                                    yield span [style "position: inherit; font-size: 0.8em; right: 10px; color: #ffffff"] [Incremental.text (m.dataRange |> AVal.map (fun r -> (roundDecimal r.max).ToString()))]
                                    }
                                )
                        
                                let histogram = 
                                        onBoot' [("data", dataChannel); ("transition", transitionChannel)] updateChart ( // when div [histogram etc] is constructed updateChart is called.
                                                    div [onBrushed; clazz "histogram"; 
                                                        style "position: fixed; bottom: 20px; right: 20px; width:400px; height: 260px; text-align: center; background-color: white; z-index: 1000"] [
                                                        span [style "padding: 2px; display: inline-block; font-size: x-large; color: black"] [
                                                            Incremental.text m.attributeText] 
                                                    ]          
                                        )
                            

                                let parallCoords = 
                                            onBoot' [("dataPath", pathChannel)] updateParallCoords (
                                                    div [onParallCoordsBrushed; clazz "parallCoords"; style "position: fixed; bottom: 20px; right: 20px; width:400px; height: 260px; z-index: 1000"] []          
                                            )

                                Html.SemUi.tabbed [clazz "temp"; style "position: fixed; bottom: 250px; right: 20px" ] [
                                    ("Histogram", histogram)
                                    ("ParallCoords", parallCoords)
                                ] "Histogram"
                            ]
                        )  
                    ]
                    )
            | Some "histogramPage" ->
                body [] [
                    DomNode.Text ("style", Option.None, AttributeMap.Empty, (AVal.constant "
                        #histogramSvg {
                            width: 100%;
                            height: 90%;
                            }"))
                    require dependencies (
                        onBoot' [("data", dataChannel); ("transition", transitionChannel)] updateChart1 ( // when div [histogram etc] is constructed updateChart is called.
                            div [onBrushed; clazz "histogram"; style "width: 100%; height: 100%; text-align: center; background-color: white; z-index: 1000"] [
                                span [style "padding: 2px; display: inline-block; font-size: 80px; color: black"] [ Incremental.text m.attributeText] 
                            ] 
                        )
                    )
                ]
            | Some "boxPlotPage" ->
                body [] [
                    require dependencies (
                        onBoot' [("boxPlotAttribute", attributeChannel); ("boxPlotData", boxPlotChannel)] updateBoxPlot (
                            div [clazz "boxPlot"; style "width: 100%; height: 100%"] [] 
                        )
                    )
                ]
            | Some other ->
                let msg = sprintf "Unknown page: %A" other
                body [] [
                    div [style "color: white; font-size: large; background-color: red; width: 100%; height: 100%"] [text msg]
                ] 
            | None -> 
                body [] []
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
        let frames = DataLoader.loadDataAllFrames runtime
        {
            initial = initial frames // store data Hera frame as array to model....
            update = update frames
            view = view runtime frames
            threads = threads
            unpersist = Unpersist.instance
        }