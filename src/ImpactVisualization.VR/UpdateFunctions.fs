namespace ImpactVisualization

open System
open System.IO
open System.Text.Json
open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Vr
open Aardvark.Application
open Aardvark.Rendering.GL


open FSharp.Data.Adaptive
open FSharpx.Collections

open AardVolume.Model
open AardVolume.App
open ImpactVisualization
open Aardvark.Cef


module UpdateFunctions = 
    
    let rd   = V3d(0.732, -0.41483, 0.013)
    let ld = V3d(-0.732, -0.41483, 0.013)
    let lu  = V3d(-0.732, 0.432, 0.013)
    let ru = V3d(0.732, 0.432, 0.013)
    let browserQuad = Quad3d(lu, ru, rd, ld)

    let planePos0 = V3d(-0.732, 0.05, -0.5)
    let planePos1 = V3d(-0.732, 0.05, 0.5)
    let planePos2 = V3d(0.732, 0.05, 0.5)
    let planePos3 = V3d(0.732, 0.05, -0.5)

    let screenResolution = V2i(1200,790)

    let texturesPath = @"..\..\..\src\ImpactVisualization\resources\textures\"
    
    let fromStreamToTexture textureName = 
        let s = File.Open((texturesPath + textureName), FileMode.Open, FileAccess.Read, FileShare.Read)
        let p = PixImage.Create s
        let t = PixTexture2d(PixImageMipMap [| p |], true) :> ITexture
        t

    let convertPixImageToITexture (p : PixImage) =
        PixTexture2d(PixImageMipMap [| p |], true) :> ITexture

    let allTextures = 
           HashMap.ofList ["initial", fromStreamToTexture "initial.png"; 
                           "left", fromStreamToTexture "left.png"; 
                           "middle", fromStreamToTexture "middle.png"; 
                           "right", fromStreamToTexture "right.png"; 
                           "analyze", fromStreamToTexture "analyze.png"; 
                           "bottom-left", fromStreamToTexture "bottom-left.png"; 
                           "bottom-middle", fromStreamToTexture "bottom-middle.png"; 
                           "bottom-right", fromStreamToTexture "bottom-right.png"; 
                           "top-left", fromStreamToTexture "top-left.png"; 
                           "top-middle", fromStreamToTexture "top-middle.png"; 
                           "top-right", fromStreamToTexture "top-right.png"; 
                           "initial-attributes", fromStreamToTexture "initial-attributes.png"; 
                           "scale-down", fromStreamToTexture "scale-down.png"; 
                           "scale-up", fromStreamToTexture "scale-up.png"; 
                           "initial-scaling", fromStreamToTexture "initial-scaling.png";
                           "energy", fromStreamToTexture "energy.png";
                           "cubicroot", fromStreamToTexture "cubicroot.png";
                           "strain", fromStreamToTexture "strain.png";
                           "alphajutzi", fromStreamToTexture "alphajutzi.png";
                           "pressure", fromStreamToTexture "pressure.png";
                           "density", fromStreamToTexture "density.png";
                           "probe", fromStreamToTexture "probe.png";
                           "probe-alphajutzi", fromStreamToTexture "probe-alphajutzi.png";
                           "probe-cubicroot", fromStreamToTexture "probe-cubicroot.png";
                           "probe-density", fromStreamToTexture "probe-density.png";
                           "probe-energy", fromStreamToTexture "probe-energy.png";
                           "probe-pressure", fromStreamToTexture "probe-pressure.png";
                           "probe-strain", fromStreamToTexture "probe-strain.png";
                           "ray", fromStreamToTexture "ray.png";
                           "clipping", fromStreamToTexture "clipping.png";
                           "select-attribute", fromStreamToTexture "select-attribute.png";
                           "select-tool", fromStreamToTexture "select-tool.png";
                           "empty", fromStreamToTexture "empty.png";
                           "grab-sphere", fromStreamToTexture "grab-sphere.png";
                           "scaling-up", fromStreamToTexture "scaling-up.png";
                           "scaling-down", fromStreamToTexture "scaling-down.png";
                           "histogram-selected", fromStreamToTexture "histogram-selected.png";
                           "statistics-selected", fromStreamToTexture "statistics-selected.png";
                           "histogram", fromStreamToTexture "histogram.png";
                           "statistics", fromStreamToTexture "statistics.png";
                           "delete-object", fromStreamToTexture "delete-object.png";
                           "analyze-probes", fromStreamToTexture "analyze-probes.png";
                           "analyze-region", fromStreamToTexture "analyze-region.png";
                           "analyze-region-screen", fromStreamToTexture "analyze-region-screen.png";
                           "analyze-time", fromStreamToTexture "analyze-time.png";
                           "analyze-time-screen", fromStreamToTexture "analyze-time-screen.png";
                           "main-controller", fromStreamToTexture "main-controller.png";
                           "play-raw", fromStreamToTexture "play-raw.png";
                           "play-play", fromStreamToTexture "play-play.png";
                           "play-forward", fromStreamToTexture "play-forward.png";
                           "play-backward", fromStreamToTexture "play-backward.png";
                           "play-stop", fromStreamToTexture "play-stop.png";
                           "play-screenshot", fromStreamToTexture "play-screenshot.png";
                           "pause-raw", fromStreamToTexture "pause-raw.png";
                           "pause-pause", fromStreamToTexture "pause-pause.png";
                           "pause-forward", fromStreamToTexture "pause-forward.png";
                           "pause-backward", fromStreamToTexture "pause-backward.png";
                           "pause-stop", fromStreamToTexture "pause-stop.png";
                           "pause-screenshot", fromStreamToTexture "pause-screenshot.png"]

    let defaultTex = allTextures.Item "empty"
    let getTexture tex = Option.defaultValue defaultTex tex
    let texture tex = allTextures.TryFind(tex) |> getTexture    

    let computeNewAttributeTextures (newAttribute : RenderValue) = //(currAttribute : RenderValue) = 
        //match currAttribute with 
        //| a when a = newAttribute && newAttribute <> RenderValue.Energy -> model.touchpadTexture, model.contrScreenTexture // if the attribute is the same then texture should not be loaded again   
        //| _ ->
        let texName, screenTexName = 
            match newAttribute with 
            | RenderValue.Energy -> "top-right", "energy"
            | RenderValue.CubicRoot -> "top-middle", "cubicroot"
            | RenderValue.Strain -> "top-left", "strain"
            | RenderValue.AlphaJutzi -> "bottom-left", "alphajutzi"
            | RenderValue.Pressure -> "bottom-middle", "pressure"
            | RenderValue.Density -> "bottom-right", "density"
            | _ -> "initial-attributes", "select-attribute"
        (texture texName), (texture screenTexName)

    let trafoOrIdentity trafo = Option.defaultValue Trafo3d.Identity trafo
    
    let newOrOldTrafo (id : int) (trafo : Trafo3d) (contrId : Option<int>) (contrTrafo : Trafo3d) =
        match contrId with 
        | Some i when i = id -> trafo 
        | _ -> contrTrafo
        
    let idIsSet (contrId : Option<int>) = contrId.IsSome
    
    let sphereIntersection (controllerSphere : Sphere3d) (probe : Sphere3d) = controllerSphere.Intersects(probe)

    // https://stackoverflow.com/questions/3717226/radius-of-projected-sphere
    let probeInScreenCoordinates (viewTr : Trafo3d) (projTr : Trafo3d) (probeCenter : V3d) (radius : float) = 
        let center_view = viewTr.Forward.TransformPos(probeCenter)
        let pointOnSphere = probeCenter + V3d(radius, 0.0, 0.0)
        let pointOnSphere_view = viewTr.Forward.TransformPos(pointOnSphere)
        let radius_view = center_view.Distance(pointOnSphere_view)
        let pointOnSphere_perpendicular = center_view + V3d(radius_view, 0.0, 0.0)
        let center_screen = projTr.Forward.TransformPosProj(center_view)
        let pointOnSphere_screen = projTr.Forward.TransformPosProj(pointOnSphere_perpendicular)
        let radius_screen = center_screen.Distance(pointOnSphere_screen)
        center_screen, radius_screen
        
    let createProbe (numberId : int) (currSelected : bool) (selected : bool) (timesSelected : int) (timeAnalyze : bool) (color : C4b) (pos : V3d) (rad : float) (posToHera : V3d) (radToHera : float) 
                    (inside : bool) (allData : HashMap<RenderValue, (float[] * string)> []) 
                    (attribute : RenderValue) (showStats : bool) (showHisto : bool) 
                    (billboardType : BillboardType) : Probe = 
        {
            id = Guid.NewGuid().ToString()
            numberId = numberId
            currSelected = currSelected
            selected = selected
            timesSelected = timesSelected
            timeAnalyze = timeAnalyze
            color = color
            center = pos
            radius = rad
            centerRelToHera = posToHera
            radiusRelToHera = radToHera
            insideHera = inside
            allData = allData
            currAttribute = attribute
            showStatistics = showStats
            currHistogram = None
            showHistogram = showHisto
            currBillboard = billboardType
        }
    
    let createBoxPlot (region : bool)(attribute : RenderValue) (trafo : Trafo3d) (corners : Quad3d) 
        (texture : PixImage) (data : HashMap<int, float[]>) (allSelectedProbes : HashMap<int, Probe>) 
        (screenPos : V2d []) (probesPositions : (string * V3d) []) (timeProbePos : V3d): BoxPlot =
        {   
            id = Guid.NewGuid().ToString()
            isRegion = region
            attribute = attribute
            trafo = trafo
            positions = corners
            texture = texture 
            data = data
            probeIds = allSelectedProbes
            screenPos = screenPos
            probesPositions = probesPositions
            timeProbePos = timeProbePos
        }
        
    let convertCartesianToPolar (cartCoords : V2d) = 
        let x = cartCoords.X
        let y = cartCoords.Y
        
        let r = Math.Sqrt(Math.Pow(x, 2.0) + Math.Pow(y, 2.0))
        let t = Math.Atan2(y, x)
        let theta = 
            let angle = t * (180.0 / Math.PI) 
            if y < 0.0 then angle + 360.0 else angle
        r, theta
    
    let computeNewAttribute (touchpadPos : V2d) (currAttribute : RenderValue) =
        let r, theta = convertCartesianToPolar touchpadPos
        if r >= 0.5 then 
            match theta with 
            | t when t >= 0.0   && t < 60.0  -> RenderValue.Energy
            | t when t >= 60.0  && t < 120.0 -> RenderValue.CubicRoot
            | t when t >= 120.0 && t < 180.0 -> RenderValue.Strain
            | t when t >= 180.0 && t < 240.0 -> RenderValue.AlphaJutzi
            | t when t >= 240.0 && t < 300.0 -> RenderValue.Pressure
            | t when t >= 300.0 && t < 360.0 -> RenderValue.Density
            | _ -> currAttribute
        else 
            currAttribute
    
    
    let initialScalingHera = 0.05
    let initialScalingTv = 2.5
    
    let tvTrafos = 
        let scale = Trafo3d(Scale3d(initialScalingTv))
        let rotation = Trafo3d.RotationEulerInDegrees(90.0, 0.0, -90.0)
        let translation = Trafo3d.Translation(2.5, 1.0, 1.5)
        scale * rotation * translation
    
    let defaultBoxPlotPositions = Quad3d(V3d(-1.7, -1.0, 0.0), V3d(1.7, -1.0, 0.0), V3d(1.7, 1.0, 0.0), V3d(-1.7, 1.0, 0.0))
    
    let defaultBoxPlotsTrafo = 
        let scale = Trafo3d.Scale(0.3)
        let rotation = Trafo3d.RotationXInDegrees(35.0)
        let translation = Trafo3d.Translation(0.0, 0.25, 0.1)
        scale * rotation * translation
    
    let getScreenshot (histogramClient : Browser) = 
        let pixImage = PixImage<byte>(Col.Format.BGRA,histogramClient.Size.GetValue())
        let temp = pixImage.GetMatrix<C4b>().SetByCoord(fun (v : V2l) -> histogramClient.ReadPixel(V2i v) |> C4b) 
        pixImage :> PixImage

    let createColorPixImage (boxPlotClient : Browser) (color : C4b) = 
        let pixImage = PixImage<byte>(Col.Format.BGRA,boxPlotClient.Size.GetValue())
        let temp = pixImage.GetMatrix<C4b>().SetByCoord(fun (v : V2l) -> color) 
        pixImage :> PixImage

    let computeSleepTime (firstHistogram : bool) (dataSize : int) = 
        match dataSize with 
        | d when 800000 <= d -> 1800
        | d when (500000 <= d && d < 800000) || firstHistogram -> 1400
        | d when 100000 <= d && d < 500000 -> 800
        | d when 50000 <= d && d < 100000 -> 300
        | d when 10000 <= d && d < 50000 -> 200
        | d when 100 <= d && d < 10000 -> 130
        | _ -> 100

    let takeFrameScreenshot (frame : int) (model : Model) = 
        if model.currProbeAnalyzeTime.IsSome && model.boxPlotFrames.TryFind(frame).IsNone then
            let currProbe = model.currProbeAnalyzeTime.Value
            let arrayBoxPlot, statsBoxPlot = currProbe.allData.[frame].Item model.currBoxPlotAttrib
            //let newOrder = model.framesOrder |> Seq.append([frame])
            let newHashmap = model.boxPlotFrames.Add(frame, arrayBoxPlot)
            let newOrder = newHashmap |> HashMap.toSeq |> Seq.map (fun result -> fst result)
            let dataForBoxPlot = newHashmap |> HashMap.toSeq |> Seq.map (fun result -> snd result ) |> Seq.toArray

            let updatedTwoDmodel = 
                { model.twoDModel with
                    framesOrder = newOrder |> Seq.toArray
                    boxPlotData = dataForBoxPlot
                }
                
            { model with 
                boxPlotFrames = newHashmap
                framesOrder = newOrder
                twoDModel = updatedTwoDmodel}                
        else 
            model