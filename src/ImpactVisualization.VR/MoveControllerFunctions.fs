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
open ImpactVisualization.UpdateFunctions
open Aardvark.Cef

module MoveControllerFunctions =
    
    let updateSphereScalingFactor (model : Model) = 
        let scalingFactor =
            if not model.scalingSphere then
                model.sphereScale
            else 
                let contrPos = model.mainControllerTrafo.Forward.TransformPos(V3d.OOO)
                let scalerPos = model.secondControllerTrafo.Forward.TransformPos(V3d.OOO)
                let distance = contrPos.Distance(scalerPos)
                let newScale = (4.0/3.0)*Math.PI*Math.Pow((distance + model.sphereRadius), 3.0)
                newScale + model.sphereRadius/2.0
        {model with sphereScale = scalingFactor}

    let updateTvTrafos (model : Model) = 
        let newTvScaleTrafo = 
            if model.grabbingTV && model.mainTouching then
                Trafo3d(Scale3d(model.scalingFactorTv))
            else 
                model.lastTvScaleTrafo

        let newTvTransformations, updatedTvQuad = 
            if model.grabbingTV then 
                let tvTrafo = model.tvToControllerTrafo * model.mainControllerTrafo
                let rotation = Trafo3d.RotationEulerInDegrees(90.0, 0.0, -90.0)
                let translation = Trafo3d.Translation(2.5, 1.0, 1.5)

                let newTvTrafos = newTvScaleTrafo * rotation * translation * tvTrafo
                let newTvQuad = newTvTrafos.Forward.TransformedPosArray(browserQuad.Points.ToArray(4)) |> Quad3d

                newTvTrafos, newTvQuad
            else 
                model.tvTransformations, model.tvQuad
        {model with 
            lastTvScaleTrafo = newTvScaleTrafo
            tvTransformations = newTvTransformations
            tvQuad = updatedTvQuad}

    let updateRay (client : Browser) (model : Model)  = 
        let initRay = 
            if model.rayActive then
                let origin = model.mainControllerTrafo.Forward.TransformPos(V3d(0.0, 0.02, 0.0))
                let direction = model.mainControllerTrafo.Forward.TransformPos(V3d.OIO * 1000.0)
                Ray3d(origin, direction)
            else Ray3d.Invalid    
        //let intersect = initRay.Intersects(model.tvQuad)
        let mutable hit = RayHit3d.MaxRange
        let hitPoint = if model.rayActive then initRay.Hits(model.tvQuad, &hit) else false
        let currRay, rColor, screenCoordsHitPos =
            if model.rayActive then 
                if hitPoint then
                    let screenCoords = (V2d(hit.Coord.X * screenResolution.ToV2d().X, hit.Coord.Y * screenResolution.ToV2d().Y)).ToV2i()
                    let screenPos = PixelPosition(screenCoords, screenResolution.X, screenResolution.Y)
                    if model.rayTriggerClicked then
                        client.Mouse.Move(screenPos)
                    Ray3d(initRay.Origin, hit.Point), C4b.Green, screenPos
                else
                    initRay, C4b.Red, PixelPosition()
            else Ray3d.Invalid, C4b.Red, PixelPosition()

        {model with 
            screenIntersection = hitPoint
            hitPoint = hit.Point
            screenHitPoint = hit.Coord
            ray = currRay
            rayColor = rColor
            screenCoordsHitPos = screenCoordsHitPos}



