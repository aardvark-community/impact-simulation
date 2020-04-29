open AardVolume

open Aardium
open Aardvark.Service
open Aardvark.UI
open Suave
open Suave.WebPart
open Aardvark.Rendering.Vulkan
open Aardvark.Base
open System


type EmbeddedRessource = EmbeddedRessource // THIS IS NECESSARY


[<EntryPoint>]
let main args =

    // plain rendering applications
    //DSLAwesomness.Points.run () |> ignore
    //SimpleVolumeRenderer.main args |> ignore

    Aardvark.Init()
    Aardium.init()

    let app = new Aardvark.Application.Slim.OpenGlApplication()

    let media = App.app app.Runtime

    app.ShaderCachePath <- None


    let resourcesFolder = @"..\..\..\src\Sketch\resources"

    WebPart.startServer 4321 [
      //  Suave.Files.browse resourcesFolder
        MutableApp.toWebPart' app.Runtime false (App.start media)
        Suave.Files.browse resourcesFolder
    ] |> ignore
    
    Aardium.run {
        title "Aardvark rocks \\o/"
        width 1024
        height 768
        url "http://localhost:4321/"
    }

    0
