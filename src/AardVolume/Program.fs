open AardVolume

open Aardium
open Aardvark.Service
open Aardvark.UI
open Suave
open Suave.WebPart
open Aardvark.Rendering.Vulkan
open Aardvark.Base
open System




[<EntryPoint>]
let main args =

    //DSLAwesomness.Points.run () |> ignore
    //System.Environment.Exit 0

    SimpleRenderer.main args |> ignore

    System.Environment.Exit 0

    Aardvark.Init()
    Aardium.init()

    let app = new Aardvark.Application.Slim.OpenGlApplication()

    WebPart.startServer 4321 [
        MutableApp.toWebPart' app.Runtime false (App.start App.app)
    ] |> ignore
    
    Aardium.run {
        title "Aardvark rocks \\o/"
        width 1024
        height 768
        url "http://localhost:4321/"
    }

    0
