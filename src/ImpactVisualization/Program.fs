open AardVolume

open Aardium
open Aardvark.UI
open Suave
open Aardvark.Base
open System


[<EntryPoint>]
let main args =

    Aardvark.Init()
    Aardium.init()

    let app = new Aardvark.Application.Slim.OpenGlApplication()

    let media = App.app app.Runtime

    app.ShaderCachePath <- None

    WebPart.startServer 4321 [
        MutableApp.toWebPart' app.Runtime false (App.start media)
        Reflection.assemblyWebPart typeof<AardVolume.EmbeddedRessource>.Assembly
    ] |> ignore
    
    Aardium.run {
        title "Aardvark rocks \\o/"
        width 1024
        height 768
        url "http://localhost:4321/"
    }

    0
