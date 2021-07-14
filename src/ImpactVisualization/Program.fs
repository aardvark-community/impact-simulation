open AardVolume

open Aardium
open Aardvark.UI
open Suave
open Aardvark.Base
open Aardvark.Rendering
open System

open Aardvark.Cef
open FSharp.Data.Adaptive




[<EntryPoint>]
let main args =

    Xilium.CefGlue.ChromiumUtilities.unpackCef()
    Aardvark.Cef.Internal.TempCef.init()
    
    Aardvark.Init()
    Aardium.init()

    let app = new Aardvark.Application.Slim.OpenGlApplication()

    let boxPlotClient = new Browser(null,AVal.constant System.DateTime.Now,app.Runtime, true, AVal.constant (V2i(1920, 1140)))

    let media = App.app app.Runtime boxPlotClient

    let runtime = app.Runtime :> IRuntime
    runtime.ShaderCachePath <- None

    WebPart.startServer 4321 [
        MutableApp.toWebPart' app.Runtime false (App.start media)
        Reflection.assemblyWebPart typeof<AardVolume.EmbeddedRessource>.Assembly
    ] |> ignore
    
    boxPlotClient.LoadUrl "http://localhost:4321/?page=boxPlotPage" |> ignore


    Aardium.run {
        title "Aardvark rocks \\o/"
        width 1024
        height 768
        url "http://localhost:4321/?page=mainPage"
        //url "http://localhost:4321/?page=controllersPage"
    }

    Aardvark.Cef.Internal.TempCef.shutdown()

    0
