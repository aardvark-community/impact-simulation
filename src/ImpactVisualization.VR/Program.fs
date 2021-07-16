open Aardvark.Base
open Aardvark.UI
open Aardvark.Vr
open Aardium
open ImpactVisualization
open Suave
open Offler

open Aardvark.Cef
open FSharp.Data.Adaptive


open Aardvark.Geometry.Points
open Uncodium.SimpleStore

let loadOctreeFromStore storepath =
    let id = storepath + ".key" |> File.readAllText
    let store = (new SimpleDiskStore(storepath)).ToPointCloudStore()
    let pointcloud = store.GetPointSet(id)
    (pointcloud, store)

[<EntryPoint>]
let main argv =
    // initialize browser controls...
    Xilium.CefGlue.ChromiumUtilities.unpackCef()
    Aardvark.Cef.Internal.Cef.init()
    Offler.Init()
    Aardvark.Init()

    // dont remove this code for now. although it does not do anything it is needed. please dont ask for now :(
    //let createData() =

    //    // @"C:\Users\vasileva\source\hera_data\impact.0110"
    //    // @"D:\TU Wien\Master\4. Semester\Praktikum aus Visual Computing\Data\r80_p0_m500_v6000_mbasalt_a1.0_1M\data"

    //    let datafile  = @"C:\Users\vasileva\source\hera_data\impact.0111"
    //    let storepath = datafile + ".store"

    //    let id = Hera.Hera.importHeraDataIntoStore datafile storepath false
    //    printfn "%s" id

    ////createData()

    //let loadData() =
    //    let datafile  = if System.Environment.UserName = "hs" then @"F:\notebooks\frame\impact.0400" else @"C:\Users\vasileva\source\hera_data\impact.0111" 
    //    let storepath = datafile + ".store"

    //    let (p, store) = loadOctreeFromStore storepath
    //    let bb = p.Root.Value.BoundingBoxExactGlobal
    //    let root = p.Root.Value
    //    ()

    //loadData()

    Aardium.init()

    //let gl = Aardvark.Application.Slim.OpenGlApplication()

    let app = VRApplication.create' (VRDisplay.OpenVR 1.0) Aardvark.Application.Backend.Vulkan 8 false
    //let client = new Browser(null,AVal.constant System.DateTime.Now,gl.Runtime, true, AVal.constant (ImpactVisualization.UpdateFunctions.screenResolution))
    //let histogramClient = new Browser(null,AVal.constant System.DateTime.Now,gl.Runtime, true, AVal.constant (V2i(1920, 1080)))
    //let boxPlotClient = new Browser(null,AVal.constant System.DateTime.Now,gl.Runtime, true, AVal.constant (V2i(1920, 1140)))
    let viewTrafos = app.SystemInfo.render.viewTrafos
    let projTrafos = app.SystemInfo.render.projTrafos
    //let hmdLocation = app.SystemInfo.state.display.pose.deviceToWorld



    let histogramOffler = 
        new Offler {
            url = "https://www.google.com/"
            width = 1920
            height = 1080
            incremental = true
        }

    let boxPlotOffler = 
        new Offler {
            url = "https://www.google.com/"
            width = 1920
            height = 1080
            incremental = true
        }

    let controllersOffler = 
        new Offler {
            url = "https://www.google.com/"
            width = 1200    
            height = 790
            incremental = true
        }


    let vrapp = Demo.app controllersOffler histogramOffler boxPlotOffler viewTrafos projTrafos app.Runtime
    let mapp = ComposedApp.start' app true vrapp

    WebPart.startServerLocalhost 4321 [
        MutableApp.toWebPart app.Runtime mapp
        Reflection.assemblyWebPart typeof<AardVolume.EmbeddedRessource>.Assembly
    ] |> ignore

    controllersOffler.StartJavascript (sprintf "win.loadURL(\"%s\");" "http://localhost:4321/?page=controllersPage")

    histogramOffler.StartJavascript (sprintf "win.loadURL(\"%s\");" "http://localhost:4321/?page=histogramPage")

    boxPlotOffler.StartJavascript (sprintf "win.loadURL(\"%s\");" "http://localhost:4321/?page=boxPlotPage")
        
    //histogramOffler.Value.StartJavascript "socket.send(http://localhost:4321/?page=histogramPage);"

    //histogramOffler.contents.Url <- "http://localhost:4321/?page=histogramPage"
    //histogramOffler.contents.StartJavascript "socket.send(http://localhost:4321/?page=histogramPage);"

    //client.LoadUrl "http://localhost:4321/?page=controllersPage" |> ignore
    //histogramClient.LoadUrl "http://localhost:4321/?page=histogramPage" |> ignore
    //boxPlotClient.LoadUrl "http://localhost:4321/?page=boxPlotPage" |> ignore

    //histogramOffler.Dispose()
    
    Aardium.run {
        url "http://localhost:4321/?page=controllersPage"
    }

    Aardvark.Cef.Internal.Cef.shutdown()

    0