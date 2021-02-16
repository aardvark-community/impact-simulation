open Aardvark.Base
open Aardvark.UI
open Aardvark.Vr
open Aardium
open ImpactVisualization
open Suave

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

    let app = VRApplication.create' (VRDisplay.OpenVR 1.0) Aardvark.Application.Backend.GL 8 false
    let client = new Browser(null,AVal.constant System.DateTime.Now,app.Runtime, true, AVal.constant (ImpactVisualization.AppUpdate.screenResolution))
    let viewTrafos = app.SystemInfo.render.viewTrafos
    let bla = Demo.app client viewTrafos app.Runtime
    let mapp = ComposedApp.start' app true bla
    
    WebPart.startServerLocalhost 4321 [
        MutableApp.toWebPart app.Runtime mapp
        Reflection.assemblyWebPart typeof<AardVolume.EmbeddedRessource>.Assembly
    ] |> ignore
    
    let res = client.LoadUrl "http://localhost:4321/"

    Aardium.run {
        url "http://localhost:4321"
    }

    Aardvark.Cef.Internal.Cef.shutdown()

    0