open Aardvark.Base
open Aardvark.UI
open Aardvark.Vr
open Aardium
open ImpactVisualization
open Suave

[<EntryPoint>]
let main argv =
    Aardvark.Init()
    Aardium.init()

    let app = VRApplication.create (VRDisplay.OpenVR 1.0) 8 false
    let bla = Demo.app app.Runtime
    let mapp = ComposedApp.start' app true bla
    
    WebPart.startServerLocalhost 4321 [
        MutableApp.toWebPart app.Runtime mapp
        Reflection.assemblyWebPart typeof<AardVolume.EmbeddedRessource>.Assembly
    ] |> ignore
    
    Aardium.run {
        url "http://localhost:4321"
    }

    0