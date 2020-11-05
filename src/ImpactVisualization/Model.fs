namespace AardVolume.Model

open Aardvark.UI
open Aardvark.UI.Primitives
open FSharp.Data.Adaptive
open Adaptify
open Aardvark.Base
open System.IO

open FSharp.Data.Adaptive

type Values = 
    {
    energies    : float[]
    cubicRoots  : float[]
    strains     : float[]
    alphaJutzis : float[]
    pressures   : float[]
    }

type Frame = 
    {
        positions   : V3f[]
        vertices    : IBuffer
        velocities  : IBuffer
        energies    : IBuffer
        cubicRoots  : IBuffer
        strains     : IBuffer
        alphaJutzis : IBuffer
        pressures   : IBuffer
        values      : Values
    }

type RenderValue = 
    | Energy = 0
    | CubicRoot = 1
    | Strain = 2
    | AlphaJutzi = 3
    | Pressure = 4

type Dim = X | Y | Z

type Value = Min | Max

type Range = 
    {
    min : float
    max : float
    }

type Filters =
    {   
        filterEnergy     : Range
        filterCubicRoot  : Range
        filterStrain     : Range
        filterAlphaJutzi : Range
        filterPressure   : Range    
    }

type DomainRange = 
    {
    x : Range
    y : Range
    z : Range
    }

type ClippingPlane = 
    {
    x : float
    y : float
    z : float
    }

[<ModelType>]
type Model =
    {
        cameraState : CameraControllerState
        frame : int

        pointSize : float
        playAnimation : bool
        animateAllFrames : bool
        renderValue : RenderValue
        colorValue : ColorInput
        colorMaps : HashMap<string, string>
        currentMap : string
        domainRange : DomainRange
        clippingPlane : ClippingPlane
        dataRange : Range
        initDataRange : Range
        discardPoints : bool
        transition : bool

        data : list<float>
        values : float[]

        filter : option<Box3f>
        filtered : list<float> 
        filteredAllFrames : float [] []

        boxColor : C4b

        initFilters : Filters
        currFilters : Filters

        dataPath : string 
    }

module DataLoader = 

    let loadData (runtime : IRuntime) startFrame frames = 

        let heraData = @"..\..\..\data"
    
        let serializer = MBrace.FsPickler.FsPickler.CreateBinarySerializer()
    
        let prepareData (d : DataParser.Parser.Data) : Frame = 
            let frame = {
                vertices    = runtime.PrepareBuffer (ArrayBuffer (Array.map (fun v3 -> V4f(v3,1.0f)) d.vertices)) :> IBuffer
                velocities  = runtime.PrepareBuffer (ArrayBuffer d.velocities) :> IBuffer
                energies    = runtime.PrepareBuffer (ArrayBuffer (Array.map float32 d.internalEnergies)) :> IBuffer
                cubicRoots  = runtime.PrepareBuffer (ArrayBuffer (Array.map float32 d.cubicRootsOfDamage)) :> IBuffer
                strains     = runtime.PrepareBuffer (ArrayBuffer (Array.map float32 d.localStrains)) :> IBuffer
                alphaJutzis = runtime.PrepareBuffer (ArrayBuffer (Array.map float32 d.alphaJutzis)) :> IBuffer
                pressures   = runtime.PrepareBuffer (ArrayBuffer (Array.map float32 d.pressures)) :> IBuffer
                positions   = d.vertices 
                values      = {
                    energies    = d.internalEnergies
                    cubicRoots  = d.cubicRootsOfDamage
                    strains     = d.localStrains
                    alphaJutzis = d.alphaJutzis
                    pressures   = d.pressures
                    }
                }
            frame
    
        let cachedFileEnding = "_cache_2"
   
        let convertToCacheFile (fileName : string) =
            let cacheName = fileName + cachedFileEnding
            if not (File.Exists cacheName) then
                let d,b = DataParser.Parser.parseFile fileName
                let data = serializer.Pickle((d,b))
                File.writeAllBytes cacheName data
        
        let loadDataAndCache (fileName : string) =
            if fileName.EndsWith(cachedFileEnding) then
                let bytes = File.readAllBytes fileName
                let (d : DataParser.Parser.Data, b : Box3f) = serializer.UnPickle(bytes) // exception hier wenn nicht richtiger inhalt... 

                let buffer = prepareData(d)
                let vertexCount = d.vertices.Length

                Some(buffer, b, vertexCount)                 
                
            else
                convertToCacheFile fileName
                None           
        
        let allFiles = 
            Directory.EnumerateFiles heraData |> Seq.toArray 
            |> Array.map loadDataAndCache
            |> Array.choose (fun elem -> elem)

        //TODO: Check for all possible exceptions! -> no data, endFrame > startFrame 
        let framesToAnimate startFrame frames =
            let l = allFiles.Length
            if l = 0 then
                Log.warn "There is no data!"
                Array.empty, Box3f.Invalid, 0
            else
                let mutable endFrame = 0
                if frames > l then endFrame <- l - 1 else endFrame <- frames - 1 //check if out of bounds
                let framesToAnimate = allFiles.[startFrame..endFrame]
                let buffers = framesToAnimate |> Array.map (fun (elem, _, _) ->  elem)
                let box = framesToAnimate.[0] |> (fun (_, b, _) -> b)
                let vertexCount = framesToAnimate.[0] |> (fun (_, _, c) -> c)
                buffers, box, vertexCount     

        framesToAnimate startFrame frames




                

        
