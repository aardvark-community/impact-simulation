namespace AardVolume.Model

open Aardvark.UI
open Aardvark.UI.Primitives
open FSharp.Data.Adaptive
open Adaptify
open Aardvark.Base
open System.IO

open Hera
open FSharp.Data.Adaptive
open Aardvark.Geometry.Points
open Uncodium.SimpleStore

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

type Frame1 = 
    {
        pointSet    : PointSet
        positions   : V3f[]
        normals     : V3f[]
        velocities  : V3f[]
        energies    : float[]
        cubicRoots  : float[]
        strains     : float[]
        alphaJutzis : float[]
        pressures   : float[]
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

[<ModelType;CustomEquality;NoComparison>]
type VersionedArray = 
    {
        version : int
        arr : float[]
    } with
        override x.Equals(other : obj) =
            match other with
            | :? VersionedArray as a -> a.version = x.version && System.Object.ReferenceEquals(a,x.arr)
            | _ -> false
        override x.GetHashCode() = Unchecked.hash x.arr

module VersionedArray =
    let mutable private v = 0
    let ofArray xs = v <- v + 1; { version = v; arr = xs }

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

        data : VersionedArray
        values : VersionedArray

        filter : option<Box3f>
        filtered : list<float> 
        filteredAllFrames : float [] []

        boxColor : C4b

        initFilters : Filters
        currFilters : Filters

        dataPath : string 
    }

module NewDataLoader = 
    let loadOctreeFromStore storepath =
        let id = storepath + ".key" |> File.readAllText
        let store = (new SimpleDiskStore(storepath)).ToPointCloudStore()
        let pointcloud = store.GetPointSet(id)
        (pointcloud, store)

    let collectLeafData (extract : IPointCloudNode -> 'a[]) (root : IPointCloudNode) : 'a[] =
        root.EnumerateNodes () |> Seq.filter (fun n -> n.IsLeaf) |> Seq.map extract |> Array.concat

    let datapath  = @"C:\Users\vasileva\source\hera_data"

    let createData() =

        let datafile  = @"C:\Users\vasileva\source\hera_data\impact.0117"
        let storepath = datafile + ".store"

        let id = Hera.Hera.importHeraDataIntoStore datafile storepath false
        printfn "%s" id

    let loadDataSingleFrame storepath = 
        //let storepath = datapath + ".store"

        let (p, store) = loadOctreeFromStore storepath
        let bb = p.Root.Value.BoundingBoxExactGlobal
        let root = p.Root.Value

        let frame = 
            { 
                pointSet    = p
                positions   = root |> collectLeafData (fun n -> n.PositionsAbsolute |> Array.map V3f)
                normals     = root |> collectLeafData (fun n -> n.Normals.Value)
                velocities  = root |> collectLeafData (fun n -> n.Properties.[Hera.Defs.Velocities] :?> V3f[])
                energies    = root |> collectLeafData (fun n -> n.Properties.[Hera.Defs.InternalEnergies] :?> float32[]) |> Array.map (fun v -> float v)
                cubicRoots  = root |> collectLeafData (fun n -> n.Properties.[Hera.Defs.CubicRootsOfDamage] :?> float32[]) |> Array.map (fun v -> float v)
                strains     = root |> collectLeafData (fun n -> n.Properties.[Hera.Defs.LocalStrains] :?> float32[]) |> Array.map (fun v -> float v)
                alphaJutzis = root |> collectLeafData (fun n -> n.Properties.[Hera.Defs.AlphaJutzi] :?> float32[]) |> Array.map (fun v -> float v)
                pressures   = root |> collectLeafData (fun n -> n.Properties.[Hera.Defs.Pressures] :?> float32[]) |> Array.map (fun v -> float v)
            }
        frame

    let files = Directory.EnumerateDirectories datapath |> Seq.toArray |> Array.filter (fun file -> file.EndsWith(".store"))

    let loadDataAllFrames = files |> Array.map (fun file -> loadDataSingleFrame file)




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




                

        
