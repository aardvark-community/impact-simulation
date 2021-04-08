namespace AardVolume.Model

open Aardvark.UI
open Aardvark.UI.Primitives
open FSharp.Data.Adaptive
open Adaptify
open Aardvark.Base
open System.IO

open Hera
open FSharp.Data.Adaptive
open Aardvark.Data.Points
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

type PreparedFrame = 
    {
        positionsBuffer   : IBuffer
        normalsBuffer     : IBuffer
        velocitiesBuffer  : IBuffer
        massesBuffer      : IBuffer
        densitiesBuffer   : IBuffer
        energiesBuffer    : IBuffer
        cubicRootsBuffer  : IBuffer
        strainsBuffer     : IBuffer
        alphaJutzisBuffer : IBuffer
        pressuresBuffer   : IBuffer
    }

type Frame = 
    {
        pointSet      : PointSet
        positions     : V3f[]
        normals       : V3f[]
        velocities    : V3f[]
        masses        : float[]
        densities     : float[]
        energies      : float[]
        cubicRoots    : float[]
        strains       : float[]
        alphaJutzis   : float[]
        pressures     : float[]
        preparedFrame : PreparedFrame
    }

type RenderValue = 
    | Energy = 0
    | CubicRoot = 1
    | Strain = 2
    | AlphaJutzi = 3
    | Pressure = 4
    | Mass = 5
    | Density = 6
    | NoValue = 7

type DiscardProperties =
    {
        plane           : V3d
        controllerPlane : Plane3d
    }
    

//type FilterQuery<'a> = 'a -> Frame -> GenericChunk[]
//type AppliedFilterQuery = Frame -> GenericChunk[]

//type FilterType = 
//    | Box of Box3d
//    | Sphere of Sphere3d

//type FilterProbeGeneric<'a> = 
//    {
//        filterFunc : FilterQuery<'a>
//        probe : 'a
//    }

//type FilterProbe = FilterProbeGeneric<FilterType>

type Probe = 
    | SphereProbe of sphere : Sphere3d
    | CubeProbe of cube : Box3f

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

type Statistics = 
    {
        attribute         : RenderValue
        numOfPoints       : int 
        maxValue          : float 
        minValue          : float
        meanValue         : float
        variance          : float
        standardDeviation : float
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
        currHeraBBox : Box3d

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

        attributeText : string

        normalizeData : bool

        enableShading : bool
        reconstructNormal : bool 
        reconstructDepth : bool 

        lowerOutliers : bool
        higherOutliers : bool
        outliersRange : Range

        data : VersionedArray
        values : VersionedArray
        boxPlotData : VersionedArray
        boxPlotData1 : float [] []

        currFilter : option<Probe>
        boxFilter : option<Box3f>
        sphereFilter : option<Sphere3d>
        filtered : list<float> 
        filteredAllFrames : float [] []

        boxColor : C4b

        initFilters : Filters
        currFilters : Filters

        dataPath : string 
    }

module DataLoader = 
    let loadOctreeFromStore storepath =
        let id = storepath + ".key" |> File.readAllText
        let store = (new SimpleDiskStore(storepath)).ToPointCloudStore()
        let pointcloud = store.GetPointSet(id)
        (pointcloud, store)

    let collectLeafData (extract : IPointCloudNode -> 'a[]) (root : IPointCloudNode) : 'a[] =
        root.EnumerateNodes () |> Seq.filter (fun n -> n.IsLeaf) |> Seq.map extract |> Array.concat

    let datapath  = @"C:\Users\vasileva\source\hera_data"

    //let datapath  = @"D:\TU Wien\Master\4. Semester\Praktikum aus Visual Computing\Data\r80_p0_m500_v6000_mbasalt_a1.0_1M\data"

    let createData() =

        let datafile  = @"C:\Users\vasileva\source\hera_data\impact.0117"
        let storepath = datafile + ".store"

        let id = Hera.Hera.importHeraDataIntoStore datafile storepath false
        printfn "%s" id

    let loadDataSingleFrame (runtime : IRuntime) (storepath : string) = 
        //let storepath = datapath + ".store"

        let (p, store) = loadOctreeFromStore storepath
        let bb = p.Root.Value.BoundingBoxExactGlobal
        let root = p.Root.Value

        let positions   = root |> collectLeafData (fun n -> n.PositionsAbsolute |> Array.map V3f)
        let normals     = root |> collectLeafData (fun n -> n.Normals.Value)
        let velocities  = root |> collectLeafData (fun n -> n.Properties.[Hera.Defs.Velocities] :?> V3f[])
        let masses      = root |> collectLeafData (fun n -> n.Properties.[Hera.Defs.Masses] :?> float32[]) 
        let densities   = root |> collectLeafData (fun n -> n.Properties.[Hera.Defs.Densities] :?> float32[]) 
        let energies    = root |> collectLeafData (fun n -> n.Properties.[Hera.Defs.InternalEnergies] :?> float32[]) 
        let cubicRoots  = root |> collectLeafData (fun n -> n.Properties.[Hera.Defs.CubicRootsOfDamage] :?> float32[])
        let strains     = root |> collectLeafData (fun n -> n.Properties.[Hera.Defs.LocalStrains] :?> float32[])
        let alphaJutzis = root |> collectLeafData (fun n -> n.Properties.[Hera.Defs.AlphaJutzi] :?> float32[])
        let pressures   = root |> collectLeafData (fun n -> n.Properties.[Hera.Defs.Pressures] :?> float32[])

        let frame = 
            { 
                pointSet      = p
                positions     = positions
                normals       = normals
                velocities    = velocities
                masses        = masses |> Array.map (fun v -> float v)
                densities     = densities |> Array.map (fun v -> float v)
                energies      = energies |> Array.map (fun v -> float v)
                cubicRoots    = cubicRoots |> Array.map (fun v -> float v)
                strains       = strains |> Array.map (fun v -> float v)
                alphaJutzis   = alphaJutzis |> Array.map (fun v -> float v)
                pressures     = pressures |> Array.map (fun v -> float v)
                preparedFrame = 
                    {
                        positionsBuffer   = runtime.PrepareBuffer (ArrayBuffer (Array.map (fun v3 -> V4f(v3,1.0f)) positions)) :> IBuffer
                        normalsBuffer     = runtime.PrepareBuffer (ArrayBuffer normals) :> IBuffer
                        velocitiesBuffer  = runtime.PrepareBuffer (ArrayBuffer velocities) :> IBuffer
                        massesBuffer      = runtime.PrepareBuffer (ArrayBuffer masses) :> IBuffer
                        densitiesBuffer   = runtime.PrepareBuffer (ArrayBuffer densities) :> IBuffer
                        energiesBuffer    = runtime.PrepareBuffer (ArrayBuffer energies) :> IBuffer
                        cubicRootsBuffer  = runtime.PrepareBuffer (ArrayBuffer cubicRoots) :> IBuffer
                        strainsBuffer     = runtime.PrepareBuffer (ArrayBuffer strains) :> IBuffer
                        alphaJutzisBuffer = runtime.PrepareBuffer (ArrayBuffer alphaJutzis) :> IBuffer
                        pressuresBuffer   = runtime.PrepareBuffer (ArrayBuffer pressures) :> IBuffer
                    }
            }
        frame

    let files = 
        let dataFiles = Directory.EnumerateDirectories datapath
        if dataFiles.IsEmpty() then
            failwith "There is no data in the specified path!!"
        else
            Directory.EnumerateDirectories datapath |> Seq.toArray |> Array.filter (fun file -> file.EndsWith(".store"))

    let loadDataAllFrames (runtime : IRuntime) = files |> Array.map (fun file -> loadDataSingleFrame runtime file)