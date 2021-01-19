namespace ImpactVisualization

open Aardvark.Base
open Aardvark.Base.Rendering
open FSharp.Data.Adaptive
open Aardvark.SceneGraph
open Aardvark.Application
open System.Runtime.InteropServices
 
#nowarn "9"
 
 
open Aardvark.Rendering.Text
 
open Aardvark.Base.Ag
open Aardvark.SceneGraph
 
type RuntimeApplicator(r : IRuntime, child : ISg) =
    inherit Sg.AbstractApplicator(child)
    member x.Runtime = r
 
[<Rule>]
type RuntimeSem() =
    member x.Runtime(w : RuntimeApplicator, scope : Ag.Scope) = w.Child?Runtime <- scope.Runtime
 
[<AutoOpen>]
module Halper =
    module Sg = 
        let applyRuntime (r : IRuntime) (sg : ISg) = RuntimeApplicator(r,sg) :> ISg
 
 
type MyBillboardApplicator(child : aval<ISg>, viewTrafo : aval<Trafo3d>) =
      inherit Sg.AbstractApplicator(child)
      member x.ViewTrafo = viewTrafo
 
 
module BillboardExtensions =
    open Aardvark.SceneGraph.Semantics
 
    [<Rule>]
    type ShapeSemExt() =
        static let defaultDepthBias = 1.0 / float (1 <<< 21)
 
        member x.ModelTrafoStack(b : MyBillboardApplicator, scope : Ag.Scope) =
            let trafo =
                b.ViewTrafo
                    |> AVal.map (fun view ->
                        let pos = view.Forward.TransformPosProj V3d.Zero
                        Trafo3d.Translation(pos) * view.Inverse
                    )
 
 
            b.Child?ModelTrafoStack <- trafo::scope.ModelTrafoStack
 
module Sg = 
    let myBillboard (viewTrafo : aval<Trafo3d>) (s : ISg)  = 
        MyBillboardApplicator(AVal.constant s, viewTrafo) :> ISg
