namespace Numberlink.ZigZag.Generator

open FSharp.Collections.Graphs
open FsToolkit.ErrorHandling
open Numberlink.ZigZag.Core

/// A vertex in a template graph, representing a cell in a level.
[<RequireQualifiedAccess>]
[<Struct>]
type TemplateVertex =
    | Empty
    | Path
    | Terminal of Terminal
    | Bridge
    
/// An edge in a template graph, representing a connection between cells.
[<RequireQualifiedAccess>]
[<Struct>]
type TemplateEdge =
    | Path
    | Warp
    | BridgeLane of BridgeLane
    
/// Constraints that can be applied to a template to guide the generation process.
type TemplateConstraints = {
    AllowTerminalGeneration: bool
    AllowBridgeGeneration: bool
    LineCount: (int * int) option
}

/// A partial template which can be filled in by a generator to create a Zig-Zag Numberlink level.
type Template<'P> = {
    Graph: PropertyGraph<Vertex, TemplateVertex, Edge, TemplateEdge>
    Positions: Map<Vertex, 'P>
    Constraints: TemplateConstraints
}

module Template =
    /// Create an empty template.
    let empty<'P>: Template<'P> = {
        Graph = PropertyGraph.empty
        Positions = Map.empty
        Constraints = {
            AllowTerminalGeneration = true
            AllowBridgeGeneration = false
            LineCount = None
        }
    }

    /// Enable or disable terminal generation.
    let withTerminalGeneration enabled (template: Template<'P>) =
        let constraints = { template.Constraints with AllowTerminalGeneration = enabled }

        { template with Constraints = constraints }

    /// Enable or disable bridge generation.
    let withBridgeGeneration enabled (template: Template<'P>) =
        let constraints = { template.Constraints with AllowBridgeGeneration = enabled }

        { template with Constraints = constraints }

    /// Set the line count range, or None to remove the constraint.
    let withLineCount range (template: Template<'P>) =
        let constraints = { template.Constraints with LineCount = range }

        { template with Constraints = constraints }

    /// Add a vertex with the specified type and position.
    let addVertex vertex kind position (template: Template<'P>) =
        let graph = PropertyGraph.addVertex vertex kind template.Graph
        let positions = Map.add vertex position template.Positions

        { template with Graph = graph; Positions = positions }

    /// Add an edge with the specified type between two vertices.
    let addEdge edge kind vertex1 vertex2 (template: Template<'P>) = result {
        let! graph = PropertyGraph.addEdge edge kind vertex1 vertex2 template.Graph

        return { template with Graph = graph }
    }

    /// Remove a vertex and its associated edges.
    let removeVertex vertex (template: Template<'P>) =
        let graph = PropertyGraph.removeVertex vertex template.Graph
        let positions = Map.remove vertex template.Positions

        { template with Graph = graph; Positions = positions }

    /// Remove an edge.
    let removeEdge edge (template: Template<'P>) =
        let graph = PropertyGraph.removeEdge edge template.Graph
        
        { template with Graph = graph }
    