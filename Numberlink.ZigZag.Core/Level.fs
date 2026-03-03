namespace Numberlink.ZigZag.Core

open FSharp.Collections.Graphs

/// A vertex in a level graph, representing a cell.
[<RequireQualifiedAccess>]
[<Struct>]
type LevelVertex =
    | Path
    | Terminal of Terminal
    | Bridge
    
/// An edge in a level graph, representing a connection between cells.
[<RequireQualifiedAccess>]
[<Struct>]
type LevelEdge =
    | Path
    | Warp
    | BridgeLane of BridgeLane
    
/// A complete Zig-Zag Numberlink level.
type Level<'P> = {
    Graph: PropertyGraph<Vertex, LevelVertex, Edge, LevelEdge>
    Positions: Map<Vertex, 'P>
}
