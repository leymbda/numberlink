namespace Numberlink.Core

open FSharp.Collections.Graphs

/// A vertex in a puzzle graph, representing a cell.
[<RequireQualifiedAccess>]
[<Struct>]
type PuzzleVertex =
    | Cell
    | Terminal of Line
    | Bridge

/// An edge in a puzzle graph, representing a connection between cells.
[<RequireQualifiedAccess>]
[<Struct>]
type PuzzleEdge =
    | Segment
    | Warp
    | BridgeLane of Lane

/// An Numberlink puzzle.
type Puzzle<'P> = {
    Graph: PropertyGraph<Vertex, PuzzleVertex, Edge, PuzzleEdge>
    Positions: Map<Vertex, 'P>
}
