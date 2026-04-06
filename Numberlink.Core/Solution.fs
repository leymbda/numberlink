namespace Numberlink.Core

open FSharp.Collections.Graphs

/// A vertex in a solution graph, representing a cell.
[<RequireQualifiedAccess>]
[<Struct>]
type SolutionVertex =
    | Cell
    | Terminal
    | Bridge

/// An unused edge in a solution graph, representing a possible but unused connection between vertices.
[<RequireQualifiedAccess>]
[<Struct>]
type UnusedEdge =
    | Segment
    | Warp
    
/// An edge in a solution graph, representing a connection between cells.
[<RequireQualifiedAccess>]
[<Struct>]
type SolutionEdge =
    | Unused of kind: UnusedEdge
    | Segment of Line
    | Warp of Line
    | BridgeLane of Line
    
/// A complete Numberlink solution.
type Solution<'P> = {
    Graph: PropertyGraph<Vertex, SolutionVertex, Edge, SolutionEdge>
    Positions: Map<Vertex, 'P>
}

module Solution =
    let ofPuzzle (flow: Flow) (puzzle: Puzzle<'P>): Solution<'P> =
        raise (System.NotImplementedException())

    let toPuzzle (solution: Solution<'P>): Puzzle<'P> =
        raise (System.NotImplementedException())

    // TODO: Implement above
