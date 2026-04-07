namespace Numberlink.Core

open FSharp.Collections.Graphs
open FsToolkit.ErrorHandling

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
    | Segment of line: Line
    | Warp of line: Line
    | BridgeLane of line: Line * lane: Lane
    
/// A complete Numberlink solution.
type Solution<'P> = {
    Graph: PropertyGraph<Vertex, SolutionVertex, Edge, SolutionEdge>
    Positions: Map<Vertex, 'P>
}

module Solution =
    /// Convert a puzzle into a solution, returning None if the a solution cannot be mapped from the puzzle and flow.
    let ofPuzzle (flow: Flow) (puzzle: Puzzle<'P>) = option {
        let! edges =
            puzzle.Graph
            |> PropertyGraph.edges
            |> Set.fold (fun acc (e, edata) -> option {
                let! map = acc

                let line = Map.tryFindKey (fun _ -> List.contains e) flow

                let! udata =
                    match edata, line with
                    | PuzzleEdge.Segment, Some line -> Some (SolutionEdge.Segment line)
                    | PuzzleEdge.Segment, None -> Some (SolutionEdge.Unused UnusedEdge.Segment)
                    | PuzzleEdge.Warp, Some line -> Some (SolutionEdge.Warp line)
                    | PuzzleEdge.Warp, None -> Some (SolutionEdge.Unused UnusedEdge.Warp)
                    | PuzzleEdge.BridgeLane lane, Some line -> Some (SolutionEdge.BridgeLane (line, lane))
                    | PuzzleEdge.BridgeLane _, None -> None

                return Map.add e udata map
            }) (Some Map.empty)

        return {
            Solution.Graph =
                puzzle.Graph
                |> PropertyGraph.mapVertices (fun _ vdata ->
                    match vdata with
                    | PuzzleVertex.Cell -> SolutionVertex.Cell
                    | PuzzleVertex.Terminal _ -> SolutionVertex.Terminal
                    | PuzzleVertex.Bridge -> SolutionVertex.Bridge
                )
                |> PropertyGraph.mapEdges (fun e _ -> Map.find e edges)
            Positions = puzzle.Positions
        }
    }

    /// Convert a solution back into a puzzle, returning None if the solution is unexpectedly invalid.
    let toPuzzle (solution: Solution<'P>) = option {
        let! vertices =
            solution.Graph
            |> PropertyGraph.vertices
            |> Set.fold (fun acc (v, vdata) -> option {
                let! map = acc

                let! line =
                    solution.Graph
                    |> PropertyGraph.incidentEdges v
                    |> Option.bind (Seq.tryPick (fun (_, edata) ->
                        match edata with
                        | SolutionEdge.Segment line -> Some line
                        | SolutionEdge.Warp line -> Some line
                        | SolutionEdge.BridgeLane (line, _) -> Some line
                        | _ -> None
                    ))

                let udata =
                    match vdata with
                    | SolutionVertex.Cell -> PuzzleVertex.Cell
                    | SolutionVertex.Terminal -> PuzzleVertex.Terminal line
                    | SolutionVertex.Bridge -> PuzzleVertex.Bridge

                return Map.add v udata map
            }) (Some Map.empty)

        return {
            Puzzle.Graph =
                solution.Graph
                |> PropertyGraph.mapVertices (fun v _ -> Map.find v vertices)
                |> PropertyGraph.mapEdges (fun _ edata ->
                    match edata with
                    | SolutionEdge.Segment _ -> PuzzleEdge.Segment
                    | SolutionEdge.Unused UnusedEdge.Segment -> PuzzleEdge.Segment
                    | SolutionEdge.Warp _ -> PuzzleEdge.Warp
                    | SolutionEdge.Unused UnusedEdge.Warp -> PuzzleEdge.Warp
                    | SolutionEdge.BridgeLane (_, lane) -> PuzzleEdge.BridgeLane lane
                )
            Positions = solution.Positions
        }
    }
