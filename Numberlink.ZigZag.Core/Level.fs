namespace Numberlink.ZigZag.Core

open Numberlink.ZigZag.Core.Lib
open System

[<RequireQualifiedAccess>]
type LevelVertex =
    | Terminal of edge: Guid
    | Path of edges: Guid * Guid
    | Bridge of edges: (Guid * Guid) list

module LevelVertex =
    /// Find the opposing edge for the given vertex and edge ID, if it exists.
    let tryTraverse edgeId vertex =
        match vertex with
        | LevelVertex.Terminal _ ->
            None

        | LevelVertex.Path (e1, e2) ->
            match e1, e2 with
            | e1, _ when e1 = edgeId -> Some e2
            | _, e2 when e2 = edgeId -> Some e1
            | _ -> None

        | LevelVertex.Bridge edges ->
            edges
            |> List.collect (fun (e1, e2) ->
                match e1, e2 with
                | e1, _ when e1 = edgeId -> [e2]
                | _, e2 when e2 = edgeId -> [e1]
                | _ -> []
            )
            |> List.tryHead

        // TODO: Should this be a function on the graph?

[<RequireQualifiedAccess>]
type LevelEdge =
    | Path
    | Warp

type Level<'P> = {
    Id: Guid
    Graph: Graph<LevelVertex, LevelEdge>
    Positions: Map<Guid, 'P>
}

module Level =
    /// Create a level.
    let create levelId graph positions =
        { Id = levelId; Graph = graph; Positions = positions }

    /// Get the paths of the level.
    let getPaths (level: Level<'P>) =
        let rec traverse visited edgeId =
            let head = List.head visited

            let next =
                level.Graph
                |> Graph.getNeighbors head
                |> Seq.filter (fun rel -> rel.EdgeId = edgeId)
                |> Seq.tryHead

        let rec loop visited paths =
            let init =
                level.Graph.Vertices
                |> Map.tryFindKey (fun vertexId -> function
                    | LevelVertex.Terminal _ -> not (List.contains vertexId visited)
                    | _ -> false
                )

            match init with
            | None -> paths
            | Some init ->
                let nextEdgeId =
                    level.Graph
                    |> Graph.getVertex init
                    |> Option.bind (LevelVertex.tryTraverse edgeId)


        loop [] []