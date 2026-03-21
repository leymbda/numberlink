namespace FSharp.Collections.Graphs

open FsToolkit.ErrorHandling

type Graph<'v, 'e when 'v : comparison and 'e : comparison> = {
    Vertices: Map<'v, Set<'e>>
    Edges: Map<'e, 'v * 'v>
}

module Graph =
    /// Create an empty graph.
    let empty<'v, 'e when 'v : comparison and 'e : comparison>: Graph<'v, 'e> = {
        Vertices = Map.empty
        Edges = Map.empty
    }
    
    /// Check if the graph is empty (i.e. has no vertices).
    let isEmpty (graph: Graph<'v, 'e>) =
        Map.isEmpty graph.Vertices

    /// Get the edges incident to a vertex, returning None if the vertex is not found.
    let incidentEdges vertex (graph: Graph<'v, 'e>) =
        Map.tryFind vertex graph.Vertices

    /// Get edge/neighbor pairs for all edges incident to a vertex, returning None if the vertex is not found.
    let adjacency vertex (graph: Graph<'v, 'e>) =
        graph
        |> incidentEdges vertex
        |> Option.map (Set.map (fun edge ->
            let v1, v2 = Map.find edge graph.Edges
            edge, if v1 = vertex then v2 else v1
        ))

    /// Get the neighboring vertices of a vertex, returning None if the vertex is not found.
    let neighbors vertex (graph: Graph<'v, 'e>) =
        graph
        |> incidentEdges vertex
        |> Option.map (Set.map (fun edge ->
            let v1, v2 = Map.find edge graph.Edges
            if v1 = vertex then v2 else v1
        ))

    /// Get the degree of a vertex, returning None if the vertex is not found.
    let degree vertex (graph: Graph<'v, 'e>) =
        graph
        |> incidentEdges vertex
        |> Option.map Set.count

    /// Check if a vertex exists in the graph.
    let containsVertex vertex (graph: Graph<'v, 'e>) =
        Map.containsKey vertex graph.Vertices

    /// Check if an edge exists in the graph.
    let containsEdge edge (graph: Graph<'v, 'e>) =
        Map.containsKey edge graph.Edges

    /// Get the endpoints of an edge, returning None if the edge is not found.
    let getEndpoints edge (graph: Graph<'v, 'e>) =
        Map.tryFind edge graph.Edges

    /// Check if two vertices are adjacent, returning None if either vertex is not found.
    let isAdjacent vertex1 vertex2 (graph: Graph<'v, 'e>) =
        graph
        |> neighbors vertex1
        |> Option.map (Set.contains vertex2)

    /// Get the edges connecting two adjacent vertices, returning None if either vertex is not found.
    let findEdges vertex1 vertex2 (graph: Graph<'v, 'e>) = option {
        do! containsVertex vertex1 graph |> Result.requireTrue () |> Option.ofResult
        do! containsVertex vertex2 graph |> Result.requireTrue () |> Option.ofResult

        return!
            graph
            |> adjacency vertex1
            |> Option.map (Set.filter (fun (_, v) -> v = vertex2))
            |> Option.map (Set.map fst)
    }
    
    /// Add a vertex to the graph, doing nothing if it already exists.
    let addVertex vertex (graph: Graph<'v, 'e>) =
        match containsVertex vertex graph with
        | true -> graph
        | false ->
            let vertices = Map.add vertex Set.empty graph.Vertices

            { graph with Vertices = vertices }

    /// Add an edge to the graph, returning an error containing the missing vertices.
    let addEdge edge vertex1 vertex2 (graph: Graph<'v, 'e>) = result {
        let undefinedVertices = List.filter (fun v -> not <| Map.containsKey v graph.Vertices) [vertex1; vertex2]
        do! Result.requireEmpty (Set.ofList undefinedVertices) undefinedVertices

        let vertices =
            graph.Vertices
            |> Map.add vertex1 (Set.add edge (Map.find vertex1 graph.Vertices))
            |> Map.add vertex2 (Set.add edge (Map.find vertex2 graph.Vertices))

        let edges = Map.add edge (vertex1, vertex2) graph.Edges

        return { graph with Vertices = vertices; Edges = edges }
    }

    /// Remove a vertex and all its associated edges from the graph, doing nothing if it doesn't exist.
    let removeVertex vertex (graph: Graph<'v, 'e>) =
        match incidentEdges vertex graph with
        | None -> graph
        | Some edges ->
            let vertices =
                graph.Vertices
                |> Map.remove vertex
                |> Map.map (fun _ -> Set.filter (fun edge -> not <| Set.contains edge edges))

            let edges = Map.filter (fun edge _ -> not <| Set.contains edge edges) graph.Edges

            { graph with Vertices = vertices; Edges = edges }

    /// Remove an edge from the graph, doing nothing if it doesn't exist.
    let removeEdge edge (graph: Graph<'v, 'e>) =
        let vertices = Map.map (fun _ edges -> Set.remove edge edges) graph.Vertices
        let edges = Map.remove edge graph.Edges

        { graph with Vertices = vertices; Edges = edges }

    /// Get the vertices of the graph.
    let vertices (graph: Graph<'v, 'e>) =
        graph.Vertices
        |> Map.keys
        |> Set.ofSeq

    /// Get the edges of the graph.
    let edges (graph: Graph<'v, 'e>) =
        graph.Edges
        |> Map.keys
        |> Set.ofSeq

    /// Get the total number of vertices in the graph.
    let countVertices (graph: Graph<'v, 'e>) =
        Map.count graph.Vertices

    /// Get the total number of edges in the graph.
    let countEdges (graph: Graph<'v, 'e>) =
        Map.count graph.Edges

    /// Fold over the vertices of the graph.
    let inline foldVertices folder state (graph: Graph<'v, 'e>) =
        Map.fold (fun acc vertex _ -> folder acc vertex) state graph.Vertices

    /// Fold over the edges of the graph.
    let inline foldEdges folder state (graph: Graph<'v, 'e>) =
        Map.fold (fun acc edge _ -> folder acc edge) state graph.Edges

    /// Iterate over the vertices of the graph.
    let inline iterVertices action (graph: Graph<'v, 'e>) =
        Map.iter (fun vertex _ -> action vertex) graph.Vertices

    /// Iterate over the edges of the graph.
    let inline iterEdges action (graph: Graph<'v, 'e>) =
        Map.iter (fun edge _ -> action edge) graph.Edges

    /// Check if any vertex satisfies a predicate.
    let inline existsVertex predicate (graph: Graph<'v, 'e>) =
        Map.exists (fun vertex _ -> predicate vertex) graph.Vertices

    /// Check if any edge satisfies a predicate.
    let inline existsEdge predicate (graph: Graph<'v, 'e>) =
        Map.exists (fun edge _ -> predicate edge) graph.Edges

    /// Check if all vertices satisfy a predicate.
    let inline forallVertices predicate (graph: Graph<'v, 'e>) =
        Map.forall (fun vertex _ -> predicate vertex) graph.Vertices

    /// Check if all edges satisfy a predicate.
    let inline forallEdges predicate (graph: Graph<'v, 'e>) =
        Map.forall (fun edge _ -> predicate edge) graph.Edges

    /// Filter the vertices of the graph, removing those that do not satisfy a predicate (and therefore any connected
    /// edges).
    let inline filterVertices predicate (graph: Graph<'v, 'e>) =
        graph.Vertices
        |> Map.keys
        |> Seq.filter (fun vertex -> not <| predicate vertex)
        |> Seq.fold (fun acc vertex -> removeVertex vertex acc) graph

    /// Filter the edges of the graph, removing those that do not satisfy a predicate.
    let inline filterEdges predicate (graph: Graph<'v, 'e>) =
        graph.Edges
        |> Map.keys
        |> Seq.filter (fun edge -> not <| predicate edge)
        |> Seq.fold (fun acc edge -> removeEdge edge acc) graph

    /// Create the subgraph induced by a set of vertices, containing only those vertices and the edges whose both
    /// endpoints are within the set.
    let inducedSubgraph (vertexSet: Set<'v>) (graph: Graph<'v, 'e>) =
        let verticesToRemove = Set.filter (fun vertex -> not <| Set.contains vertex vertexSet) (vertices graph)
        Set.fold (fun acc vertex -> removeVertex vertex acc) graph verticesToRemove

    /// Breadth-first search starting from a vertex, returning vertices in BFS order. The predicate takes (in order of
    /// curried argument) the current vertex, the edge being traversed, and the neighboring vertex, and will skip any
    /// neighbor for which the predicate returns false.
    let inline bfs start predicate (graph: Graph<'v, 'e>) =
        let rec loop queue seen = seq {
            match queue with
            | [] -> ()
            | vertex :: rest ->
                yield vertex

                let newNeighbors =
                    graph
                    |> adjacency vertex
                    |> Option.defaultValue Set.empty
                    |> Set.fold (fun acc (edge, nv) ->
                        match not (Set.contains nv seen) && predicate vertex edge nv with
                        | true -> nv :: acc
                        | false -> acc
                    ) []

                let seen = List.fold (fun s nv -> Set.add nv s) seen newNeighbors
                yield! loop (rest @ newNeighbors) seen
        }
        
        match containsVertex start graph with
        | true -> loop [start] (Set.singleton start)
        | false -> Seq.empty

    /// Check if there is a path between two vertices, returning false if either vertex is not found. The predicate
    /// filters the search to support different notions of connectivity.
    let inline isConnected vertex1 vertex2 predicate (graph: Graph<'v, 'e>) =
        graph
        |> bfs vertex1 predicate
        |> Seq.contains vertex2
