namespace Numberlink.ZigZag.Core.Lib

open FsToolkit.ErrorHandling

type PropertyGraph<'v, 'vdata, 'e, 'edata when 'v : comparison and 'e : comparison> = {
    Graph: Graph<'v, 'e>
    Vertices: Map<'v, 'vdata>
    Edges: Map<'e, 'edata>
}

module PropertyGraph =
    /// Create an empty property graph.
    let empty<'v, 'vdata, 'e, 'edata when 'v : comparison and 'e : comparison>: PropertyGraph<'v, 'vdata, 'e, 'edata> = {
        Graph = Graph.empty
        Vertices = Map.empty
        Edges = Map.empty
    }

    /// Check if the property graph is empty (i.e. has no vertices).
    let isEmpty (pg: PropertyGraph<'v, 'vdata, 'e, 'edata>) =
        Graph.isEmpty pg.Graph

    /// Get the edges incident to a vertex, returning None if the vertex is not found.
    let getIncidentEdges vertex (pg: PropertyGraph<'v, 'vdata, 'e, 'edata>) =
        pg.Graph
        |> Graph.getIncidentEdges vertex
        |> Option.map (Set.map (fun edge -> edge, Map.find edge pg.Edges))
        
    /// Get edge/neighbor pairs for all edges incident to a vertex, returning an empty set if the vertex is not found.
    let incidentNeighbors vertex (pg: PropertyGraph<'v, 'vdata, 'e, 'edata>) =
        pg
        |> getIncidentEdges vertex
        |> Option.defaultValue Set.empty
        |> Set.map (fun (e, edata) ->
            let (v1, v2) = Map.find e pg.Graph.Edges
            let v = if v1 = vertex then v2 else v1
            let vdata = Map.find v pg.Vertices

            e, edata, v, vdata
        )

    /// Get the neighboring vertices of a vertex, returning None if the vertex is not found.
    let neighbors vertex (pg: PropertyGraph<'v, 'vdata, 'e, 'edata>) =
        pg.Graph
        |> Graph.neighbors vertex
        |> Option.map (Set.map (fun neighbor -> neighbor, Map.find neighbor pg.Vertices))
        
    /// Get the degree of a vertex, returning None if the vertex is not found.
    let degree vertex (pg: PropertyGraph<'v, 'vdata, 'e, 'edata>) =
        Graph.degree vertex pg.Graph
        
    /// Check if a vertex exists in the graph.
    let containsVertex vertex (pg: PropertyGraph<'v, 'vdata, 'e, 'edata>) =
        Graph.containsVertex vertex pg.Graph
        
    /// Check if an edge exists in the graph.
    let containsEdge edge (pg: PropertyGraph<'v, 'vdata, 'e, 'edata>) =
        Graph.containsEdge edge pg.Graph

    /// Get the endpoints of an edge, returning None if the edge is not found.
    let getEndpoints edge (pg: PropertyGraph<'v, 'vdata, 'e, 'edata>) =
        pg.Graph
        |> Graph.getEndpoints edge
        |> Option.map (fun (v1, v2) -> (v1, Map.find v1 pg.Vertices), (v2, Map.find v2 pg.Vertices))
        
    /// Check if two vertices are adjacent, returning false if either vertex is not found.
    let isAdjacent vertex1 vertex2 (pg: PropertyGraph<'v, 'vdata, 'e, 'edata>) =
        Graph.isAdjacent vertex1 vertex2 pg.Graph
        
    /// Get the edges connecting two adjacent vertices, returning an empty set if either vertex does not exist or they
    /// have no connecting edges.
    let findEdges vertex1 vertex2 (pg: PropertyGraph<'v, 'vdata, 'e, 'edata>) =
        pg.Graph
        |> Graph.findEdges vertex1 vertex2
        |> Set.map (fun edge -> edge, Map.find edge pg.Edges)

    /// Add a vertex to the property graph, doing nothing if it already exists.
    let addVertex vertex data (pg: PropertyGraph<'v, 'vdata, 'e, 'edata>) =
        match containsVertex vertex pg with
        | true -> pg
        | false ->
            let graph = Graph.addVertex vertex pg.Graph
            let vertices = Map.add vertex data pg.Vertices

            { pg with Graph = graph; Vertices = vertices }
        
    /// Add an edge to the property graph, returning an error containing the missing vertices.
    let addEdge edge data vertex1 vertex2 (pg: PropertyGraph<'v, 'vdata, 'e, 'edata>) = result {
        let! graph = Graph.addEdge edge vertex1 vertex2 pg.Graph
        let edges = Map.add edge data pg.Edges

        return { pg with Graph = graph; Edges = edges }
    }
    
    /// Remove a vertex and all its associated edges from the property graph, doing nothing if it doesn't exist.
    let removeVertex vertex (pg: PropertyGraph<'v, 'vdata, 'e, 'edata>) =
        match Graph.getIncidentEdges vertex pg.Graph with
        | None -> pg
        | Some incidentEdges ->
            let graph = Graph.removeVertex vertex pg.Graph
            let vertices = Map.remove vertex pg.Vertices
            let edges = Set.fold (fun acc edge -> Map.remove edge acc) pg.Edges incidentEdges

            { pg with Graph = graph; Vertices = vertices; Edges = edges; }
        
    /// Remove an edge from the property graph.
    let removeEdge edge (pg: PropertyGraph<'v, 'vdata, 'e, 'edata>) =
        let graph = Graph.removeEdge edge pg.Graph
        let edges = Map.remove edge pg.Edges

        { pg with Graph = graph; Edges = edges }
        
    /// Get the vertices of the property graph.
    let vertices (pg: PropertyGraph<'v, 'vdata, 'e, 'edata>) =
        pg.Graph
        |> Graph.vertices
        |> Set.map (fun vertex -> vertex, Map.find vertex pg.Vertices)
        
    /// Get the edges of the property graph.
    let edges (pg: PropertyGraph<'v, 'vdata, 'e, 'edata>) =
        pg.Graph
        |> Graph.edges
        |> Set.map (fun edge -> edge, Map.find edge pg.Edges)
        
    /// Get the total number of vertices in the property graph.
    let countVertices (pg: PropertyGraph<'v, 'vdata, 'e, 'edata>) =
        Graph.countVertices pg.Graph
        
    /// Get the total number of edges in the property graph.
    let countEdges (pg: PropertyGraph<'v, 'vdata, 'e, 'edata>) =
        Graph.countEdges pg.Graph

    /// Fold over the vertices of the property graph.
    let foldVertices folder state (pg: PropertyGraph<'v, 'vdata, 'e, 'edata>) =
        Map.fold folder state pg.Vertices
        
    /// Fold over the edges of the property graph.
    let foldEdges folder state (pg: PropertyGraph<'v, 'vdata, 'e, 'edata>) =
        Map.fold folder state pg.Edges
        
    /// Iterate over the vertices of the property graph.
    let iterVertices action (pg: PropertyGraph<'v, 'vdata, 'e, 'edata>) =
        Map.iter action pg.Vertices
        
    /// Iterate over the edges of the property graph.
    let iterEdges action (pg: PropertyGraph<'v, 'vdata, 'e, 'edata>) =
        Map.iter action pg.Edges
        
    /// Check if any vertex satisfies a predicate.
    let existsVertex predicate (pg: PropertyGraph<'v, 'vdata, 'e, 'edata>) =
        Map.exists predicate pg.Vertices
        
    /// Check if any edge satisfies a predicate.
    let existsEdge predicate (pg: PropertyGraph<'v, 'vdata, 'e, 'edata>) =
        Map.exists predicate pg.Edges
        
    /// Check if all vertices satisfy a predicate.
    let forallVertices predicate (pg: PropertyGraph<'v, 'vdata, 'e, 'edata>) =
        Map.forall predicate pg.Vertices
        
    /// Check if all edges satisfy a predicate.
    let forallEdges predicate (pg: PropertyGraph<'v, 'vdata, 'e, 'edata>) =
        Map.forall predicate pg.Edges

    /// Filter the vertices of the property graph, removing those that do not satisfy a predicate (and therefore any
    /// connected edges).
    let filterVertices predicate (pg: PropertyGraph<'v, 'vdata, 'e, 'edata>) =
        pg.Vertices
        |> Map.filter (fun vertex data -> not <| predicate vertex data)
        |> Map.fold (fun acc vertex _ -> removeVertex vertex acc) pg

    /// Filter the edges of the property graph, removing those that do not satisfy a predicate.
    let filterEdges predicate (pg: PropertyGraph<'v, 'vdata, 'e, 'edata>) =
        pg.Edges
        |> Map.filter (fun edge data -> not <| predicate edge data)
        |> Map.fold (fun acc edge _ -> removeEdge edge acc) pg
        
    /// Create the subgraph induced by a set of vertices, containing only those vertices and the edges whose both
    /// endpoints are within the set.
    let inducedSubgraph vertexSet (pg: PropertyGraph<'v, 'vdata, 'e, 'edata>) =
        let verticesToRemove = Set.filter (fun vertex -> not <| Set.contains vertex vertexSet) (Graph.vertices pg.Graph)
        Set.fold (fun acc vertex -> removeVertex vertex acc) pg verticesToRemove
        
    /// Breadth-first search starting from a vertex, returning vertices in BFS order. The predicate takes (in order of
    /// curried argument) the current vertex, its data, the edge being traversed, its data, the neighboring vertex, and
    /// its data, and will skip any neighbor for which the predicate returns false.
    let bfs start predicate (pg: PropertyGraph<'v, 'vdata, 'e, 'edata>) =
        let predicate' v1 edge v2 =
            predicate v1 (Map.find v1 pg.Vertices) edge (Map.find edge pg.Edges) v2 (Map.find v2 pg.Vertices)

        Graph.bfs start predicate' pg.Graph
        
    /// Check if there is a path between two vertices, returning false if either vertex is not found. The predicate
    /// filters the search to support different notions of connectivity.
    let isConnected vertex1 vertex2 predicate (pg: PropertyGraph<'v, 'vdata, 'e, 'edata>) =
        pg
        |> bfs vertex1 predicate
        |> Seq.contains vertex2

    /// Get the data associated with a vertex, returning None if the vertex is not found.
    let getVertexData vertex (pg: PropertyGraph<'v, 'vdata, 'e, 'edata>) =
        Map.tryFind vertex pg.Vertices

    /// Get the data associated with an edge, returning None if the edge is not found.
    let getEdgeData edge (pg: PropertyGraph<'v, 'vdata, 'e, 'edata>) =
        Map.tryFind edge pg.Edges

    /// Set the data associated with a vertex, doing nothing if the vertex is not found.
    let setVertexData vertex data (pg: PropertyGraph<'v, 'vdata, 'e, 'edata>) =
        match containsVertex vertex pg with
        | false -> pg
        | true ->
            let vertices = Map.add vertex data pg.Vertices

            { pg with Vertices = vertices }

    /// Set the data associated with an edge, doing nothing if the edge is not found.
    let setEdgeData edge data (pg: PropertyGraph<'v, 'vdata, 'e, 'edata>) =
        match containsEdge edge pg with
        | false -> pg
        | true ->
            let edges = Map.add edge data pg.Edges

            { pg with Edges = edges }

    /// Map a function over the vertex data of the property graph.
    let mapVertices mapper (pg: PropertyGraph<'v, 'vdata, 'e, 'edata>) =
        let vertices = Map.map mapper pg.Vertices

        { pg with Vertices = vertices }

    /// Map a function over the edge data of the property graph.
    let mapEdges mapper (pg: PropertyGraph<'v, 'vdata, 'e, 'edata>) =
        let edges = Map.map mapper pg.Edges

        { pg with Edges = edges }