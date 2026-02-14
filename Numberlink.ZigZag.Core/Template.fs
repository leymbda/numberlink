namespace Numberlink.ZigZag.Core

open Numberlink.ZigZag.Core.Lib
open System

type TemplateVertex =
    | Unobserved
    | Bridge of edges: (Guid * Guid) list

type TemplateEdge =
    | Path
    | Warp

type Template<'P> = {
    Graph: Graph<TemplateVertex, TemplateEdge>
    Positions: Map<Guid, 'P>
}

module Template =
    /// Create an empty template.
    let empty<'P> =
        { Graph = Graph.empty; Positions = Map.empty<Guid, 'P> }

    /// Add an unobserved vertex to the template graph to generate levels off.
    let addUnobserved vertexId position (template: Template<'P>) =
        let graph = Graph.addVertex vertexId Unobserved template.Graph
        let positions = Map.add vertexId position template.Positions

        { template with Graph = graph; Positions = positions }

    /// Add a bridge vertex to the template graph along with the edges it connects, defined as a list of tuples where
    /// the first item is the ID of the edge to create and the second is the ID of the vertex to connect to. The tuples
    /// are grouped in pairs to define the connections for each bridge segment.
    let addBridge vertexId (edges: ((Guid * Guid) * (Guid * Guid)) list) position (template: Template<'P>) =
        let vertex = Bridge (List.map (fun ((e1, _), (e2, _)) -> (e1, e2)) edges)

        let graph =
            Graph.addVertex vertexId vertex template.Graph
            |> List.foldBack
                (fun ((e1, v1), (e2, v2)) graph ->
                    graph
                    |> Graph.addEdge vertexId v1 e1 Path
                    |> Graph.addEdge vertexId v2 e2 Path
                )
                edges

        let positions = Map.add vertexId position template.Positions

        { template with Graph = graph; Positions = positions }

    /// Remove any type of vertex from the template graph.
    let removeVertex vertexId (template: Template<'P>) =
        let graph = Graph.removeVertex vertexId template.Graph
        let positions = Map.remove vertexId template.Positions

        { template with Graph = graph; Positions = positions }

    /// Add a possible path edge between two vertices in the template graph.
    let addPath fromVertexId toVertexId edgeId (template: Template<'P>) =
        let graph = Graph.addEdge fromVertexId toVertexId edgeId Path template.Graph
        
        { template with Graph = graph }

    /// Add a warp edge between two vertices in the template graph.
    let addWarp fromVertexId toVertexId edgeId (template: Template<'P>) =
        let graph = Graph.addEdge fromVertexId toVertexId edgeId Warp template.Graph
        
        { template with Graph = graph }

    /// Remove an edge from the template graph.
    let removeEdge edgeId (template: Template<'P>) =
        let graph = Graph.removeEdge edgeId template.Graph

        { template with Graph = graph }
