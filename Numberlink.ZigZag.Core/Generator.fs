namespace Numberlink.ZigZag.Core

open FsToolkit.ErrorHandling
open Numberlink.ZigZag.Core.Lib
open System

type GeneratorDomain =
    | Terminal of edge: Guid
    | Path of edges: Guid * Guid
    | Bridge of edges: (Guid * Guid) list

module GeneratorDomain =
    /// Get all edges used in a domain.
    let edges domain =
        match domain with
        | Terminal e -> seq { e }
        | Path (e1, e2) -> seq { e1; e2 }
        | Bridge edges ->
            edges
            |> List.collect (fun (e1, e2) -> [e1; e2])
            |> List.toSeq

    /// Attempt to traverse an edge in a domain, returning the connected edge if it exists.
    let tryTraverse edgeId domain =
        match domain with
        | Terminal _ ->
            None

        | Path (e1, e2) ->
            match e1, e2 with
            | e1, _ when e1 = edgeId -> Some e2
            | _, e2 when e2 = edgeId -> Some e1
            | _ -> None

        | Bridge edges ->
            edges
            |> List.collect (fun (e1, e2) ->
                match e1, e2 with
                | e1, _ when e1 = edgeId -> [e2]
                | _, e2 when e2 = edgeId -> [e1]
                | _ -> []
            )
            |> List.tryHead

module Generator =
    /// Calculate the initial domains for the graph
    let initialDomains (graph: Graph<TemplateVertex, TemplateEdge>): Map<Guid, Domain<GeneratorDomain>> =
        graph.Vertices
        |> Map.map (fun vertexId v ->
            match v with
            | Unobserved ->
                let edges =
                    graph
                    |> Graph.getNeighbors vertexId
                    |> Seq.map _.EdgeId
                    |> Seq.toList
                    
                let pathDomains =
                    edges
                    |> List.allPairs edges
                    |> List.filter (fun (a, b) -> a < b)
                    |> List.map (fun (a, b) -> GeneratorDomain.Path (a, b), 1.0)

                let terminalDomains =
                    edges
                    |> List.map (fun e -> GeneratorDomain.Terminal e, 1.0)
                    
                terminalDomains @ pathDomains
                |> Map.ofList

            | TemplateVertex.Bridge edges ->
                Map.singleton (GeneratorDomain.Bridge edges) 1.0
        )

    /// Ensure vertices collapse into a compatible state with neighboring collapsed vertices
    let neighborConsistencyConstraint: Constraint<TemplateVertex, TemplateEdge, GeneratorDomain> =
        fun vertexId currentDomain collapsed graph ->
            let neighbors = Graph.getNeighbors vertexId graph

            let requiredEdges =
                neighbors
                |> Seq.collect (fun rel ->
                    Map.tryFind rel.VertexId collapsed
                    |> Option.map (GeneratorDomain.edges >> Seq.filter ((=) rel.EdgeId))
                    |> Option.defaultValue Seq.empty
                )

            let optionalEdges = // TODO: Should this consider potential uncollapsed neighbor domains?
                neighbors
                |> Seq.collect (fun rel ->
                    Map.tryFind rel.VertexId collapsed
                    |> Option.map (fun _ -> Seq.empty)
                    |> Option.defaultValue (Seq.singleton rel.EdgeId)
                )

            currentDomain
            |> Map.map (fun domain weight ->
                /// Ensure neighboring edges are compatible with the current domain
                let isEdgeConsistent =
                    match domain with
                    | GeneratorDomain.Terminal e ->
                        match Seq.toList requiredEdges, Seq.contains e optionalEdges with
                        | [], true -> true
                        | [r], _ when r = e -> true
                        | _, _ -> false

                    | GeneratorDomain.Path (e1, e2) ->
                        let hasUnexpectedRequired = Seq.exists (fun e -> e <> e1 && e <> e2) requiredEdges
                        let allowsEdge1 = Seq.contains e1 optionalEdges || Seq.contains e1 requiredEdges
                        let allowsEdge2 = Seq.contains e2 optionalEdges || Seq.contains e2 requiredEdges

                        not hasUnexpectedRequired && allowsEdge1 && allowsEdge2

                    | GeneratorDomain.Bridge edges ->
                        let bridgeEdges = edges |> Seq.collect (fun (e1, e2) -> [e1; e2])

                        let hasUnexpectedRequired =
                            Seq.exists (fun e -> not <| Seq.contains e bridgeEdges) requiredEdges

                        let allowsAllBridgeEdges =
                            bridgeEdges
                            |> Seq.forall (fun e -> Seq.contains e optionalEdges || Seq.contains e requiredEdges)

                        not hasUnexpectedRequired && allowsAllBridgeEdges

                /// Ensure neighboring vertex types are compatible with the current domain
                let isTypeConsistent =
                    neighbors
                    |> Seq.collect (fun rel ->
                        Map.tryFind rel.VertexId collapsed
                        |> Option.map (fun d -> [d])
                        |> Option.defaultValue []
                    )
                    |> Seq.forall (fun neighborDomain ->
                        match domain, neighborDomain with
                        | GeneratorDomain.Terminal _, GeneratorDomain.Bridge _ -> false
                        | GeneratorDomain.Bridge _, GeneratorDomain.Terminal _ -> false
                        | _, _ -> true
                    )

                /// Ensure connected warps only connect compatible domains
                let isWarpConsistent =
                    true
                        
                    // TODO: Warps need to have property in template to specify if they must be used in order to
                    //       constrain appropriately for requirements. For now, a level could have warps but none
                    //       be used. Once implemented, this check should ensure it so domains that don't use a
                    //       required warp are rejected.

                if isEdgeConsistent && isTypeConsistent && isWarpConsistent then weight
                else 0.0
            )
            |> Map.filter (fun _ weight -> weight <> 0.0)

    /// Ensure that all continuous lines end with terminals
    let lineTerminationConstraint: Constraint<TemplateVertex, TemplateEdge, GeneratorDomain> =
        fun vertexId currentDomain collapsed graph ->
            // Check neighbors to see if any would lose their ability to be terminated. May cause issues with a
            // situation where many lines expect to terminate as a single vertex, but hopefully backtracking can
            // eventually resolve such situations.

            // TODO: Implement

            currentDomain

    /// Ensure continuous lines do not have available shortcuts (cycles)
    let lineShortcutConstraint: Constraint<TemplateVertex, TemplateEdge, GeneratorDomain> =
        fun vertexId currentDomain collapsed graph ->
            currentDomain

    /// Ensure continuous lines do not have available shortcuts (cycles)
    let lineShortcutConstraint_OLD_TO_REMOVE: Constraint<TemplateVertex, TemplateEdge, GeneratorDomain> =
        fun vertexId currentDomain collapsed graph ->
            let rec loop visited queued collapsed' =
                match queued with
                | [] -> false
                | (vertexId, _) :: _ when List.contains vertexId visited -> true
                | (vertexId, edgeId) :: rest ->
                    let nextEdgeId =
                        collapsed'
                        |> Map.tryFind vertexId
                        |> Option.bind (GeneratorDomain.tryTraverse edgeId)

                    let nextVertexId =
                        nextEdgeId
                        |> Option.bind (fun e ->
                            graph
                            |> Graph.getNeighbors vertexId
                            |> Seq.tryFind (fun r -> r.EdgeId = e)
                            |> Option.map _.VertexId
                        )

                    match nextEdgeId, nextVertexId with
                    | Some v, Some e -> loop (vertexId :: visited) ((v, e) :: rest) collapsed'
                    | _, _ -> loop (vertexId :: visited) rest collapsed'

            let containsCycle edgeIds collapsed' =
                let queued =
                    graph
                    |> Graph.getNeighbors vertexId
                    |> Seq.filter (fun rel -> List.contains rel.EdgeId edgeIds)
                    |> Seq.map (fun rel -> rel.VertexId, rel.EdgeId)
                    |> Seq.toList

                loop [vertexId] queued collapsed'

            currentDomain
            |> Map.map (fun domain weight ->
                let collapsed' = Map.add vertexId domain collapsed

                let containsCycle =
                    match domain with
                    | GeneratorDomain.Terminal _ -> false
                    | GeneratorDomain.Path (e1, e2) -> containsCycle [e1; e2] collapsed'
                    | GeneratorDomain.Bridge e -> List.exists (fun (e1, e2) -> containsCycle [e1; e2] collapsed') e

                if containsCycle then 0.0
                else weight
            )

            // TODO: This is still capable of forming a circle in the 3x3 donut with terminals disabled. Should
            //       fail to generate instead.

    // TODO: This constraint seems to also achieve the terminal end constraint. Still need to check that a line
    //       doesn't pass nearby itself though (unless implicitly implemented already too). I dont think it does
    //       and this anti-cycle logic would probably be inferred from this new necessary constraint, so probably
    //       need to create it then delete this one as its redundant.

    /// Multiply domain weights to ensure an appropriate distribution of types
    let weightMultiplierConstraint: Constraint<TemplateVertex, TemplateEdge, GeneratorDomain> =
        fun _ currentDomain _ _ ->
            // Make the proportion of weight assigned to terminals lower than that of paths so that lines are more
            // likely to continue rather than make many tiny ones.

            // TODO: Implement

            currentDomain
            |> Map.map (fun domain weight ->
                match domain with
                | GeneratorDomain.Terminal _ -> 0.0
                | _ -> weight
            )

    let constraints =
        [
            neighborConsistencyConstraint
            lineTerminationConstraint
            lineShortcutConstraint
            weightMultiplierConstraint
        ]

    /// Generate valid domains for each vertex in the template.
    let generate random (template: Template<'P>) =
        // TODO: Validation:
        //       - Ensure any vertices exist
        //       - Ensure all vertices have the same position type
        //       - Ensure edges connect existing vertices
        //       - Ensure bridges have an even number of connections
        //       - Ensure bridge edges are properly formed with pairs (probably integrate into above check)
        //       - Ensure warps aren't stacked

        template.Graph
        |> WaveFunctionCollapse.init random initialDomains constraints
        |> WaveFunctionCollapse.run
