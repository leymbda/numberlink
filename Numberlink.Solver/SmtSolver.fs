namespace Numberlink.Solver

open FSharp.Collections.Graphs
open Microsoft.Z3
open Numberlink.Core

[<RequireQualifiedAccess>]
type SmtSolverError =
    | Unsatisfiable
    | Unknown of string
    | Error of string

module SmtSolver =
    /// Solve the Numberlink puzzle using an SMT solver. Returns the flow if a solution is found, or an error if the
    /// puzzle is unsatisfiable or an error occurs during solving.
    let solve (puzzle: Puzzle<'P>) =
        try
            use ctx = new Context()
            let solver = ctx.MkSolver()

            let edgeExprMap =
                puzzle.Graph
                |> PropertyGraph.edges
                |> Set.map (fun (edge, _) ->
                    let (Edge edgeVal) = edge
                    edge, ctx.MkIntConst $"e_{edgeVal}" :> Expr
                )
                |> Set.toSeq
                |> Map.ofSeq

            let bridgeLaneGroups vertex =
                puzzle.Graph
                |> PropertyGraph.incidentEdges vertex
                |> Option.defaultValue Set.empty
                |> Set.toSeq
                |> Seq.choose (fun (edge, edata) ->
                    match edata with
                    | PuzzleEdge.BridgeLane lane -> Some (lane, edge)
                    | _ -> None
                )
                |> Seq.groupBy fst
                |> Seq.map (fun (_, laneEdges) ->
                    let exprs = laneEdges |> Seq.map (fun (_, edge) -> Map.find edge edgeExprMap) |> Seq.toArray
                    exprs[0], exprs[1]
                )

            let bridgeLaneExprs vertex =
                bridgeLaneGroups vertex
                |> Seq.map (fun (e1, e2) -> ctx.MkEq(e1, e2))

            // Bridge lane pairs must connect the same line
            let bridgeLaneConstraints =
                puzzle.Graph
                |> PropertyGraph.vertices
                |> Set.toSeq
                |> Seq.filter (fun (_, vdata) -> vdata = PuzzleVertex.Bridge)
                |> Seq.collect (fun (vertex, _) -> bridgeLaneExprs vertex)
                |> Seq.toArray

            solver.Add(bridgeLaneConstraints)

            let bridgeDistinctLaneExpr vertex =
                bridgeLaneGroups vertex
                |> Seq.map fst
                |> Seq.toArray
                |> ctx.MkDistinct

            // Bridge lanes must be used for different lines
            let bridgeDistinctLaneConstraints =
                puzzle.Graph
                |> PropertyGraph.vertices
                |> Set.toSeq
                |> Seq.filter (fun (_, vdata) -> vdata = PuzzleVertex.Bridge)
                |> Seq.map (fun (vertex, _) -> bridgeDistinctLaneExpr vertex)
                |> Seq.toArray

            solver.Add(bridgeDistinctLaneConstraints)

            let isUsed (edgeExpr: Expr) =
                ctx.MkNot(ctx.MkEq(edgeExpr, ctx.MkInt 0))

            // Bridge lane edges are always part of a line
            let bridgeLaneUsedConstraints =
                puzzle.Graph
                |> PropertyGraph.edges
                |> Set.toSeq
                |> Seq.choose (fun (edge, edata) ->
                    match edata with
                    | PuzzleEdge.BridgeLane _ -> Some (isUsed (Map.find edge edgeExprMap))
                    | _ -> None
                )
                |> Seq.toArray

            solver.Add(bridgeLaneUsedConstraints)

            let maxLineValue =
                puzzle.Graph
                |> PropertyGraph.vertices
                |> Set.toSeq
                |> Seq.choose (fun (_, vdata) ->
                    match vdata with
                    | PuzzleVertex.Terminal (Line n) -> Some n
                    | _ -> None
                )
                |> Seq.max

            // Edge values are bounded to valid line identifiers or 0 (unused)
            let edgeBoundConstraints =
                edgeExprMap
                |> Map.toSeq
                |> Seq.collect (fun (_, edgeExpr) ->
                    let arith = edgeExpr :?> ArithExpr
                    [| ctx.MkGe(arith, ctx.MkInt 0); ctx.MkLe(arith, ctx.MkInt maxLineValue) |]
                )
                |> Seq.toArray

            solver.Add(edgeBoundConstraints)

            let vertexConstraintExprs vertex vdata =
                let edgeExprs =
                    puzzle.Graph
                    |> PropertyGraph.incidentEdges vertex
                    |> Option.defaultValue Set.empty
                    |> Set.toSeq
                    |> Seq.map (fun (edge, _) -> Map.find edge edgeExprMap)
                    |> Seq.toArray

                let usedBools = Array.map isUsed edgeExprs

                seq {
                    match vdata with
                    | PuzzleVertex.Bridge -> ()
                    | PuzzleVertex.Terminal (Line lineVal) ->
                        yield ctx.MkPBEq(Array.create usedBools.Length 1, usedBools, 1)
                        yield ctx.MkOr(Array.map (fun e -> ctx.MkEq(e, ctx.MkInt lineVal)) edgeExprs)

                    | PuzzleVertex.Cell ->
                        yield ctx.MkPBEq(Array.create usedBools.Length 1, usedBools, 2)

                        for i in 0 .. edgeExprs.Length - 1 do
                            for j in i + 1 .. edgeExprs.Length - 1 do
                                yield ctx.MkImplies(
                                    ctx.MkAnd(isUsed edgeExprs[i], isUsed edgeExprs[j]),
                                    ctx.MkEq(edgeExprs[i], edgeExprs[j]))
                }

            // Degree, terminal line identity, and line consistency per vertex
            let vertexConstraints =
                puzzle.Graph
                |> PropertyGraph.vertices
                |> Set.toSeq
                |> Seq.collect (fun (vertex, vdata) -> vertexConstraintExprs vertex vdata)
                |> Seq.toArray

            solver.Add(vertexConstraints)

            // Prevent lines from touching themselves
            let selfTouchConstraints =
                puzzle.Graph
                |> PropertyGraph.edges
                |> Set.toSeq
                |> Seq.collect (fun (edge, _) ->
                    let (v1, _), (v2, _) =
                        puzzle.Graph
                        |> PropertyGraph.getEndpoints edge
                        |> Option.get

                    let v1Exprs =
                        puzzle.Graph
                        |> PropertyGraph.incidentEdges v1
                        |> Option.defaultValue Set.empty
                        |> Set.toSeq
                        |> Seq.choose (fun (e, _) -> if e = edge then None else Some (Map.find e edgeExprMap))
                        |> Seq.toArray

                    let v2Exprs =
                        puzzle.Graph
                        |> PropertyGraph.incidentEdges v2
                        |> Option.defaultValue Set.empty
                        |> Set.toSeq
                        |> Seq.choose (fun (e, _) -> if e = edge then None else Some (Map.find e edgeExprMap))
                        |> Seq.toArray

                    let edgeExpr = Map.find edge edgeExprMap

                    seq {
                        for e1 in v1Exprs do
                            for e2 in v2Exprs do
                                yield ctx.MkImplies(
                                    ctx.MkAnd([| isUsed e1; isUsed e2; ctx.MkEq(e1, e2) |]),
                                    ctx.MkEq(edgeExpr, e1)
                                )
                    }
                )
                |> Seq.toArray

            solver.Add(selfTouchConstraints)

            // TODO: Rank constraint to prevent shadow cycles.

            //let terminalSources =
            //    puzzle.Graph
            //    |> PropertyGraph.vertices
            //    |> Set.toSeq
            //    |> Seq.choose (fun (vertex, vdata) ->
            //        match vdata with
            //        | PuzzleVertex.Terminal line -> Some (line, vertex)
            //        | _ -> None
            //    )
            //    |> Seq.groupBy fst
            //    |> Seq.map (fun (_, pairs) -> pairs |> Seq.map snd |> Seq.min)
            //    |> Seq.toArray

            //let vertexRankMap =
            //    puzzle.Graph
            //    |> PropertyGraph.vertices
            //    |> Set.toSeq
            //    |> Seq.choose (fun (vertex, vdata) ->
            //        let (Vertex vertexVal) = vertex

            //        match vdata with
            //        | PuzzleVertex.Bridge -> None
            //        | _ -> Some (vertex, ctx.MkIntConst $"r_{vertexVal}" :> ArithExpr)
            //    )
            //    |> Map.ofSeq

            //let bridgeLaneRankVars =
            //    puzzle.Graph
            //    |> PropertyGraph.vertices
            //    |> Set.toSeq
            //    |> Seq.filter (fun (_, vdata) -> vdata = PuzzleVertex.Bridge)
            //    |> Seq.collect (fun (vertex, _) ->
            //        let (Vertex vertexVal) = vertex

            //        puzzle.Graph
            //        |> PropertyGraph.incidentEdges vertex
            //        |> Option.defaultValue Set.empty
            //        |> Set.toSeq
            //        |> Seq.choose (fun (_, edata) ->
            //            match edata with
            //            | PuzzleEdge.BridgeLane lane -> Some lane
            //            | _ -> None
            //        )
            //        |> Seq.distinct
            //        |> Seq.map (fun lane -> (vertex, lane), ctx.MkInt $"r_{vertexVal}_l{lane}" :> ArithExpr)
            //    )
            //    |> Map.ofSeq

            //let bridgeEdgeRankMap =
            //    puzzle.Graph
            //    |> PropertyGraph.vertices
            //    |> Set.toSeq
            //    |> Seq.filter (fun (_, vdata) -> vdata = PuzzleVertex.Bridge)
            //    |> Seq.collect (fun (vertex, _) ->
            //        puzzle.Graph
            //        |> PropertyGraph.incidentEdges vertex
            //        |> Option.defaultValue Set.empty
            //        |> Set.toSeq
            //        |> Seq.choose (fun (edge, edata) ->
            //            match edata with
            //            | PuzzleEdge.BridgeLane lane -> Some (edge, Map.find (vertex, lane) bridgeLaneRankVars)
            //            | _ -> None
            //        )
            //    )
            //    |> Map.ofSeq

            //let getRankExpr edge vertex =
            //    match Map.tryFind vertex vertexRankMap with
            //    | Some r -> r
            //    | None -> Map.find edge bridgeEdgeRankMap

            //let vertexCount =
            //    puzzle.Graph
            //    |> PropertyGraph.vertices
            //    |> Set.count

            //// Rank values are non-negative and bounded by the total number of vertices
            //let rankBoundConstraints =
            //    seq {
            //        yield! vertexRankMap |> Map.toSeq |> Seq.map snd
            //        yield! bridgeLaneRankVars |> Map.toSeq |> Seq.map snd
            //    }
            //    |> Seq.collect (fun r -> [| ctx.MkGe(r, ctx.MkInt 0); ctx.MkLe(r, ctx.MkInt (vertexCount - 1)) |])
            //    |> Seq.toArray

            //solver.Add(rankBoundConstraints)

            //// Source terminals start at rank 0
            //let rankSourceConstraints =
            //    terminalSources
            //    |> Array.map (fun source -> ctx.MkEq(Map.find source vertexRankMap, ctx.MkInt 0))

            //solver.Add(rankSourceConstraints)

            //// For each used edge, the ranks of its endpoints must differ by exactly 1
            //let rankIncrementConstraints =
            //    puzzle.Graph
            //    |> PropertyGraph.edges
            //    |> Set.toSeq
            //    |> Seq.map (fun (edge, _) ->
            //        let (v1, _), (v2, _) =
            //            puzzle.Graph
            //            |> PropertyGraph.getEndpoints edge
            //            |> Option.get

            //        let r1 = getRankExpr edge v1
            //        let r2 = getRankExpr edge v2

            //        ctx.MkImplies(
            //            isUsed (Map.find edge edgeExprMap),
            //            ctx.MkOr(
            //                ctx.MkEq(r2, ctx.MkAdd([| r1; ctx.MkInt 1 |])),
            //                ctx.MkEq(r1, ctx.MkAdd([| r2; ctx.MkInt 1 |]))
            //            )
            //        )
            //    )
            //    |> Seq.toArray

            //solver.Add(rankIncrementConstraints)

            //printfn "%A" solver

            // TODO: Various optimizations (like corner checks, etc)
            // TODO: Retry constraint to prevent nonunique (should this be a separate function?)
            // TODO: Order by unit, domain limits, local topology, connectivity for efficiency
            // TODO: Use bitvectors for efficiency

            match solver.Check() with
            | Status.SATISFIABLE ->
                // edges
                // |> Set.map (fun (edge, edgeExpr) ->
                //     let lineExpr = solver.Model.Eval(edgeExpr, true)
                //     let line = (lineExpr :?> IntNum).Int
                //     let result = if line = 0 then None else Some (Line line)
                
                //     edge, result
                // )
                // |> Ok

                Ok Flow.empty // TODO: Extract values from model and construct Flow

            | Status.UNSATISFIABLE ->
                Error SmtSolverError.Unsatisfiable

            | Status.UNKNOWN | _ ->
                Error (SmtSolverError.Unknown solver.ReasonUnknown)

        with ex ->
            Error (SmtSolverError.Error ex.Message)
