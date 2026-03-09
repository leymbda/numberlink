module Numberlink.ZigZag.Solver.SmtSolver

open FSharp.Collections.Graphs
open Microsoft.Z3
open Numberlink.ZigZag.Core

/// Solve the Numberlink puzzle using an SMT solver. Returns a set of tuples of edges and the line that passes through it.
let solve (puzzle: Puzzle<'P>) =
    try
        use ctx = new Context()

        // Define variables for each vertex
        let vertices =
            puzzle.Graph
            |> PropertyGraph.vertices
            |> Set.map (fun (vertex, _) -> vertex, ctx.MkInt $"v_{vertex}" :> Expr)

        // Define variables for each edge
        let edges =
            puzzle.Graph
            |> PropertyGraph.edges
            |> Set.map (fun (edge, _) -> edge, ctx.MkInt $"e_{edge}" :> Expr)

        // Run solver and extract results
        let solver = ctx.MkSolver()

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
            Error "Unsatisfiable problem"

        | Status.UNKNOWN | _ ->
            Error "Could not determine if a solution exists"

    with ex ->
        Error (sprintf "An error occurred: %s" ex.Message)
