module Numberlink.ZigZag.Solver.SmtSolver

open FSharp.Collections.Graphs
open Microsoft.Z3
open Numberlink.ZigZag.Core

/// Solve the Numberlink puzzle using an SMT solver. Returns a set of tuples of vertex IDs and the ID of the line in
/// which passes through it.
let solve (level: Level<'P>) =
    try
        use ctx = new Context()

        // Define variables for each vertex
        let vertices =
            level.Graph
            |> PropertyGraph.vertices
            |> Set.map (fun (vertex, _) -> vertex, ctx.MkInt $"v_{vertex}" :> Expr)

        // Define variables for each edge
        let edges =
            level.Graph
            |> PropertyGraph.edges
            |> Set.map (fun (edge, _) -> edge, ctx.MkInt $"e_{edge}" :> Expr)

        // Run solver and extract results
        let solver = ctx.MkSolver()

        match solver.Check() with
        | Status.SATISFIABLE ->
            vertices
            |> Set.map (fun (vertex, vertexExpr) ->
                let lineExpr = solver.Model.Eval(vertexExpr, true)
                vertex, (lineExpr :?> IntNum).Int
            )
            |> Ok

        | Status.UNSATISFIABLE ->
            Error "Unsatisfiable problem"

        | Status.UNKNOWN | _ ->
            Error "Could not determine if a solution exists"

    with ex ->
        Error (sprintf "An error occurred: %s" ex.Message)
