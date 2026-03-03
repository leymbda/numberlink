module Numberlink.ZigZag.Solver.SmtSolver

open FSharp.Collections.Graphs
open Microsoft.Z3

/// An expression representing the ID of the line passing through a vertex.
let private mkVertexExpr (vertex: int) (ctx: Context) =
    ctx.MkIntConst(sprintf "v_%d" vertex) :> Expr

/// Solve the Numberlink puzzle using an SMT solver. Returns a set of tuples of vertex IDs and the ID of the line in
/// which passes through it.
let solve (pg: PropertyGraph<int, unit, int, unit>) = // TODO: This type should be replaced by some kind of empty representation of a level (e.g. solver template vs generator template?)
    try
        use ctx = new Context()

        // TODO: Implement SMT encoding of the Numberlink puzzle here.

        let solver = ctx.MkSolver()

        match solver.Check() with
        | Status.SATISFIABLE ->
            pg
            |> PropertyGraph.vertices
            |> Set.map (fun (vertex, _) ->
                let vertexExpr = mkVertexExpr vertex ctx
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
