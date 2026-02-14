namespace Numberlink.ZigZag.Core

open FsToolkit.ErrorHandling
open Numberlink.ZigZag.Core.Lib
open System

type Level<'P> = {
    Id: Guid
    Template: Template<'P>
    Links: Guid list list
}

module Level =
    /// Create an existing level with predefined links.
    let create levelId template links =
        { Id = levelId; Template = template; Links = links }

    /// Generate a new level based on the given template.
    let generate random levelId (template: Template<'P>) = result {
        let! domains =
            Generator.generate random template
            |> Result.requireSome "Failed to generate level from template"

        // TODO: Remove above temporary debug output (or consider how to add this observability togglable)

        domains |> Map.iter (fun vertexId state ->
            match state with
                | GeneratorDomain.Terminal edge ->
                    let neighbor =
                        Graph.getNeighbors vertexId template.Graph
                        |> Seq.find (fun rel -> rel.EdgeId = edge)
                        |> _.VertexId

                    printfn "V %A: Terminal connecting %A" vertexId neighbor

                | GeneratorDomain.Bridge edges ->
                    let neighbors =
                        Graph.getNeighbors vertexId template.Graph
                        |> Seq.filter (fun rel ->
                            edges
                            |> Seq.collect (fun (e1, e2) -> [e1; e2])
                            |> Seq.contains rel.EdgeId
                        )
                        |> Seq.map _.VertexId
                        |> Seq.toArray

                    printfn "V %A: Bridge connecting %A" vertexId neighbors
                
                | GeneratorDomain.Path (edge1, edge2) ->
                    let neighbors =
                        Graph.getNeighbors vertexId template.Graph
                        |> Seq.filter (fun rel -> rel.EdgeId = edge1 || rel.EdgeId = edge2)
                        |> Seq.map _.VertexId
                        |> Seq.toArray

                    printfn "V %A: Path connecting %A" vertexId neighbors
        )

        // TODO: Create links based off domains

        return { Id = levelId; Template = template; Links = [] }
    }
