namespace Numberlink.Generator

open FsToolkit.ErrorHandling
open Numberlink.Core

type TemplateState<'P> = {
    Template: Template<'P>
    VertexIncrement: int
    EdgeIncrement: int
}

[<RequireQualifiedAccess>]
type TemplateBuilderError =
    /// Occurs when adding an edge to the given vertices that do not exist.
    | InvalidEdgeConnection of Set<Vertex>

type TemplateBuilder<'P>() =
    member _.Zero(): TemplateState<'P> -> Result<TemplateState<'P>, TemplateBuilderError> =
        Ok

    member _.Bind(
        (kind, position): TemplateVertex * 'P,
        f: Vertex -> TemplateState<'P> -> Result<TemplateState<'P>, TemplateBuilderError>
    ) =
        fun state ->
            let vertex = Vertex state.VertexIncrement
            let template = Template.addVertex vertex kind position state.Template

            f vertex { state with Template = template; VertexIncrement = state.VertexIncrement + 1 }

    member _.Bind(
        (kind, vertex1, vertex2): TemplateEdge * Vertex * Vertex,
        f: unit -> TemplateState<'P> -> Result<TemplateState<'P>, TemplateBuilderError>
    ) =
        fun state -> result {
            let edge = Edge state.EdgeIncrement

            let! template =
                state.Template
                |> Template.addEdge edge kind vertex1 vertex2
                |> Result.mapError TemplateBuilderError.InvalidEdgeConnection

            return! f () { state with Template = template; EdgeIncrement = state.EdgeIncrement + 1 }
        }

    member _.Return(_): TemplateState<'P> -> Result<TemplateState<'P>, TemplateBuilderError> =
        Ok

    member _.Run(
        f: TemplateState<'P> -> Result<TemplateState<'P>, TemplateBuilderError>
    ) =
        { Template = Template.empty<'P>; VertexIncrement = 0; EdgeIncrement = 0 }
        |> f
        |> Result.map _.Template

// TODO: This should do additional validation, returning new errors if other necessary rules are not followed when
//       the builder tries to complete (but implement validation as function on Template rather than here directly)

// TODO: Consider a LevelBuilder that runs the generator on the completed build. It may not make sense to do so, in
//       which case this template builder makes sense to remain as-is

[<AutoOpen>]
module TemplateBuilder =
    let template<'P> = TemplateBuilder<'P>()
    