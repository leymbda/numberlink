namespace FSharp.Collections.Graphs

[<RequireQualifiedAccess>]
type GraphBuilderCommand<'v, 'e when 'v : comparison and 'e : comparison> =
    | AddVertex of vertex: 'v
    | AddEdge of edge: 'e * vertex1: 'v * vertex2: 'v
    
[<RequireQualifiedAccess>]
type GraphBuilderError<'v, 'e when 'v : comparison and 'e : comparison> =
    | VertexAlreadyExists of vertex: 'v
    | EdgeAlreadyExists of edge: 'e
    | InvalidEdgeConnection of edge: 'e * missingVertices: Set<'v>

type GraphBuilder<'v, 'e when 'v : comparison and 'e : comparison>() =
    member _.Bind(vertex: 'v, f: 'v -> GraphBuilderCommand<'v, 'e> list) =
        GraphBuilderCommand.AddVertex vertex :: f vertex

    member _.Bind((edge: 'e, vertex1: 'v, vertex2: 'v), f: unit -> GraphBuilderCommand<'v, 'e> list) =
        GraphBuilderCommand.AddEdge(edge, vertex1, vertex2) :: f ()
        
    member _.Zero(): GraphBuilderCommand<'v, 'e> list =
        []

    member _.Return(_): GraphBuilderCommand<'v, 'e> list =
        []

    member _.Delay(f: unit -> GraphBuilderCommand<'v, 'e> list) =
        f ()

    member _.Combine(a: GraphBuilderCommand<'v, 'e> list, b: GraphBuilderCommand<'v, 'e> list) =
        a @ b

    member _.For(xs: 'a seq, f: 'a -> GraphBuilderCommand<'v, 'e> list) =
        xs
        |> Seq.collect f
        |> Seq.toList

    member _.Run(commands: GraphBuilderCommand<'v, 'e> list) =
        let evolve = fun cmd graph ->
            match cmd with
            | GraphBuilderCommand.AddVertex v ->
                match Graph.containsVertex v graph with
                | true -> Error (GraphBuilderError.VertexAlreadyExists v)
                | false -> Ok (Graph.addVertex v graph)

            | GraphBuilderCommand.AddEdge (e, v1, v2) ->
                match Graph.containsEdge e graph with
                | true -> Error (GraphBuilderError.EdgeAlreadyExists e)
                | false ->
                    graph
                    |> Graph.addEdge e v1 v2
                    |> Result.mapError (fun missing -> GraphBuilderError.InvalidEdgeConnection(e, missing))

        List.fold (fun acc cmd -> Result.bind (evolve cmd) acc) (Ok Graph.empty<'v, 'e>) commands

[<AutoOpen>]
module GraphBuilder =
    let graph<'v, 'e when 'v : comparison and 'e : comparison> = GraphBuilder<'v, 'e>()
    