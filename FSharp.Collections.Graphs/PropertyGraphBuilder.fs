namespace FSharp.Collections.Graphs

[<RequireQualifiedAccess>]
type PropertyGraphBuilderCommand<'v, 'vdata, 'e, 'edata when 'v : comparison and 'e : comparison> =
    | AddVertex of vertex: 'v * data: 'vdata
    | AddEdge of edge: 'e * data: 'edata * vertex1: 'v * vertex2: 'v

[<RequireQualifiedAccess>]
type PropertyGraphBuilderError<'v, 'e when 'v : comparison and 'e : comparison> =
    | VertexAlreadyExists of vertex: 'v
    | EdgeAlreadyExists of edge: 'e
    | InvalidEdgeConnection of edge: 'e * missingVertices: Set<'v>
    
type PropertyGraphBuilder<'v, 'vdata, 'e, 'edata when 'v : comparison and 'e : comparison>() =
    member _.Bind((vertex: 'v, data: 'vdata), f: 'v -> PropertyGraphBuilderCommand<'v, 'vdata, 'e, 'edata> list) =
        PropertyGraphBuilderCommand.AddVertex(vertex, data) :: f vertex

    member _.Bind((edge: 'e, data: 'edata, vertex1: 'v, vertex2: 'v), f: unit -> PropertyGraphBuilderCommand<'v, 'vdata, 'e, 'edata> list) =
        PropertyGraphBuilderCommand.AddEdge(edge, data, vertex1, vertex2) :: f ()
        
    member _.Zero(): PropertyGraphBuilderCommand<'v, 'vdata, 'e, 'edata> list =
        []

    member _.Return(_): PropertyGraphBuilderCommand<'v, 'vdata, 'e, 'edata> list =
        []
        
    member _.Delay(f: unit -> PropertyGraphBuilderCommand<'v, 'vdata, 'e, 'edata> list) =
        f ()

    member _.Combine(a: PropertyGraphBuilderCommand<'v, 'vdata, 'e, 'edata> list, b: PropertyGraphBuilderCommand<'v, 'vdata, 'e, 'edata> list) =
        a @ b

    member _.For(xs: 'a seq, f: 'a -> PropertyGraphBuilderCommand<'v, 'vdata, 'e, 'edata> list) =
        xs
        |> Seq.collect f
        |> Seq.toList

    member _.Run(commands: PropertyGraphBuilderCommand<'v, 'vdata, 'e, 'edata> list) =
        let evolve = fun cmd graph ->
            match cmd with
            | PropertyGraphBuilderCommand.AddVertex (v, data) ->
                match PropertyGraph.containsVertex v graph with
                | true -> Error (PropertyGraphBuilderError.VertexAlreadyExists v)
                | false -> Ok (PropertyGraph.addVertex v data graph)

            | PropertyGraphBuilderCommand.AddEdge (e, data, v1, v2) ->
                match PropertyGraph.containsEdge e graph with
                | true -> Error (PropertyGraphBuilderError.EdgeAlreadyExists e)
                | false ->
                    graph
                    |> PropertyGraph.addEdge e data v1 v2
                    |> Result.mapError (fun missing -> PropertyGraphBuilderError.InvalidEdgeConnection(e, missing))

        List.fold (fun acc cmd -> Result.bind (evolve cmd) acc) (Ok PropertyGraph.empty<'v, 'vdata, 'e, 'edata>) commands
    
[<AutoOpen>]
module PropertyGraphBuilder =
    let propertyGraph<'v, 'vdata, 'e, 'edata when 'v : comparison and 'e : comparison> = PropertyGraphBuilder<'v, 'vdata, 'e, 'edata>()
    