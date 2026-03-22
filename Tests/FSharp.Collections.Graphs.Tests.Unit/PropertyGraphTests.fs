namespace FSharp.Collections.Graphs

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type PropertyGraphTests() =
    let pg =
        propertyGraph {
            let! v1 = 1, ()
            let! v2 = 2, ()
            let! v3 = 3, ()
            let! v4 = 4, ()
            let! _ = 5, ()
            let! v6 = 6, ()
            let! v7 = 7, ()
            do! 1, (), v1, v2
            do! 2, (), v1, v3
            do! 3, (), v1, v4
            do! 4, (), v3, v6
            do! 5, (), v3, v6
            do! 6, (), v6, v7
        }
        |> Result.defaultWith (fun _ -> failwith "Invalid graph")

    [<TestMethod>]
    member _.``isEmpty - Returns true for empty graph``() =
        // Arrange
        let emptyGraph = PropertyGraph.empty<int, unit, int, unit>

        // Act
        let res = PropertyGraph.isEmpty emptyGraph

        // Assert
        Assert.IsTrue(res)

    [<TestMethod>]
    member _.``isEmpty - Returns false for non-empty graph``() =
        // Arrange

        // Act
        let res = PropertyGraph.isEmpty pg

        // Assert
        Assert.IsFalse(res)

    [<TestMethod>]
    member _.``incidentEdges - Returns None for non-existent vertex``() =
        // Arrange

        // Act
        let res = PropertyGraph.incidentEdges 0 pg

        // Assert
        Assert.AreEqual(None, res)

    [<TestMethod>]
    member _.``incidentEdges - Returns empty set for vertex with no edges``() =
        // Arrange

        // Act
        let res = PropertyGraph.incidentEdges 5 pg

        // Assert
        Assert.AreEqual(Some Set.empty, res)

    [<TestMethod>]
    member _.``incidentEdges - Returns correct edges for vertex``() =
        // Arrange

        // Act
        let res = PropertyGraph.incidentEdges 1 pg

        // Assert
        Assert.AreEqual(Some (Set.ofList [1, (); 2, (); 3, ()]), res)

    [<TestMethod>]
    member _.``adjacency - Returns None for non-existent vertex``() =
        // Arrange

        // Act
        let res = PropertyGraph.adjacency 0 pg

        // Assert
        Assert.AreEqual(None, res)

    [<TestMethod>]
    member _.``adjacency - Returns empty set for vertex with no edges``() =
        // Arrange

        // Act
        let res = PropertyGraph.adjacency 5 pg

        // Assert
        Assert.AreEqual(Some Set.empty, res)

    [<TestMethod>]
    member _.``adjacency - Returns correct neighbors for vertex``() =
        // Arrange

        // Act
        let res = PropertyGraph.adjacency 1 pg

        // Assert
        Assert.AreEqual(Some (Set.ofList [1, (), 2, (); 2, (), 3, (); 3, (), 4, ()]), res)

    [<TestMethod>]
    member _.``neighbors - Returns None for non-existent vertex``() =
        // Arrange

        // Act
        let res = PropertyGraph.neighbors 0 pg

        // Assert
        Assert.AreEqual(None, res)

    [<TestMethod>]
    member _.``neighbors - Returns empty set for vertex with no edges``() =
        // Arrange

        // Act
        let res = PropertyGraph.neighbors 5 pg

        // Assert
        Assert.AreEqual(Some Set.empty, res)

    [<TestMethod>]
    member _.``neighbors - Returns correct neighbors for vertex``() =
        // Arrange

        // Act
        let res = PropertyGraph.neighbors 1 pg

        // Assert
        Assert.AreEqual(Some (Set.ofList [2, (); 3, (); 4, ()]), res)

    [<TestMethod>]
    member _.``degree - Returns None for non-existent vertex``() =
        // Arrange

        // Act
        let res = PropertyGraph.degree 0 pg

        // Assert
        Assert.AreEqual(None, res)

    [<TestMethod>]
    member _.``degree - Returns 0 for vertex with no edges``() =
        // Arrange

        // Act
        let res = PropertyGraph.degree 5 pg

        // Assert
        Assert.AreEqual(Some 0, res)

    [<TestMethod>]
    member _.``degree - Returns correct degree for vertex``() =
        // Arrange

        // Act
        let res = PropertyGraph.degree 1 pg

        // Assert
        Assert.AreEqual(Some 3, res)

    [<TestMethod>]
    member _.``containsVertex - Returns false for non-existent vertex``() =
        // Arrange

        // Act
        let res = PropertyGraph.containsVertex 0 pg

        // Assert
        Assert.IsFalse(res)

    [<TestMethod>]
    member _.``containsVertex - Returns true for existing vertex``() =
        // Arrange

        // Act
        let res = PropertyGraph.containsVertex 2 pg

        // Assert
        Assert.IsTrue(res)

    [<TestMethod>]
    member _.``containsEdge - Returns false for non-existent edge``() =
        // Arrange

        // Act
        let res = PropertyGraph.containsEdge 0 pg

        // Assert
        Assert.IsFalse(res)

    [<TestMethod>]
    member _.``containsEdge - Returns true for existing edge``() =
        // Arrange

        // Act
        let res = PropertyGraph.containsEdge 1 pg

        // Assert
        Assert.IsTrue(res)

    [<TestMethod>]
    member _.``getEndpoints - Returns None for non-existent edge``() =
        // Arrange

        // Act
        let res = PropertyGraph.getEndpoints 0 pg

        // Assert
        Assert.AreEqual(None, res)

    [<TestMethod>]
    member _.``getEndpoints - Returns correct endpoints for existing edge``() =
        // Arrange

        // Act
        let res = PropertyGraph.getEndpoints 1 pg

        // Assert
        Assert.AreEqual(Some ((1, ()), (2, ())), res)

    [<TestMethod>]
    member _.``isAdjacent - Returns None if either vertex doesn't exist``() =
        // Arrange

        // Act
        let res = PropertyGraph.isAdjacent 0 1 pg

        // Assert
        Assert.AreEqual(None, res)

    [<TestMethod>]
    member _.``isAdjacent - Returns false for non-adjacent vertices``() =
        // Arrange

        // Act
        let res = PropertyGraph.isAdjacent 2 4 pg

        // Assert
        Assert.AreEqual(Some false, res)

    [<TestMethod>]
    member _.``isAdjacent - Returns true for adjacent vertices``() =
        // Arrange

        // Act
        let res = PropertyGraph.isAdjacent 1 2 pg

        // Assert
        Assert.AreEqual(Some true, res)

    [<TestMethod>]
    [<DataRow(0, 1)>]
    [<DataRow(1, 0)>]
    [<DataRow(0, -1)>]
    member _.``findEdges - Returns None if either or both vertices don't exist``(v1: int, v2: int) =
        // Arrange

        // Act
        let res = PropertyGraph.findEdges v1 v2 pg

        // Assert
        Assert.AreEqual(None, res)

    [<TestMethod>]
    member _.``findEdges - Returns empty set for non-adjacent vertices``() =
        // Arrange

        // Act
        let res = PropertyGraph.findEdges 2 4 pg

        // Assert
        Assert.AreEqual(Some Set.empty, res)

    [<TestMethod>]
    member _.``findEdges - Returns correct edges for adjacent vertices``() =
        // Arrange

        // Act
        let res = PropertyGraph.findEdges 3 6 pg

        // Assert
        Assert.AreEqual(Some (Set.ofList [4, (); 5, ()]), res)

    [<TestMethod>]
    member _.``addVertex - Replaces data if vertex already exists``() =
        // Arrange
        let pg =
            PropertyGraph.empty<int, unit, int, unit>
            |> PropertyGraph.addVertex 1 ()

        // Act
        let res = PropertyGraph.addVertex 1 () pg

        // Assert
        Assert.AreEqual(1, PropertyGraph.countVertices res)
        Assert.IsTrue(PropertyGraph.containsVertex 1 res)

    [<TestMethod>]
    member _.``addVertex - Adds vertex to graph``() =
        // Arrange
        let pg = PropertyGraph.empty<int, unit, int, unit>

        // Act
        let res = PropertyGraph.addVertex 1 () pg

        // Assert
        Assert.AreEqual(1, PropertyGraph.countVertices res)
        Assert.IsTrue(PropertyGraph.containsVertex 1 res)

    [<TestMethod>]
    member _.``addEdge - Returns error if either vertex doesn't exist``() =
        // Arrange
        let pg = PropertyGraph.empty<int, unit, int, unit>

        // Act
        let res = PropertyGraph.addEdge 1 () 1 2 pg

        // Assert
        Assert.IsTrue(Result.isError res)

    [<TestMethod>]
    member _.``addEdge - Adds another edge even if an edge between the same vertices already exists``() =
        // Arrange
        let pg =
            PropertyGraph.empty<int, unit, int, unit>
            |> PropertyGraph.addVertex 1 ()
            |> PropertyGraph.addVertex 2 ()
            |> PropertyGraph.addEdge 1 () 1 2
            |> Result.toOption
            |> Option.get

        // Act
        let res = PropertyGraph.addEdge 2 () 1 2 pg

        // Assert
        match res with
        | Error _ -> Assert.Fail("Expected Ok result")
        | Ok g ->
            Assert.AreEqual(2, PropertyGraph.countEdges g)
            Assert.IsTrue(PropertyGraph.containsEdge 2 g)
            Assert.AreEqual(2, PropertyGraph.findEdges 1 2 g |> Option.get |> Set.count)

    [<TestMethod>]
    member _.``addEdge - Adds edge to graph``() =
        // Arrange
        let pg =
            PropertyGraph.empty<int, unit, int, unit>
            |> PropertyGraph.addVertex 1 ()
            |> PropertyGraph.addVertex 2 ()

        // Act
        let res = PropertyGraph.addEdge 1 () 1 2 pg

        // Assert
        match res with
        | Error _ -> Assert.Fail("Expected Ok result")
        | Ok g ->
            Assert.AreEqual(1, PropertyGraph.countEdges g)
            Assert.IsTrue(PropertyGraph.containsEdge 1 g)

    [<TestMethod>]
    member _.``removeVertex - Does not modify graph if vertex doesn't exist``() =
        // Arrange
        let pg =
            PropertyGraph.empty<int, unit, int, unit>
            |> PropertyGraph.addVertex 1 ()

        // Act
        let res = PropertyGraph.removeVertex 2 pg

        // Assert
        Assert.AreEqual(1, PropertyGraph.countVertices res)
        Assert.IsTrue(PropertyGraph.containsVertex 1 res)

    [<TestMethod>]
    member _.``removeVertex - Removes vertex and its edges from graph``() =
        // Arrange
        let pg =
            PropertyGraph.empty<int, unit, int, unit>
            |> PropertyGraph.addVertex 1 ()
            |> PropertyGraph.addVertex 2 ()
            |> PropertyGraph.addEdge 1 () 1 2
            |> Result.toOption
            |> Option.get

        // Act
        let res = PropertyGraph.removeVertex 1 pg

        // Assert
        Assert.AreEqual(1, PropertyGraph.countVertices res)
        Assert.IsFalse(PropertyGraph.containsVertex 1 res)
        Assert.IsTrue(PropertyGraph.containsVertex 2 res)
        Assert.AreEqual(0, PropertyGraph.countEdges res)

    [<TestMethod>]
    member _.``removeEdge - Does not modify graph if edge doesn't exist``() =
        // Arrange
        let pg =
            PropertyGraph.empty<int, unit, int, unit>
            |> PropertyGraph.addVertex 1 ()
            |> PropertyGraph.addVertex 2 ()

        // Act
        let res = PropertyGraph.removeEdge 1 pg

        // Assert
        Assert.AreEqual(0, PropertyGraph.countEdges res)
        Assert.AreEqual(0, PropertyGraph.countEdges res)

    [<TestMethod>]
    member _.``removeEdge - Removes edge from graph``() =
        // Arrange
        let pg =
            PropertyGraph.empty<int, unit, int, unit>
            |> PropertyGraph.addVertex 1 ()
            |> PropertyGraph.addVertex 2 ()
            |> PropertyGraph.addEdge 1 () 1 2
            |> Result.toOption
            |> Option.get

        // Act
        let res = PropertyGraph.removeEdge 1 pg

        // Assert
        Assert.AreEqual(0, PropertyGraph.countEdges res)
        Assert.IsFalse(PropertyGraph.containsEdge 1 res)

    [<TestMethod>]
    member _.``vertices - Returns empty set if no vertices in graph``() =
        // Arrange
        let pg = PropertyGraph.empty<int, unit, int, unit>

        // Act
        let res = PropertyGraph.vertices pg

        // Assert
        Assert.AreEqual(Set.empty, res)

    [<TestMethod>]
    member _.``vertices - Returns all vertices in graph``() =
        // Arrange

        // Act
        let res = PropertyGraph.vertices pg

        // Assert
        Assert.AreEqual(Set.ofList [1, (); 2, (); 3, (); 4, (); 5, (); 6, (); 7, ()], res)

    [<TestMethod>]
    member _.``edges - Returns empty set if no edges in graph``() =
        // Arrange
        let pg = PropertyGraph.empty<int, unit, int, unit>

        // Act
        let res = PropertyGraph.edges pg

        // Assert
        Assert.AreEqual(Set.empty, res)

    [<TestMethod>]
    member _.``edges - Returns all edges in graph``() =
        // Arrange

        // Act
        let res = PropertyGraph.edges pg

        // Assert
        Assert.AreEqual(Set.ofList [1, (); 2, (); 3, (); 4, (); 5, (); 6, ()], res)

    [<TestMethod>]
    member _.``countVertices - Returns 0 for empty graph``() =
        // Arrange
        let pg = PropertyGraph.empty<int, unit, int, unit>

        // Act
        let res = PropertyGraph.countVertices pg

        // Assert
        Assert.AreEqual(0, res)

    [<TestMethod>]
    member _.``countVertices - Returns correct count for non-empty graph``() =
        // Arrange

        // Act
        let res = PropertyGraph.countVertices pg

        // Assert
        Assert.AreEqual(7, res)

    [<TestMethod>]
    member _.``countEdges - Returns 0 for empty graph``() =
        // Arrange
        let pg = PropertyGraph.empty<int, unit, int, unit>

        // Act
        let res = PropertyGraph.countEdges pg

        // Assert
        Assert.AreEqual(0, res)

    [<TestMethod>]
    member _.``countEdges - Returns correct count for non-empty graph``() =
        // Arrange

        // Act
        let res = PropertyGraph.countEdges pg

        // Assert
        Assert.AreEqual(6, res)

    [<TestMethod>]
    member _.``filterVertices - Returns empty graph if no vertices satisfy predicate``() =
        // Arrange
        let predicate = fun v _ ->
            v > 10

        // Act
        let res = PropertyGraph.filterVertices predicate pg

        // Assert
        Assert.IsTrue(PropertyGraph.isEmpty res)

    [<TestMethod>]
    member _.``filterVertices - Returns correct subgraph for vertices that satisfy predicate``() =
        // Arrange
        let predicate = fun v _ ->
            v % 2 = 1

        // Act
        let res = PropertyGraph.filterVertices predicate pg

        // Assert
        Assert.AreEqual(Set.ofList [1, (); 3, (); 5, (); 7, ()], PropertyGraph.vertices res)
        Assert.AreEqual(Set.ofList [2, ()], PropertyGraph.edges res)

    [<TestMethod>]
    member _.``filterEdges - Returns graph with all edges removed if no edges satisfy predicate``() =
        // Arrange
        let predicate = fun e _ ->
            e > 10

        // Act
        let res = PropertyGraph.filterEdges predicate pg

        // Assert
        Assert.AreEqual(0, PropertyGraph.countEdges res)

    [<TestMethod>]
    member _.``filterEdges - Returns correct subgraph for edges that satisfy predicate``() =
        // Arrange
        let predicate = fun e _ ->
            e % 2 = 0

        // Act
        let res = PropertyGraph.filterEdges predicate pg

        // Assert
        Assert.AreEqual(Set.ofList [1, (); 2, (); 3, (); 4, (); 5, (); 6, (); 7, ()], PropertyGraph.vertices res)
        Assert.AreEqual(Set.ofList [2, (); 4, (); 6, ()], PropertyGraph.edges res)

    [<TestMethod>]
    member _.``inducedSubgraph - Returns empty graph if no vertices in subset``() =
        // Arrange

        // Act
        let res = PropertyGraph.inducedSubgraph (Set.ofList [0; -1]) pg

        // Assert
        Assert.IsTrue(PropertyGraph.isEmpty res)

    [<TestMethod>]
    member _.``inducedSubgraph - Returns correct subgraph for subset of vertices``() =
        // Arrange

        // Act
        let res = PropertyGraph.inducedSubgraph (Set.ofList [1; 3; 5]) pg

        // Assert
        Assert.AreEqual(Set.ofList [1, (); 3, (); 5, ()], PropertyGraph.vertices res)
        Assert.AreEqual(Set.ofList [2, ()], PropertyGraph.edges res)

    [<TestMethod>]
    member _.``bfs - Returns empty seq if vertex does not exist``() =
        // Arrange
        let predicate = fun _ _ _ _ _ _ ->
            true

        // Act
        let res = PropertyGraph.bfs 0 predicate pg

        // Assert
        Assert.AreEqual(Seq.empty |> Seq.toList, res |> Seq.toList)

    [<TestMethod>]
    member _.``bfs - Returns empty seq if vertex has no neighbors``() =
        // Arrange
        let pg = PropertyGraph.empty<int, unit, int, unit>

        let predicate = fun _ _ _ _ _ _ ->
            true

        // Act
        let res = PropertyGraph.bfs 5 predicate pg

        // Assert
        Assert.AreEqual(Seq.empty |> Seq.toList, res |> Seq.toList)

    [<TestMethod>]
    member _.``bfs - Returns correct vertices in BFS order``() =
        // Arrange
        let predicate = fun _ _ _ _ _ _ ->
            true

        // Act
        let res = PropertyGraph.bfs 1 predicate pg

        // Assert
        Assert.AreEqual([1, (); 4, (); 3, (); 2, (); 6, (); 7, ()], res |> Seq.toList)

    [<TestMethod>]
    member _.``bfs - Returns correct vertices in BFS order with predicate removing some``() =
        // Arrange
        let predicate = fun _ _ e _ _ _ ->
            e <> 3

        // Act
        let res = PropertyGraph.bfs 1 predicate pg

        // Assert
        Assert.AreEqual([1, (); 3, (); 2, (); 6, (); 7, ()], res |> Seq.toList)

    [<TestMethod>]
    member _.``isConnected - Returns false if vertices not connected by the predicate``() =
        // Arrange
        let predicate = fun _ _ _ _ _ _ ->
            true

        // Act
        let res = PropertyGraph.isConnected 1 5 predicate pg

        // Assert
        Assert.IsFalse(res)

    [<TestMethod>]
    member _.``isConnected - Returns true if vertices connected without predicate filter``() =
        // Arrange
        let predicate = fun _ _ _ _ _ _ ->
            true

        // Act
        let res = PropertyGraph.isConnected 1 7 predicate pg

        // Assert
        Assert.IsTrue(res)

    [<TestMethod>]
    member _.``isConnected - Returns false if vertices connected but removed through predicate``() =
        // Arrange
        let predicate = fun _ _ e _ _ _ ->
            e <> 6

        // Act
        let res = PropertyGraph.isConnected 1 7 predicate pg

        // Assert
        Assert.IsFalse(res)

    [<TestMethod>]
    member _.``getVertexData - Returns None for non-existent vertex``() =
        // Arrange
        let pg =
            PropertyGraph.empty<int, string, int, string>
            |> PropertyGraph.addVertex 1 "Alice"

        // Act
        let res = PropertyGraph.getVertexData 0 pg

        // Assert
        Assert.AreEqual(None, res)

    [<TestMethod>]
    member _.``getVertexData - Returns correct data for existing vertex``() =
        // Arrange
        let pg =
            PropertyGraph.empty<int, string, int, string>
            |> PropertyGraph.addVertex 1 "Alice"
            |> PropertyGraph.addVertex 2 "Bob"

        // Act
        let res = PropertyGraph.getVertexData 1 pg

        // Assert
        Assert.AreEqual(Some "Alice", res)

    [<TestMethod>]
    member _.``getEdgeData - Returns None for non-existent edge``() =
        // Arrange
        let pg =
            PropertyGraph.empty<int, string, int, string>
            |> PropertyGraph.addVertex 1 "Alice"
            |> PropertyGraph.addVertex 2 "Bob"
            |> PropertyGraph.addEdge 1 "friends" 1 2
            |> Result.toOption
            |> Option.get

        // Act
        let res = PropertyGraph.getEdgeData 0 pg

        // Assert
        Assert.AreEqual(None, res)

    [<TestMethod>]
    member _.``getEdgeData - Returns correct data for existing edge``() =
        // Arrange
        let pg =
            PropertyGraph.empty<int, string, int, string>
            |> PropertyGraph.addVertex 1 "Alice"
            |> PropertyGraph.addVertex 2 "Bob"
            |> PropertyGraph.addEdge 1 "friends" 1 2
            |> Result.toOption
            |> Option.get

        // Act
        let res = PropertyGraph.getEdgeData 1 pg

        // Assert
        Assert.AreEqual(Some "friends", res)

    [<TestMethod>]
    member _.``setVertexData - Returns error for non-existent vertex``() =
        // Arrange
        let pg =
            PropertyGraph.empty<int, string, int, string>
            |> PropertyGraph.addVertex 1 "Alice"

        // Act
        let res = PropertyGraph.setVertexData 0 "Ghost" pg

        // Assert
        Assert.AreEqual(Error 0, res)

    [<TestMethod>]
    member _.``setVertexData - Updates data for existing vertex``() =
        // Arrange
        let pg =
            PropertyGraph.empty<int, string, int, string>
            |> PropertyGraph.addVertex 1 "old"

        // Act
        let res = PropertyGraph.setVertexData 1 "new" pg

        // Assert
        match res with
        | Error _ -> Assert.Fail("Expected Ok result")
        | Ok g ->
            Assert.AreEqual(Some "new", PropertyGraph.getVertexData 1 g)

    [<TestMethod>]
    member _.``setEdgeData - Returns error for non-existent edge``() =
        // Arrange
        let pg =
            PropertyGraph.empty<int, string, int, string>
            |> PropertyGraph.addVertex 1 "Alice"
            |> PropertyGraph.addVertex 2 "Bob"
            |> PropertyGraph.addEdge 1 "friends" 1 2
            |> Result.toOption
            |> Option.get

        // Act
        let res = PropertyGraph.setEdgeData 0 "rivals" pg

        // Assert
        Assert.AreEqual(Error 0, res)

    [<TestMethod>]
    member _.``setEdgeData - Updates data for existing edge``() =
        // Arrange
        let pg =
            PropertyGraph.empty<int, string, int, string>
            |> PropertyGraph.addVertex 1 "Alice"
            |> PropertyGraph.addVertex 2 "Bob"
            |> PropertyGraph.addEdge 1 "friends" 1 2
            |> Result.toOption
            |> Option.get

        // Act
        let res = PropertyGraph.setEdgeData 1 "rivals" pg

        // Assert
        match res with
        | Error _ -> Assert.Fail("Expected Ok result")
        | Ok g ->
            Assert.AreEqual(Some "rivals", PropertyGraph.getEdgeData 1 g)

    [<TestMethod>]
    member _.``mapVertices - Maps data for all vertices``() =
        // Arrange
        let pg =
            PropertyGraph.empty<int, int, int, int>
            |> PropertyGraph.addVertex 1 10
            |> PropertyGraph.addVertex 2 20

        // Act
        let res = PropertyGraph.mapVertices (fun _ d -> d * 2) pg

        // Assert
        Assert.AreEqual(Some 20, PropertyGraph.getVertexData 1 res)
        Assert.AreEqual(Some 40, PropertyGraph.getVertexData 2 res)

    [<TestMethod>]
    member _.``mapEdges - Maps data for all edges``() =
        // Arrange
        let pg =
            PropertyGraph.empty<int, int, int, int>
            |> PropertyGraph.addVertex 1 10
            |> PropertyGraph.addVertex 2 20
            |> PropertyGraph.addEdge 1 10 1 2
            |> Result.toOption
            |> Option.get

        // Act
        let res = PropertyGraph.mapEdges (fun _ d -> d * 2) pg

        // Assert
        Assert.AreEqual(Some 20, PropertyGraph.getEdgeData 1 res)
