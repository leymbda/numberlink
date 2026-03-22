namespace FSharp.Collections.Graphs

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type GraphTests() =
    let graph =
        graph {
            let! v1 = 1
            let! v2 = 2
            let! v3 = 3
            let! v4 = 4
            let! _ = 5
            let! v6 = 6
            let! v7 = 7
            do! 1, v1, v2
            do! 2, v1, v3
            do! 3, v1, v4
            do! 4, v3, v6
            do! 5, v3, v6
            do! 6, v6, v7
        }
        |> Result.defaultWith (fun _ -> failwith "Invalid graph")

    [<TestMethod>]
    member _.``isEmpty - Returns true for empty graph``() =
        // Arrange
        let emptyGraph = Graph.empty<int, int>

        // Act
        let res = Graph.isEmpty emptyGraph

        // Assert
        Assert.IsTrue(res)

    [<TestMethod>]
    member _.``isEmpty - Returns false for non-empty graph``() =
        // Arrange

        // Act
        let res = Graph.isEmpty graph

        // Assert
        Assert.IsFalse(res)

    [<TestMethod>]
    member _.``incidentEdges - Returns None for non-existent vertex``() =
        // Arrange

        // Act
        let res = Graph.incidentEdges 0 graph

        // Assert
        Assert.AreEqual(None, res)
        
    [<TestMethod>]
    member _.``incidentEdges - Returns empty set for vertex with no edges``() =
        // Arrange

        // Act
        let res = Graph.incidentEdges 5 graph

        // Assert
        Assert.AreEqual(Some Set.empty, res)

    [<TestMethod>]
    member _.``incidentEdges - Returns correct edges for vertex``() =
        // Arrange

        // Act
        let res = Graph.incidentEdges 1 graph

        // Assert
        Assert.AreEqual(Some (Set.ofList [1; 2; 3]), res)

    [<TestMethod>]
    member _.``adjacency - Returns None for non-existent vertex``() =
        // Arrange

        // Act
        let res = Graph.adjacency 0 graph

        // Assert
        Assert.AreEqual(None, res)
        
    [<TestMethod>]
    member _.``adjacency - Returns empty set for vertex with no edges``() =
        // Arrange

        // Act
        let res = Graph.adjacency 5 graph

        // Assert
        Assert.AreEqual(Some Set.empty, res)

    [<TestMethod>]
    member _.``adjacency - Returns correct neighbors for vertex``() =
        // Arrange

        // Act
        let res = Graph.adjacency 1 graph

        // Assert
        Assert.AreEqual(Some (Set.ofList [1, 2; 2, 3; 3, 4]), res)

    [<TestMethod>]
    member _.``neighbors - Returns None for non-existent vertex``() =
        // Arrange

        // Act
        let res = Graph.neighbors 0 graph

        // Assert
        Assert.AreEqual(None, res)
        
    [<TestMethod>]
    member _.``neighbors - Returns empty set for vertex with no edges``() =
        // Arrange

        // Act
        let res = Graph.neighbors 5 graph

        // Assert
        Assert.AreEqual(Some Set.empty, res)

    [<TestMethod>]
    member _.``neighbors - Returns correct neighbors for vertex``() =
        // Arrange

        // Act
        let res = Graph.neighbors 1 graph

        // Assert
        Assert.AreEqual(Some (Set.ofList [2; 3; 4]), res)

    [<TestMethod>]
    member _.``degree - Returns None for non-existent vertex``() =
        // Arrange

        // Act
        let res = Graph.degree 0 graph

        // Assert
        Assert.AreEqual(None, res)
        
    [<TestMethod>]
    member _.``degree - Returns 0 for vertex with no edges``() =
        // Arrange

        // Act
        let res = Graph.degree 5 graph

        // Assert
        Assert.AreEqual(Some 0, res)

    [<TestMethod>]
    member _.``degree - Returns correct degree for vertex``() =
        // Arrange

        // Act
        let res = Graph.degree 1 graph

        // Assert
        Assert.AreEqual(Some 3, res)

    [<TestMethod>]
    member _.``containsVertex - Returns false for non-existent vertex``() =
        // Arrange

        // Act
        let res = Graph.containsVertex 0 graph

        // Assert
        Assert.IsFalse(res)
        
    [<TestMethod>]
    member _.``containsVertex - Returns true for existing vertex``() =
        // Arrange

        // Act
        let res = Graph.containsVertex 2 graph

        // Assert
        Assert.IsTrue(res)

    [<TestMethod>]
    member _.``containsEdge - Returns false for non-existent edge``() =
        // Arrange

        // Act
        let res = Graph.containsEdge 0 graph

        // Assert
        Assert.IsFalse(res)
        
    [<TestMethod>]
    member _.``containsEdge - Returns true for existing edge``() =
        // Arrange

        // Act
        let res = Graph.containsEdge 1 graph

        // Assert
        Assert.IsTrue(res)

    [<TestMethod>]
    member _.``getEndpoints - Returns None for non-existent edge``() =
        // Arrange

        // Act
        let res = Graph.getEndpoints 0 graph

        // Assert
        Assert.AreEqual(None, res)
        
    [<TestMethod>]
    member _.``getEndpoints - Returns correct endpoints for existing edge``() =
        // Arrange

        // Act
        let res = Graph.getEndpoints 1 graph

        // Assert
        Assert.AreEqual(Some (1, 2), res)

    [<TestMethod>]
    member _.``isAdjacent - Returns None if either vertex doesn't exist``() =
        // Arrange

        // Act
        let res = Graph.isAdjacent 0 1 graph

        // Assert
        Assert.AreEqual(None, res)
        
    [<TestMethod>]
    member _.``isAdjacent - Returns false for non-adjacent vertices``() =
        // Arrange

        // Act
        let res = Graph.isAdjacent 2 4 graph

        // Assert
        Assert.AreEqual(Some false, res)

    [<TestMethod>]
    member _.``isAdjacent - Returns true for adjacent vertices``() =
        // Arrange

        // Act
        let res = Graph.isAdjacent 1 2 graph

        // Assert
        Assert.AreEqual(Some true, res)

    [<TestMethod>]
    [<DataRow(0, 1)>]
    [<DataRow(1, 0)>]
    [<DataRow(0, -1)>]
    member _.``findEdges - Returns None if either or both vertices don't exist``(v1: int, v2: int) =
        // Arrange

        // Act
        let res = Graph.findEdges v1 v2 graph

        // Assert
        Assert.AreEqual(None, res)
        
    [<TestMethod>]
    member _.``findEdges - Returns empty set for non-adjacent vertices``() =
        // Arrange

        // Act
        let res = Graph.findEdges 2 4 graph

        // Assert
        Assert.AreEqual(Some Set.empty, res)

    [<TestMethod>]
    member _.``findEdges - Returns correct edges for adjacent vertices``() =
        // Arrange

        // Act
        let res = Graph.findEdges 3 6 graph

        // Assert
        Assert.AreEqual(Some (Set.ofList [4; 5]), res)

    [<TestMethod>]
    member _.``addVertex - Does not modify graph if vertex already exists``() =
        // Arrange
        let graph =
            Graph.empty<int, int>
            |> Graph.addVertex 1

        // Act
        let res = Graph.addVertex 1 graph

        // Assert
        Assert.AreEqual(1, Graph.countVertices res)
        Assert.IsTrue(Graph.containsVertex 1 res)

    [<TestMethod>]
    member _.``addVertex - Adds vertex to graph``() =
        // Arrange
        let graph = Graph.empty<int, int>

        // Act
        let res = Graph.addVertex 1 graph

        // Assert
        Assert.AreEqual(1, Graph.countVertices res)
        Assert.IsTrue(Graph.containsVertex 1 res)

    [<TestMethod>]
    member _.``addEdge - Returns error if either vertex doesn't exist``() =
        // Arrange
        let graph = Graph.empty<int, int>

        // Act
        let res = Graph.addEdge 1 1 2 graph

        // Assert
        Assert.IsTrue(Result.isError res)

    [<TestMethod>]
    member _.``addEdge - Adds another edge even if an edge between the same vertices already exists``() =
        // Arrange
        let graph =
            Graph.empty<int, int>
            |> Graph.addVertex 1
            |> Graph.addVertex 2
            |> Graph.addEdge 1 1 2
            |> Result.toOption
            |> Option.get

        // Act
        let res = Graph.addEdge 2 1 2 graph

        // Assert
        match res with
        | Error _ -> Assert.Fail("Expected Ok result")
        | Ok g ->
            Assert.AreEqual(2, Graph.countEdges g)
            Assert.IsTrue(Graph.containsEdge 2 g)
            Assert.AreEqual(2, Graph.findEdges 1 2 g |> Option.get |> Set.count)

    [<TestMethod>]
    member _.``addEdge - Adds edge to graph``() =
        // Arrange
        let graph =
            Graph.empty<int, int>
            |> Graph.addVertex 1
            |> Graph.addVertex 2

        // Act
        let res = Graph.addEdge 1 1 2 graph

        // Assert
        match res with
        | Error _ -> Assert.Fail("Expected Ok result")
        | Ok g ->
            Assert.AreEqual(1, Graph.countEdges g)
            Assert.IsTrue(Graph.containsEdge 1 g)

    [<TestMethod>]
    member _.``removeVertex - Does not modify graph if vertex doesn't exist``() =
        // Arrange
        let graph =
            Graph.empty<int, int>
            |> Graph.addVertex 1

        // Act
        let res = Graph.removeVertex 2 graph

        // Assert
        Assert.AreEqual(1, Graph.countVertices res)
        Assert.IsTrue(Graph.containsVertex 1 res)

    [<TestMethod>]
    member _.``removeVertex - Removes vertex and its edges from graph``() =
        // Arrange
        let graph =
            Graph.empty<int, int>
            |> Graph.addVertex 1
            |> Graph.addVertex 2
            |> Graph.addEdge 1 1 2
            |> Result.toOption
            |> Option.get

        // Act
        let res = Graph.removeVertex 1 graph

        // Assert
        Assert.AreEqual(1, Graph.countVertices res)
        Assert.IsFalse(Graph.containsVertex 1 res)
        Assert.IsTrue(Graph.containsVertex 2 res)
        Assert.AreEqual(0, Graph.countEdges res)

    [<TestMethod>]
    member _.``removeEdge - Does not modify graph if edge doesn't exist``() =
        // Arrange
        let graph =
            Graph.empty<int, int>
            |> Graph.addVertex 1
            |> Graph.addVertex 2

        // Act
        let res = Graph.removeEdge 1 graph

        // Assert
        Assert.AreEqual(0, Graph.countEdges res)
        Assert.AreEqual(0, Graph.countEdges res)

    [<TestMethod>]
    member _.``removeEdge - Removes edge from graph``() =
        // Arrange
        let graph =
            Graph.empty<int, int>
            |> Graph.addVertex 1
            |> Graph.addVertex 2
            |> Graph.addEdge 1 1 2
            |> Result.toOption
            |> Option.get

        // Act
        let res = Graph.removeEdge 1 graph

        // Assert
        Assert.AreEqual(0, Graph.countEdges res)
        Assert.IsFalse(Graph.containsEdge 1 res)

    [<TestMethod>]
    member _.``vertices - Returns empty seq if no vertices in graph``() =
        // Arrange
        let graph = Graph.empty<int, int>

        // Act
        let res = Graph.vertices graph

        // Assert
        Assert.AreEqual(Set.empty, res)

    [<TestMethod>]
    member _.``vertices - Returns all vertices in graph``() =
        // Arrange

        // Act
        let res = Graph.vertices graph

        // Assert
        Assert.AreEqual(Set.ofList [1; 2; 3; 4; 5; 6; 7], res)

    [<TestMethod>]
    member _.``edges - Returns empty seq if no edges in graph``() =
        // Arrange
        let graph = Graph.empty<int, int>

        // Act
        let res = Graph.edges graph

        // Assert
        Assert.AreEqual(Set.empty, res)

    [<TestMethod>]
    member _.``edges - Returns all edges in graph``() =
        // Arrange

        // Act
        let res = Graph.edges graph

        // Assert
        Assert.AreEqual(Set.ofList [1; 2; 3; 4; 5; 6], res)

    [<TestMethod>]
    member _.``countVertices - Returns 0 for empty graph``() =
        // Arrange
        let graph = Graph.empty<int, int>

        // Act
        let res = Graph.countVertices graph

        // Assert
        Assert.AreEqual(0, res)

    [<TestMethod>]
    member _.``countVertices - Returns correct count for non-empty graph``() =
        // Arrange

        // Act
        let res = Graph.countVertices graph

        // Assert
        Assert.AreEqual(7, res)

    [<TestMethod>]
    member _.``countEdges - Returns 0 for empty graph``() =
        // Arrange
        let graph = Graph.empty<int, int>

        // Act
        let res = Graph.countEdges graph

        // Assert
        Assert.AreEqual(0, res)

    [<TestMethod>]
    member _.``countEdges - Returns correct count for non-empty graph``() =
        // Arrange

        // Act
        let res = Graph.countEdges graph

        // Assert
        Assert.AreEqual(6, res)

    [<TestMethod>]
    member _.``filterVertices - Returns empty graph if no vertices satisfy predicate``() =
        // Arrange
        let predicate = fun v ->
            v > 10

        // Act
        let res = Graph.filterVertices predicate graph

        // Assert
        Assert.IsTrue(Graph.isEmpty res)

    [<TestMethod>]
    member _.``filterVertices - Returns correct subgraph for vertices that satisfy predicate``() =
        // Arrange
        let predicate = fun v ->
            v % 2 = 1

        // Act
        let res = Graph.filterVertices predicate graph

        // Assert
        Assert.AreEqual(Set.ofList [1; 3; 5; 7], Graph.vertices res)
        Assert.AreEqual(Set.ofList [2], Graph.edges res)

    [<TestMethod>]
    member _.``filterEdges - Returns graph with all edges removed if no edges satisfy predicate``() =
        // Arrange
        let predicate = fun e ->
            e > 10

        // Act
        let res = Graph.filterEdges predicate graph

        // Assert
        Assert.AreEqual(0, Graph.countEdges res)

    [<TestMethod>]
    member _.``filterEdges - Returns correct subgraph for edges that satisfy predicate``() =
        // Arrange
        let predicate = fun e ->
            e % 2 = 0

        // Act
        let res = Graph.filterEdges predicate graph

        // Assert
        Assert.AreEqual(Set.ofList [1; 2; 3; 4; 5; 6; 7], Graph.vertices res)
        Assert.AreEqual(Set.ofList [2; 4; 6], Graph.edges res)

    [<TestMethod>]
    member _.``inducedSubgraph - Returns empty graph if no vertices in subset``() =
        // Arrange

        // Act
        let res = Graph.inducedSubgraph (Set.ofList [0; -1]) graph

        // Assert
        Assert.IsTrue(Graph.isEmpty res)

    [<TestMethod>]
    member _.``inducedSubgraph - Returns correct subgraph for subset of vertices``() =
        // Arrange

        // Act
        let res = Graph.inducedSubgraph (Set.ofList [1; 3; 5]) graph

        // Assert
        Assert.AreEqual(Set.ofList [1; 3; 5], Graph.vertices res)
        Assert.AreEqual(Set.ofList [2], Graph.edges res)
        
    [<TestMethod>]
    member _.``bfs - Returns empty seq if vertex does not exist``() =
        // Arrange
        let predicate = fun _ _ _ ->
            true

        // Act
        let res = Graph.bfs 0 predicate graph

        // Assert
        Assert.AreEqual(Seq.empty |> Seq.toList, res |> Seq.toList)
        
    [<TestMethod>]
    member _.``bfs - Returns empty seq if vertex has no neighbors``() =
        // Arrange
        let graph = Graph.empty<int, int>

        let predicate = fun _ _ _ ->
            true

        // Act
        let res = Graph.bfs 5 predicate graph

        // Assert
        Assert.AreEqual(Seq.empty |> Seq.toList, res |> Seq.toList)

    [<TestMethod>]
    member _.``bfs - Returns correct vertices in BFS order``() =
        // Arrange
        let predicate = fun _ _ _ ->
            true

        // Act
        let res = Graph.bfs 1 predicate graph

        // Assert
        Assert.AreEqual(Seq.ofList [1; 4; 3; 2; 6; 7] |> Seq.toList, res |> Seq.toList)

    [<TestMethod>]
    member _.``bfs - Returns correct vertices in BFS order with predicate removing some``() =
        // Arrange
        let predicate = fun _ e _ ->
            e <> 3

        // Act
        let res = Graph.bfs 1 predicate graph

        // Assert
        Assert.AreEqual(Seq.ofList [1; 3; 2; 6; 7] |> Seq.toList, res |> Seq.toList)

    [<TestMethod>]
    member _.``isConnected - Returns false if vertices not connected by the predicate``() =
        // Arrange
        let predicate = fun _ _ _ ->
            true

        // Act
        let res = Graph.isConnected 1 5 predicate graph

        // Assert
        Assert.IsFalse(res)

    [<TestMethod>]
    member _.``isConnected - Returns true if vertices connected without predicate filter``() =
        // Arrange
        let predicate = fun _ _ _ ->
            true

        // Act
        let res = Graph.isConnected 1 7 predicate graph

        // Assert
        Assert.IsTrue(res)

    [<TestMethod>]
    member _.``isConnected - Returns false if vertices connected but removed through predicate``() =
        // Arrange
        let predicate = fun _ e _ ->
            e <> 6

        // Act
        let res = Graph.isConnected 1 7 predicate graph

        // Assert
        Assert.IsFalse(res)
