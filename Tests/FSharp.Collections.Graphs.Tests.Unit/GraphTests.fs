namespace FSharp.Collections.Graphs

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type GraphTests() =
    let graph =
        Graph.empty<int, int>
        |> Graph.addVertex 1
        |> Graph.addVertex 2
        |> Graph.addVertex 3
        |> Graph.addVertex 4
        |> Graph.addVertex 5
        |> Graph.addVertex 6
        |> Graph.addEdge 1 1 2
        |> Result.bind (Graph.addEdge 2 1 3)
        |> Result.bind (Graph.addEdge 3 1 4)
        |> Result.bind (Graph.addEdge 4 3 6)
        |> Result.bind (Graph.addEdge 5 3 6)
        |> Result.defaultWith (fun _ -> failwith "Invalid graph")

        // TODO: Computation expression to build graphs (base off TemplateBuilder, consider decide/evolve pattern)

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

    // TODO: Tests for remaining functions
