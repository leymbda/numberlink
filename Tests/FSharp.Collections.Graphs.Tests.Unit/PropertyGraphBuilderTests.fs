namespace FSharp.Collections.Graphs

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type PropertyGraphBuilderTests() =
    [<TestMethod>]
    member _.``Returns VertexAlreadyExists error if vertex is added twice``() =
        // Arrange

        // Act
        let res = propertyGraph<int, unit, int, unit> {
            let! v1 = 1, ()
            let! v2 = 2, ()
            let! _ = 1, ()
            do! 1, (), v1, v2
        }
        
        // Assert
        match res with
        | Error (PropertyGraphBuilderError.VertexAlreadyExists v) -> Assert.AreEqual(1, v)
        | _ -> Assert.Fail("Expected VertexAlreadyExists error")
        
    [<TestMethod>]
    member _.``Returns EdgeAlreadyExists error if edge is added twice``() =
        // Arrange

        // Act
        let res = propertyGraph<int, unit, int, unit> {
            let! v1 = 1, ()
            let! v2 = 2, ()
            do! 1, (), v1, v2
            do! 1, (), v1, v2
        }
        
        // Assert
        match res with
        | Error (PropertyGraphBuilderError.EdgeAlreadyExists e) -> Assert.AreEqual(1, e)
        | _ -> Assert.Fail("Expected EdgeAlreadyExists error")
        
    [<TestMethod>]
    member _.``Returns InvalidEdgeConnection error if edge is added with both non-existent vertices``() =
        // Arrange

        // Act
        let res =  propertyGraph<int, unit, int, unit> {
            do! 1, (), 1, 2
        }
        
        // Assert
        match res with
        | Error (PropertyGraphBuilderError.InvalidEdgeConnection (e, missing)) ->
            Assert.AreEqual(1, e)
            Assert.IsTrue(missing.Contains(1))
            Assert.IsTrue(missing.Contains(2))
            Assert.AreEqual(2, Set.count missing)
        | _ -> Assert.Fail("Expected InvalidEdgeConnection error")
        
    [<TestMethod>]
    member _.``Returns InvalidEdgeConnection error if edge is added with first vertex non-existent``() =
        // Arrange

        // Act
        let res = propertyGraph<int, unit, int, unit> {
            let! v2 = 2, ()
            do! 1, (), 1, v2
        }
        
        // Assert
        match res with
        | Error (PropertyGraphBuilderError.InvalidEdgeConnection (e, missing)) ->
            Assert.AreEqual(1, e)
            Assert.IsTrue(missing.Contains(1))
            Assert.IsFalse(missing.Contains(2))
            Assert.AreEqual(1, Set.count missing)
        | _ -> Assert.Fail("Expected InvalidEdgeConnection error")
        
    [<TestMethod>]
    member _.``Returns InvalidEdgeConnection error if edge is added with second vertex non-existent``() =
        // Arrange

        // Act
        let res = propertyGraph<int, unit, int, unit> {
            let! v1 = 1, ()
            do! 1, (), v1, 2
        }
        
        // Assert
        match res with
        | Error (PropertyGraphBuilderError.InvalidEdgeConnection (e, missing)) ->
            Assert.AreEqual(1, e)
            Assert.IsFalse(missing.Contains(1))
            Assert.IsTrue(missing.Contains(2))
            Assert.AreEqual(1, Set.count missing)
        | _ -> Assert.Fail("Expected InvalidEdgeConnection error")
        
    [<TestMethod>]
    member _.``Successfully builds a graph with valid vertices and edges``() =
        // Arrange

        // Act
        let res = propertyGraph<int, unit, int, unit> {
            let! v1 = 1, ()
            let! v2 = 2, ()
            do! 1, (), v1, v2
        }
        
        // Assert
        match res with
        | Error _ -> Assert.Fail("Expected successful graph construction")
        | Ok pg ->
            Assert.IsTrue(PropertyGraph.containsVertex 1 pg)
            Assert.IsTrue(PropertyGraph.containsVertex 2 pg)
            Assert.IsTrue(PropertyGraph.containsEdge 1 pg)
