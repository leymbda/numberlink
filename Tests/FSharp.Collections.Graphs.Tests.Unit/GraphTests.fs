namespace FSharp.Collections.Graphs

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type GraphTests() =
    [<TestMethod>]
    member _.``empty - Constructs empty graph``() =
        // Arrange
        
        // Act
        let graph = Graph.empty<int, string>

        // Assert
        Assert.IsEmpty graph.Vertices
        Assert.IsEmpty graph.Edges
