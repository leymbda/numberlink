namespace Numberlink.Solver

open FSharp.Collections.Graphs
open FsToolkit.ErrorHandling
open Microsoft.VisualStudio.TestTools.UnitTesting
open Numberlink.Core

[<TestClass>]
type SmtSolverTests() =
    let puzzleFromGraph graph = {
        Positions = Map.empty
        Graph = Result.defaultWith (fun _ -> failwith "Invalid graph") graph
    }

    let assertUnsatisfiable res =
        match res with
        | Error (SmtSolverError.Unsatisfiable) -> ()
        | res -> Assert.Fail($"Expected unsatisfiable result, but instead got {res}")

    let assertSatisfiable res =
        match res with
        | Ok _ -> ()
        | res -> Assert.Fail($"Expected satisfiable result, but instead got {res}")
        
    [<TestMethod>]
    member _.``Degree - Terminals with too few expected neighbors (0) should be unsatisfiable``() =
        // Arrange
        let graph = propertyGraph {
            let! _ = Vertex 1, PuzzleVertex.Terminal (Line 1)
            return ()

            // A single terminal with no neighbors. This should be unsatisfiable because terminals must have degree 1.
        }

        // Act
        let res = SmtSolver.solve (puzzleFromGraph graph)

        // Assert
        assertUnsatisfiable res

    [<TestMethod>]
    member _.``Degree - Terminals with too many expected neighbors (2) should be unsatisfiable``() =
        // Arrange
        let graph = propertyGraph {
            let! v1 = Vertex 1, PuzzleVertex.Terminal (Line 1)
            let! v2 = Vertex 2, PuzzleVertex.Terminal (Line 1)
            let! v3 = Vertex 3, PuzzleVertex.Terminal (Line 1)
            do! Edge 1, PuzzleEdge.Segment, v1, v2
            do! Edge 2, PuzzleEdge.Segment, v2, v3

            // A row of three terminals. This should be unsatisfiable as it expect the middle terminal to connect to
            // both, resulting in it having degree 2.
        }

        // Act
        let res = SmtSolver.solve (puzzleFromGraph graph)

        // Assert
        assertUnsatisfiable res

    [<TestMethod>]
    member _.``Degree - Cells with too few expected neighbors (1) should be unsatisfiable``() =
        // Arrange
        let graph = propertyGraph {
            let! v1 = Vertex 1, PuzzleVertex.Terminal (Line 1)
            let! v2 = Vertex 2, PuzzleVertex.Cell
            do! Edge 1, PuzzleEdge.Segment, v1, v2

            // A terminal neighboring a cell. This should be unsatisfiable because the cell does not neighbor a second
            // vertex, meaning it has degree 1.
        }

        // Act
        let res = SmtSolver.solve (puzzleFromGraph graph)

        // Assert
        assertUnsatisfiable res

    [<TestMethod>]
    member _.``Degree - Cells with too many expected neighbors (3) should be unsatisfiable``() =
        // Arrange
        let graph = propertyGraph {
            let! v1 = Vertex 1, PuzzleVertex.Cell
            let! v2 = Vertex 2, PuzzleVertex.Terminal (Line 1)
            let! v3 = Vertex 3, PuzzleVertex.Terminal (Line 1)
            let! v4 = Vertex 4, PuzzleVertex.Terminal (Line 1)
            do! Edge 1, PuzzleEdge.Segment, v1, v2
            do! Edge 2, PuzzleEdge.Segment, v1, v3
            do! Edge 3, PuzzleEdge.Segment, v1, v4

            // A cell neighboring 3 terminals. This should be unsatisfiable because the cell is expected to have degree
            // 3.
        }

        // Act
        let res = SmtSolver.solve (puzzleFromGraph graph)

        // Assert
        assertUnsatisfiable res

    [<TestMethod>]
    member _.``Bridges - Unmatched lane pairs are unsatisfiable``() =
        // Arrange
        let graph = propertyGraph {
            let! v1 = Vertex 1, PuzzleVertex.Bridge
            let! v2 = Vertex 2, PuzzleVertex.Terminal (Line 1)
            let! v3 = Vertex 3, PuzzleVertex.Terminal (Line 1)
            let! v4 = Vertex 4, PuzzleVertex.Terminal (Line 2)
            let! v5 = Vertex 5, PuzzleVertex.Terminal (Line 2)
            do! Edge 1, PuzzleEdge.BridgeLane (Lane 1), v1, v2
            do! Edge 2, PuzzleEdge.BridgeLane (Lane 2), v1, v3
            do! Edge 3, PuzzleEdge.BridgeLane (Lane 1), v1, v4
            do! Edge 4, PuzzleEdge.BridgeLane (Lane 2), v1, v5

            // A cross shape where the vertices connect to a bridge at the center, and the lanes are jumbled to not
            // correspond to the appropriate terminals. This should be unsatisfiable because the lanes do not pair up
            // terminals of the same line.
        }

        // Act
        let res = SmtSolver.solve (puzzleFromGraph graph)

        // Assert
        assertUnsatisfiable res
        
    [<TestMethod>]
    member _.``Bridges - A line passing through a bridge twice should be unsatisfiable``() =
        // Arrange
        let graph = propertyGraph {
            let! v1 = Vertex 1, PuzzleVertex.Terminal (Line 1)
            let! v2 = Vertex 2, PuzzleVertex.Bridge
            let! v3 = Vertex 3, PuzzleVertex.Cell
            let! v4 = Vertex 4, PuzzleVertex.Cell
            let! v5 = Vertex 5, PuzzleVertex.Terminal (Line 1)
            do! Edge 1, PuzzleEdge.BridgeLane (Lane 1), v1, v2
            do! Edge 2, PuzzleEdge.BridgeLane (Lane 1), v2, v3
            do! Edge 3, PuzzleEdge.Segment, v3, v4
            do! Edge 5, PuzzleEdge.BridgeLane (Lane 2), v4, v2
            do! Edge 6, PuzzleEdge.BridgeLane (Lane 2), v2, v5

            // A single line starts at a terminal, goes through a bridge lane, then loops around to re-enter the bridge
            // for a second time. This should be unsatisfiable because the line cannot pass through the same bridge
            // twice.
        }

        // Act
        let res = SmtSolver.solve (puzzleFromGraph graph)

        // Assert
        assertUnsatisfiable res
        
    [<TestMethod>]
    member _.``Bridges - Unused bridge lanes should be unsatisfiable``() =
        // Arrange
        let graph = propertyGraph {
            let! v1 = Vertex 1, PuzzleVertex.Terminal (Line 1)
            let! v2 = Vertex 2, PuzzleVertex.Cell
            let! v3 = Vertex 3, PuzzleVertex.Terminal (Line 1)
            let! v4 = Vertex 4, PuzzleVertex.Terminal (Line 2)
            let! v5 = Vertex 5, PuzzleVertex.Bridge
            let! v6 = Vertex 6, PuzzleVertex.Terminal (Line 2)
            let! v7 = Vertex 7, PuzzleVertex.Terminal (Line 3)
            let! v8 = Vertex 8, PuzzleVertex.Cell
            let! v9 = Vertex 9, PuzzleVertex.Terminal (Line 3)
            do! Edge 1, PuzzleEdge.Segment, v1, v2
            do! Edge 2, PuzzleEdge.Segment, v2, v3
            do! Edge 3, PuzzleEdge.BridgeLane (Lane 1), v5, v4
            do! Edge 4, PuzzleEdge.BridgeLane (Lane 1), v5, v6
            do! Edge 5, PuzzleEdge.BridgeLane (Lane 2), v5, v2
            do! Edge 6, PuzzleEdge.BridgeLane (Lane 2), v5, v8
            do! Edge 7, PuzzleEdge.Segment, v7, v8
            do! Edge 8, PuzzleEdge.Segment, v8, v9

            // Three parallel lines in a 3x3 grid with a bridge in the middle. This should be unsatisfiable because the
            // vertical bridge lane is unused in order for the lines to connect the terminals.
        }

        // Act
        let res = SmtSolver.solve (puzzleFromGraph graph)

        // Assert
        assertUnsatisfiable res

    [<TestMethod>]
    member _.``Lines - Terminals/cells that connect to two vertices of a different lines should be unsatisfiable``() =
        // Arrange
        let graph = propertyGraph {
            let! v1 = Vertex 1, PuzzleVertex.Terminal (Line 1)
            let! v2 = Vertex 2, PuzzleVertex.Cell
            let! v3 = Vertex 3, PuzzleVertex.Terminal (Line 2)
            do! Edge 1, PuzzleEdge.Segment, v1, v2
            do! Edge 2, PuzzleEdge.Segment, v2, v3

            // A cell neighboring two terminals of different lines. This should be unsatisfiable because the cell
            // cannot connect terminals for different lines.
        }

        // Act
        let res = SmtSolver.solve (puzzleFromGraph graph)

        // Assert
        assertUnsatisfiable res

    [<TestMethod>]
    member _.``Lines - A puzzle containing unusable cells should be unsatisfiable``() =
        // Arrange
        let graph = propertyGraph {
            let! v1 = Vertex 1, PuzzleVertex.Terminal (Line 1)
            let! v2 = Vertex 2, PuzzleVertex.Cell
            let! v3 = Vertex 3, PuzzleVertex.Terminal (Line 1)
            let! v4 = Vertex 4, PuzzleVertex.Cell
            do! Edge 1, PuzzleEdge.Segment, v1, v2
            do! Edge 2, PuzzleEdge.Segment, v2, v3
            do! Edge 3, PuzzleEdge.Segment, v2, v4

            // A line connects two terminals, but also has an extra cell neighboring it. This extra cell cannot be
            // filled making it unusable, which should not be satisfiable as all cells must be filled.
        }

        // Act
        let res = SmtSolver.solve (puzzleFromGraph graph)

        // Assert
        assertUnsatisfiable res

    [<TestMethod>]
    member _.``Lines - A slitherable line fill should be unsatisfiable``() =
        // Arrange
        let graph = propertyGraph {
            let! v1 = Vertex 1, PuzzleVertex.Terminal (Line 1)
            let! v2 = Vertex 2, PuzzleVertex.Cell
            let! v3 = Vertex 3, PuzzleVertex.Cell
            let! v4 = Vertex 4, PuzzleVertex.Cell
            let! v5 = Vertex 5, PuzzleVertex.Cell
            let! v6 = Vertex 6, PuzzleVertex.Terminal (Line 1)
            do! Edge 1, PuzzleEdge.Segment, v1, v2
            do! Edge 2, PuzzleEdge.Segment, v2, v3
            do! Edge 3, PuzzleEdge.Segment, v3, v4
            do! Edge 4, PuzzleEdge.Segment, v4, v5
            do! Edge 5, PuzzleEdge.Segment, v5, v6
            do! Edge 6, PuzzleEdge.Segment, v2, v5

            // A line that has to slither past itself in order to fill the board. Imagine a cartesian board that
            // follows the path Right, Down, Right, Up, Right to get from one terminal to the other. This should be
            // unsatisfiable because the second and fifth vertices (cells) touch each other as well. Board filling is
            // necessitated by degree constraints on vertices, meaning the shortest path that leave unused vertices is
            // also invalid.
        }

        // Act
        let res = SmtSolver.solve (puzzleFromGraph graph)

        // Assert
        assertUnsatisfiable res

    [<TestMethod>]
    [<Ignore "Requires rank constraints not yet implemented">]
    member _.``Lines - A line fill that can be done with a shadow cycle should not be satisfiable``() =
        // Arrange
        let graph = propertyGraph {
            let! v1 = Vertex 1, PuzzleVertex.Terminal (Line 1)
            let! v2 = Vertex 2, PuzzleVertex.Cell
            let! v3 = Vertex 3, PuzzleVertex.Terminal (Line 1)
            let! v4 = Vertex 4, PuzzleVertex.Terminal (Line 2)
            let! v5 = Vertex 5, PuzzleVertex.Cell
            let! v6 = Vertex 6, PuzzleVertex.Terminal (Line 2)
            let! v7 = Vertex 7, PuzzleVertex.Cell
            let! v8 = Vertex 8, PuzzleVertex.Cell
            let! v9 = Vertex 9, PuzzleVertex.Cell
            let! v10 = Vertex 10, PuzzleVertex.Cell
            do! Edge 1, PuzzleEdge.Segment, v1, v2
            do! Edge 2, PuzzleEdge.Segment, v2, v3
            do! Edge 3, PuzzleEdge.Segment, v4, v5
            do! Edge 4, PuzzleEdge.Segment, v5, v6
            do! Edge 5, PuzzleEdge.Segment, v7, v8
            do! Edge 6, PuzzleEdge.Segment, v8, v9
            do! Edge 7, PuzzleEdge.Segment, v1, v4
            do! Edge 8, PuzzleEdge.Segment, v2, v5
            do! Edge 9, PuzzleEdge.Segment, v3, v6
            do! Edge 10, PuzzleEdge.Segment, v4, v7
            do! Edge 11, PuzzleEdge.Segment, v5, v8
            do! Edge 12, PuzzleEdge.Segment, v6, v9
            do! Edge 13, PuzzleEdge.Segment, v7, v10
            do! Edge 14, PuzzleEdge.Segment, v9, v10
            
            // Two parallel lines stack each other, with an additional three cells connecting to form a 3x3 grid, plus
            // an additional cell connecting the two sides of the cell line (to form a non cartesian 4 cell cycle).
            // While the second line cannot slither to fill this shape, a shadow cycle would be able to, but that
            // should be unsatisfiable as well.
        }

        // Act
        let res = SmtSolver.solve (puzzleFromGraph graph)

        // Assert
        assertUnsatisfiable res

    [<TestMethod>]
    member _.``Valid classic puzzle is satisfiable``() =
        // Arrange
        let graph = propertyGraph {
            // Lines
            let A = 1
            let B = 2
            let C = 3
            let D = 4
            let E = 5

            // Vertices
            let! v0_0 = Vertex 1, PuzzleVertex.Terminal (Line A)
            let! v1_0 = Vertex 6, PuzzleVertex.Cell
            let! v2_0 = Vertex 11, PuzzleVertex.Cell
            let! v3_0 = Vertex 16, PuzzleVertex.Cell
            let! v4_0 = Vertex 21, PuzzleVertex.Cell
            let! v0_1 = Vertex 2, PuzzleVertex.Cell
            let! v1_1 = Vertex 7, PuzzleVertex.Cell
            let! v2_1 = Vertex 12, PuzzleVertex.Cell
            let! v3_1 = Vertex 17, PuzzleVertex.Terminal (Line B)
            let! v4_1 = Vertex 22, PuzzleVertex.Terminal (Line A)
            let! v0_2 = Vertex 3, PuzzleVertex.Terminal (Line B)
            let! v1_2 = Vertex 8, PuzzleVertex.Terminal (Line C)
            let! v2_2 = Vertex 13, PuzzleVertex.Cell
            let! v3_2 = Vertex 18, PuzzleVertex.Cell
            let! v4_2 = Vertex 23, PuzzleVertex.Terminal (Line C)
            let! v0_3 = Vertex 4, PuzzleVertex.Cell
            let! v1_3 = Vertex 9, PuzzleVertex.Cell
            let! v2_3 = Vertex 14, PuzzleVertex.Cell
            let! v3_3 = Vertex 19, PuzzleVertex.Terminal (Line D)
            let! v4_3 = Vertex 24, PuzzleVertex.Terminal (Line E)
            let! v0_4 = Vertex 5, PuzzleVertex.Terminal (Line D)
            let! v1_4 = Vertex 10, PuzzleVertex.Terminal (Line E)
            let! v2_4 = Vertex 15, PuzzleVertex.Cell
            let! v3_4 = Vertex 20, PuzzleVertex.Cell
            let! v4_4 = Vertex 25, PuzzleVertex.Cell

            // Horizontal edges
            do! Edge 1, PuzzleEdge.Segment, v0_0, v1_0
            do! Edge 2, PuzzleEdge.Segment, v1_0, v2_0
            do! Edge 3, PuzzleEdge.Segment, v2_0, v3_0
            do! Edge 4, PuzzleEdge.Segment, v3_0, v4_0
            do! Edge 5, PuzzleEdge.Segment, v0_1, v1_1
            do! Edge 6, PuzzleEdge.Segment, v1_1, v2_1
            do! Edge 7, PuzzleEdge.Segment, v2_1, v3_1
            do! Edge 8, PuzzleEdge.Segment, v3_1, v4_1
            do! Edge 9, PuzzleEdge.Segment, v0_2, v1_2
            do! Edge 10, PuzzleEdge.Segment, v1_2, v2_2
            do! Edge 11, PuzzleEdge.Segment, v2_2, v3_2
            do! Edge 12, PuzzleEdge.Segment, v3_2, v4_2
            do! Edge 13, PuzzleEdge.Segment, v0_3, v1_3
            do! Edge 14, PuzzleEdge.Segment, v1_3, v2_3
            do! Edge 15, PuzzleEdge.Segment, v2_3, v3_3
            do! Edge 16, PuzzleEdge.Segment, v3_3, v4_3
            do! Edge 17, PuzzleEdge.Segment, v0_4, v1_4
            do! Edge 18, PuzzleEdge.Segment, v1_4, v2_4
            do! Edge 19, PuzzleEdge.Segment, v2_4, v3_4
            do! Edge 20, PuzzleEdge.Segment, v3_4, v4_4

            // Vertical edges
            do! Edge 21, PuzzleEdge.Segment, v0_0, v0_1
            do! Edge 22, PuzzleEdge.Segment, v0_1, v0_2
            do! Edge 23, PuzzleEdge.Segment, v0_2, v0_3
            do! Edge 24, PuzzleEdge.Segment, v0_3, v0_4
            do! Edge 25, PuzzleEdge.Segment, v1_0, v1_1
            do! Edge 26, PuzzleEdge.Segment, v1_1, v1_2
            do! Edge 27, PuzzleEdge.Segment, v1_2, v1_3
            do! Edge 28, PuzzleEdge.Segment, v1_3, v1_4
            do! Edge 29, PuzzleEdge.Segment, v2_0, v2_1
            do! Edge 30, PuzzleEdge.Segment, v2_1, v2_2
            do! Edge 31, PuzzleEdge.Segment, v2_2, v2_3
            do! Edge 32, PuzzleEdge.Segment, v2_3, v2_4
            do! Edge 33, PuzzleEdge.Segment, v3_0, v3_1
            do! Edge 34, PuzzleEdge.Segment, v3_1, v3_2
            do! Edge 35, PuzzleEdge.Segment, v3_2, v3_3
            do! Edge 36, PuzzleEdge.Segment, v3_3, v3_4
            do! Edge 37, PuzzleEdge.Segment, v4_0, v4_1
            do! Edge 38, PuzzleEdge.Segment, v4_1, v4_2
            do! Edge 39, PuzzleEdge.Segment, v4_2, v4_3
            do! Edge 40, PuzzleEdge.Segment, v4_3, v4_4
        }

        // Act
        let res = SmtSolver.solve (puzzleFromGraph graph)

        // Assert
        assertSatisfiable res
        
    [<TestMethod>]
    member _.``Valid bridge puzzle is satisfiable``() =
        // Arrange
        let graph = propertyGraph {
            // Lines
            let A = 1
            let B = 2
            let C = 3
            let D = 4
            let E = 5

            // Vertices
            let! v1_0 = Vertex 1, PuzzleVertex.Terminal (Line B)
            let! v2_0 = Vertex 2, PuzzleVertex.Terminal (Line A)
            let! v3_0 = Vertex 3, PuzzleVertex.Terminal (Line C)
            let! v0_1 = Vertex 4, PuzzleVertex.Terminal (Line B)
            let! v1_1 = Vertex 5, PuzzleVertex.Cell
            let! v2_1 = Vertex 6, PuzzleVertex.Cell
            let! v3_1 = Vertex 7, PuzzleVertex.Cell
            let! v4_1 = Vertex 8, PuzzleVertex.Terminal (Line C)
            let! v0_2 = Vertex 9, PuzzleVertex.Terminal (Line D)
            let! v1_2 = Vertex 10, PuzzleVertex.Cell
            let! v2_2 = Vertex 11, PuzzleVertex.Bridge
            let! v3_2 = Vertex 12, PuzzleVertex.Cell
            let! v4_2 = Vertex 13, PuzzleVertex.Terminal (Line D)
            let! v0_3 = Vertex 14, PuzzleVertex.Terminal (Line E)
            let! v1_3 = Vertex 15, PuzzleVertex.Cell
            let! v2_3 = Vertex 16, PuzzleVertex.Terminal (Line A)
            let! v3_3 = Vertex 17, PuzzleVertex.Cell
            let! v4_3 = Vertex 18, PuzzleVertex.Terminal (Line E)
            let! v1_4 = Vertex 19, PuzzleVertex.Cell
            let! v2_4 = Vertex 20, PuzzleVertex.Cell
            let! v3_4 = Vertex 21, PuzzleVertex.Cell

            // Horizontal edges
            do! Edge 1, PuzzleEdge.Segment, v1_0, v2_0
            do! Edge 2, PuzzleEdge.Segment, v2_0, v3_0
            do! Edge 3, PuzzleEdge.Segment, v0_1, v1_1
            do! Edge 4, PuzzleEdge.Segment, v1_1, v2_1
            do! Edge 5, PuzzleEdge.Segment, v2_1, v3_1
            do! Edge 6, PuzzleEdge.Segment, v3_1, v4_1
            do! Edge 7, PuzzleEdge.Segment, v0_2, v1_2
            do! Edge 8, PuzzleEdge.Segment, v3_2, v4_2
            do! Edge 9, PuzzleEdge.Segment, v0_3, v1_3
            do! Edge 10, PuzzleEdge.Segment, v1_3, v2_3
            do! Edge 11, PuzzleEdge.Segment, v2_3, v3_3
            do! Edge 12, PuzzleEdge.Segment, v3_3, v4_3
            do! Edge 13, PuzzleEdge.Segment, v1_4, v2_4
            do! Edge 14, PuzzleEdge.Segment, v2_4, v3_4

            // Vertical edges
            do! Edge 15, PuzzleEdge.Segment, v1_0, v1_1
            do! Edge 16, PuzzleEdge.Segment, v2_0, v2_1
            do! Edge 17, PuzzleEdge.Segment, v3_0, v3_1
            do! Edge 18, PuzzleEdge.Segment, v0_1, v0_2
            do! Edge 19, PuzzleEdge.Segment, v1_1, v1_2
            do! Edge 20, PuzzleEdge.Segment, v3_1, v3_2
            do! Edge 21, PuzzleEdge.Segment, v4_1, v4_2
            do! Edge 22, PuzzleEdge.Segment, v0_2, v0_3
            do! Edge 23, PuzzleEdge.Segment, v1_2, v1_3
            do! Edge 24, PuzzleEdge.Segment, v3_2, v3_3
            do! Edge 25, PuzzleEdge.Segment, v4_2, v4_3
            do! Edge 26, PuzzleEdge.Segment, v1_3, v1_4
            do! Edge 27, PuzzleEdge.Segment, v2_3, v2_4
            do! Edge 28, PuzzleEdge.Segment, v3_3, v3_4

            // Bridge edges
            do! Edge 29, PuzzleEdge.BridgeLane (Lane 1), v2_2, v1_2
            do! Edge 30, PuzzleEdge.BridgeLane (Lane 1), v2_2, v3_2
            do! Edge 31, PuzzleEdge.BridgeLane (Lane 2), v2_2, v2_1
            do! Edge 32, PuzzleEdge.BridgeLane (Lane 2), v2_2, v2_3
        }

        // Act
        let res = SmtSolver.solve (puzzleFromGraph graph)

        // Assert
        assertSatisfiable res
        
    [<TestMethod>]
    member _.``Valid warp puzzle is satisfiable``() =
        // Arrange
        let graph = propertyGraph {
            // Lines
            let A = 1
            let B = 2
            let C = 3
            let D = 4
            
            // Vertices
            let! v1_0 = Vertex 1, PuzzleVertex.Terminal (Line B)
            let! v2_0 = Vertex 2, PuzzleVertex.Cell
            let! v3_0 = Vertex 3, PuzzleVertex.Terminal (Line B)
            let! v0_1 = Vertex 4, PuzzleVertex.Terminal (Line A)
            let! v1_1 = Vertex 5, PuzzleVertex.Terminal (Line C)
            let! v2_1 = Vertex 6, PuzzleVertex.Cell
            let! v3_1 = Vertex 7, PuzzleVertex.Terminal (Line C)
            let! v4_1 = Vertex 8, PuzzleVertex.Terminal (Line A)
            let! v5_1 = Vertex 9, PuzzleVertex.Cell
            let! v1_2 = Vertex 10, PuzzleVertex.Terminal (Line D)
            let! v2_2 = Vertex 11, PuzzleVertex.Cell
            let! v3_2 = Vertex 12, PuzzleVertex.Terminal (Line D)
            // Horizontal edges
            do! Edge 1, PuzzleEdge.Segment, v1_0, v2_0
            do! Edge 2, PuzzleEdge.Segment, v2_0, v3_0
            do! Edge 3, PuzzleEdge.Segment, v0_1, v1_1
            do! Edge 4, PuzzleEdge.Segment, v1_1, v2_1
            do! Edge 5, PuzzleEdge.Segment, v2_1, v3_1
            do! Edge 6, PuzzleEdge.Segment, v3_1, v4_1
            do! Edge 7, PuzzleEdge.Segment, v4_1, v5_1
            do! Edge 8, PuzzleEdge.Segment, v1_2, v2_2
            do! Edge 9, PuzzleEdge.Segment, v2_2, v3_2

            // Vertical edges
            do! Edge 10, PuzzleEdge.Segment, v1_0, v1_1
            do! Edge 11, PuzzleEdge.Segment, v2_0, v2_1
            do! Edge 12, PuzzleEdge.Segment, v3_0, v3_1
            do! Edge 13, PuzzleEdge.Segment, v1_1, v1_2
            do! Edge 14, PuzzleEdge.Segment, v2_1, v2_2
            do! Edge 15, PuzzleEdge.Segment, v3_1, v3_2

            // Warp edges
            do! Edge 16, PuzzleEdge.Warp, v0_1, v5_1
        }

        // Act
        let res = SmtSolver.solve (puzzleFromGraph graph)

        // Assert
        assertSatisfiable res

    [<TestMethod>]
    member _.``Valid hexes puzzles is satisfiable``() =
        // Arrange
        let graph = propertyGraph {
            // Lines
            let A = 1
            let B = 2
            let C = 3
            let D = 4
            let E = 5

            // Vertices
            let! v0_0 = Vertex 1, PuzzleVertex.Cell
            let! v1_0 = Vertex 2, PuzzleVertex.Cell
            let! v1_1 = Vertex 3, PuzzleVertex.Cell
            let! v1_2 = Vertex 4, PuzzleVertex.Cell
            let! v1_3 = Vertex 5, PuzzleVertex.Cell
            let! v1_4 = Vertex 6, PuzzleVertex.Cell
            let! v1_5 = Vertex 7, PuzzleVertex.Cell
            let! v2_1 = Vertex 8, PuzzleVertex.Terminal (Line D)
            let! v2_2 = Vertex 9, PuzzleVertex.Terminal (Line E)
            let! v2_3 = Vertex 10, PuzzleVertex.Terminal (Line B)
            let! v2_4 = Vertex 11, PuzzleVertex.Cell
            let! v2_5 = Vertex 12, PuzzleVertex.Terminal (Line E)
            let! v2_6 = Vertex 13, PuzzleVertex.Terminal (Line D)
            let! v2_7 = Vertex 14, PuzzleVertex.Terminal (Line A)
            let! v2_8 = Vertex 15, PuzzleVertex.Cell
            let! v2_9 = Vertex 16, PuzzleVertex.Cell
            let! v2_10 = Vertex 17, PuzzleVertex.Terminal (Line C)
            let! v2_11 = Vertex 18, PuzzleVertex.Terminal (Line A)
            let! v3_7 = Vertex 19, PuzzleVertex.Cell
            let! v3_8 = Vertex 20, PuzzleVertex.Terminal (Line B)
            let! v3_10 = Vertex 21, PuzzleVertex.Terminal (Line C)
            let! v3_11 = Vertex 22, PuzzleVertex.Cell

            // Loop edges
            do! Edge 1, PuzzleEdge.Segment, v1_0, v1_1
            do! Edge 2, PuzzleEdge.Segment, v1_1, v1_2
            do! Edge 3, PuzzleEdge.Segment, v1_2, v1_3
            do! Edge 4, PuzzleEdge.Segment, v1_3, v1_4
            do! Edge 5, PuzzleEdge.Segment, v1_4, v1_5
            do! Edge 6, PuzzleEdge.Segment, v1_5, v1_0
            do! Edge 8, PuzzleEdge.Segment, v2_1, v2_2
            do! Edge 9, PuzzleEdge.Segment, v2_2, v2_3
            do! Edge 10, PuzzleEdge.Segment, v2_3, v2_4
            do! Edge 11, PuzzleEdge.Segment, v2_4, v2_5
            do! Edge 12, PuzzleEdge.Segment, v2_5, v2_6
            do! Edge 13, PuzzleEdge.Segment, v2_6, v2_7
            do! Edge 14, PuzzleEdge.Segment, v2_7, v2_8
            do! Edge 15, PuzzleEdge.Segment, v2_8, v2_9
            do! Edge 16, PuzzleEdge.Segment, v2_9, v2_10
            do! Edge 17, PuzzleEdge.Segment, v2_10, v2_11
            do! Edge 18, PuzzleEdge.Segment, v3_7, v3_8
            do! Edge 19, PuzzleEdge.Segment, v3_10, v3_11

            // Connecting edges
            do! Edge 20, PuzzleEdge.Segment, v0_0, v1_0
            do! Edge 21, PuzzleEdge.Segment, v0_0, v1_1
            do! Edge 22, PuzzleEdge.Segment, v0_0, v1_2
            do! Edge 23, PuzzleEdge.Segment, v0_0, v1_3
            do! Edge 24, PuzzleEdge.Segment, v0_0, v1_4
            do! Edge 25, PuzzleEdge.Segment, v0_0, v1_5
            do! Edge 26, PuzzleEdge.Segment, v1_0, v2_11
            do! Edge 27, PuzzleEdge.Segment, v1_0, v2_1
            do! Edge 28, PuzzleEdge.Segment, v1_1, v2_1
            do! Edge 29, PuzzleEdge.Segment, v1_1, v2_2
            do! Edge 30, PuzzleEdge.Segment, v1_1, v2_3
            do! Edge 31, PuzzleEdge.Segment, v1_2, v2_3
            do! Edge 32, PuzzleEdge.Segment, v1_2, v2_4
            do! Edge 33, PuzzleEdge.Segment, v1_2, v2_5
            do! Edge 34, PuzzleEdge.Segment, v1_3, v2_5
            do! Edge 35, PuzzleEdge.Segment, v1_3, v2_6
            do! Edge 36, PuzzleEdge.Segment, v1_3, v2_7
            do! Edge 37, PuzzleEdge.Segment, v1_4, v2_7
            do! Edge 38, PuzzleEdge.Segment, v1_4, v2_8
            do! Edge 39, PuzzleEdge.Segment, v1_4, v2_9
            do! Edge 40, PuzzleEdge.Segment, v1_5, v2_9
            do! Edge 41, PuzzleEdge.Segment, v1_5, v2_10
            do! Edge 42, PuzzleEdge.Segment, v1_5, v2_11
            do! Edge 43, PuzzleEdge.Segment, v2_4, v3_7
            do! Edge 44, PuzzleEdge.Segment, v2_5, v3_7
            do! Edge 45, PuzzleEdge.Segment, v2_5, v3_8
            do! Edge 46, PuzzleEdge.Segment, v2_6, v3_8
            do! Edge 47, PuzzleEdge.Segment, v2_6, v3_10
            do! Edge 48, PuzzleEdge.Segment, v2_7, v3_10
            do! Edge 49, PuzzleEdge.Segment, v2_7, v3_11
            do! Edge 50, PuzzleEdge.Segment, v2_8, v3_11
        }

        // Act
        let res = SmtSolver.solve (puzzleFromGraph graph)

        // Assert
        assertSatisfiable res
        
    [<TestMethod>]
    member _.``Valid shapes puzzles is satisfiable``() =
        // Arrange
        let graph = propertyGraph {
            // Top face
            let! vtt = Vertex 1, PuzzleVertex.Cell
            let! vtl = Vertex 2, PuzzleVertex.Terminal (Line 1)
            let! vtr = Vertex 3, PuzzleVertex.Cell
            let! vtb = Vertex 4, PuzzleVertex.Terminal (Line 3)
            do! Edge 1, PuzzleEdge.Segment, vtt, vtl
            do! Edge 2, PuzzleEdge.Segment, vtl, vtb
            do! Edge 3, PuzzleEdge.Segment, vtb, vtr
            do! Edge 4, PuzzleEdge.Segment, vtr, vtt

            // Left 
            let! vlt = Vertex 5, PuzzleVertex.Cell
            let! vll = Vertex 6, PuzzleVertex.Terminal (Line 2)
            let! vlr = Vertex 7, PuzzleVertex.Terminal (Line 2)
            let! vlb = Vertex 8, PuzzleVertex.Terminal (Line 3)
            do! Edge 5, PuzzleEdge.Segment, vlt, vll
            do! Edge 6, PuzzleEdge.Segment, vll, vlb
            do! Edge 7, PuzzleEdge.Segment, vlb, vlr
            do! Edge 8, PuzzleEdge.Segment, vlr, vlt

            // Right face
            let! vrt = Vertex 9, PuzzleVertex.Cell
            let! vrl = Vertex 10, PuzzleVertex.Cell
            let! vrr = Vertex 11, PuzzleVertex.Terminal (Line 1)
            let! vrb = Vertex 12, PuzzleVertex.Cell
            do! Edge 9, PuzzleEdge.Segment, vrt, vrl
            do! Edge 10, PuzzleEdge.Segment, vrl, vrb
            do! Edge 11, PuzzleEdge.Segment, vrb, vrr
            do! Edge 12, PuzzleEdge.Segment, vrr, vrt

            // Shape edges
            do! Edge 13, PuzzleEdge.Segment, vtl, vlt
            do! Edge 14, PuzzleEdge.Segment, vtb, vlr
            do! Edge 15, PuzzleEdge.Segment, vtb, vrl
            do! Edge 16, PuzzleEdge.Segment, vtr, vrt
            do! Edge 17, PuzzleEdge.Segment, vlr, vrl
            do! Edge 18, PuzzleEdge.Segment, vlb, vrb
        }

        // Act
        let res = SmtSolver.solve (puzzleFromGraph graph)

        // Assert
        assertSatisfiable res
