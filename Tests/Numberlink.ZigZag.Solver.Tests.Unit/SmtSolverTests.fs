namespace Numberlink.ZigZag.Solver

open FSharp.Collections.Graphs
open FsToolkit.ErrorHandling
open Microsoft.VisualStudio.TestTools.UnitTesting
open Numberlink.ZigZag.Core

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

    [<TestMethod>]
    member _.``bridgeLaneConstraints - Unmatched lane pairs are unsatisfiable``() =
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
    member _.``bridgeDistinctLaneConstraints - A line passing through a bridge twice should be unsatisfiable``() =
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
    member _.``bridgeLaneUsedConstraints - Unused bridge lanes should be unsatisfiable``() =
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
    member _.``vertexConstraints - Terminals with too few expected neighbors (0) should be unsatisfiable``() =
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
    member _.``vertexConstraints - Terminals with too many expected neighbors (2) should be unsatisfiable``() =
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
    member _.``vertexConstraints - Cells with too few expected neighbors (1) should be unsatisfiable``() =
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
    member _.``vertexConstraints - Cells with too many expected neighbors (3) should be unsatisfiable``() =
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
    member _.``vertexConstraints - Terminals/cells that connect to two vertices of a different lines should be unsatisfiable``() =
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
    member _.``selfTouchConstraints - A line that has to slither past itself should be unsatisfiable``() =
        // Arrange
        let graph = propertyGraph {
            let! v1 = Vertex 1, PuzzleVertex.Terminal (Line 1)
            let! v2 = Vertex 2, PuzzleVertex.Cell
            let! v3 = Vertex 3, PuzzleVertex.Cell
            let! v4 = Vertex 4, PuzzleVertex.Cell
            let! v5 = Vertex 5, PuzzleVertex.Cell
            let! v6 = Vertex 6, PuzzleVertex.Terminal (Line 2)
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

    // TODO: rankBoundConstraints
    // TODO: rankSourceConstraints
    // TODO: rankIncrementConstraints
    // TODO: Several valid levels to ensure they CAN be solved
