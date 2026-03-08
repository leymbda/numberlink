namespace Numberlink.ZigZag.Core

open FSharp.Collections.Graphs
open FsToolkit.ErrorHandling

/// A level position representing coordinates on a grid.
[<Struct>]
type Orthogonal = {
    X: int
    Y: int
}

type OrthogonalLevel = Level<Orthogonal>

module OrthogonalLevel =
    let [<Literal>] private unicodeNumberOffset = 49

    /// Convert the level into a string representation, using numbers to represent the line a vertex is a part of, and
    /// 'X' to represent a bridge. Will overflow to worse unicode characters if the level contains more than 9 lines.
    let toAsciiGrid (level: OrthogonalLevel) =
        let toChar x y =
            option {
                let! vertex = Map.tryFindKey (fun _ position -> position = { X = x; Y = y }) level.Positions
                let! kind = PropertyGraph.getVertexData vertex level.Graph

                match kind with
                | LevelVertex.Cell (Line line)
                | LevelVertex.Terminal (Line line) -> return char (line + unicodeNumberOffset)
                | LevelVertex.Bridge -> return 'X'
            }
            |> Option.defaultValue ' '

        let xs =
            level.Positions
            |> Map.toSeq
            |> Seq.map (fun (_, { X = x }) -> x)

        let minX, maxX = Seq.min xs, Seq.max xs

        let ys =
            level.Positions
            |> Map.toSeq
            |> Seq.map (fun (_, { Y = y }) -> y)

        let minY, maxY = Seq.min ys, Seq.max ys

        seq { minY .. maxY }
        |> Seq.map (fun y ->
            seq { for x in minX .. maxX do toChar x y }
            |> Seq.map string
            |> String.concat ""
        )
        |> String.concat "\n"
