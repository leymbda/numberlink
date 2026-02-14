open Numberlink.ZigZag.Core
open System

let seed = (new Random()).Next()
let random = new Random(seed)

let template = // 3x3 donut shape
    Template.empty<Orthogonal>
    |> Template.addUnobserved (Guid("00000000-0000-0000-0000-ffffffffff00")) { X = 0; Y = 0 }
    |> Template.addUnobserved (Guid("00000000-0000-0000-0000-ffffffffff01")) { X = 0; Y = 1 }
    |> Template.addUnobserved (Guid("00000000-0000-0000-0000-ffffffffff02")) { X = 0; Y = 2 }
    |> Template.addUnobserved (Guid("00000000-0000-0000-0000-ffffffffff10")) { X = 1; Y = 0 }
    |> Template.addUnobserved (Guid("00000000-0000-0000-0000-ffffffffff12")) { X = 1; Y = 2 }
    |> Template.addUnobserved (Guid("00000000-0000-0000-0000-ffffffffff20")) { X = 2; Y = 0 }
    |> Template.addUnobserved (Guid("00000000-0000-0000-0000-ffffffffff21")) { X = 2; Y = 1 }
    |> Template.addUnobserved (Guid("00000000-0000-0000-0000-ffffffffff22")) { X = 2; Y = 2 }
    |> Template.addPath (Guid("00000000-0000-0000-0000-ffffffffff00")) (Guid("00000000-0000-0000-0000-ffffffffff01")) (Guid("1fffffff-0000-0000-0000-000000000000"))
    |> Template.addPath (Guid("00000000-0000-0000-0000-ffffffffff01")) (Guid("00000000-0000-0000-0000-ffffffffff02")) (Guid("2fffffff-0000-0000-0000-000000000000"))
    |> Template.addPath (Guid("00000000-0000-0000-0000-ffffffffff02")) (Guid("00000000-0000-0000-0000-ffffffffff12")) (Guid("3fffffff-0000-0000-0000-000000000000"))
    |> Template.addPath (Guid("00000000-0000-0000-0000-ffffffffff12")) (Guid("00000000-0000-0000-0000-ffffffffff22")) (Guid("4fffffff-0000-0000-0000-000000000000"))
    |> Template.addPath (Guid("00000000-0000-0000-0000-ffffffffff22")) (Guid("00000000-0000-0000-0000-ffffffffff21")) (Guid("5fffffff-0000-0000-0000-000000000000"))
    |> Template.addPath (Guid("00000000-0000-0000-0000-ffffffffff21")) (Guid("00000000-0000-0000-0000-ffffffffff20")) (Guid("6fffffff-0000-0000-0000-000000000000"))
    |> Template.addPath (Guid("00000000-0000-0000-0000-ffffffffff20")) (Guid("00000000-0000-0000-0000-ffffffffff10")) (Guid("7fffffff-0000-0000-0000-000000000000"))
    |> Template.addPath (Guid("00000000-0000-0000-0000-ffffffffff10")) (Guid("00000000-0000-0000-0000-ffffffffff00")) (Guid("8fffffff-0000-0000-0000-000000000000"))

for _ in 0..9 do Console.WriteLine()

let level =
    Level.generate random (Guid("00000000-0000-0000-0000-000000000000")) template
    |> Result.defaultWith (fun err ->
        printfn "Error generating level: %s" err
        exit 1
    )

level.Template.Positions |> Map.iter (fun vertexId position ->
    printfn "Vertex %A position (%d, %d)" vertexId position.X position.Y
)
