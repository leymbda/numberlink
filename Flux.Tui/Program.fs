open Numberlink.ZigZag.Core
open System

let seed = (new Random()).Next()
let random = new Random(seed)

let vertex1 = Guid.generate random
let vertex2 = Guid.generate random
let vertex3 = Guid.generate random
let vertex4 = Guid.generate random
let vertex6 = Guid.generate random
let vertex7 = Guid.generate random
let vertex8 = Guid.generate random
let vertex9 = Guid.generate random

let template = // 3x3 donut shape
    Template.empty<Orthogonal>
    |> Template.addUnobserved vertex1 { X = 0; Y = 0 }
    |> Template.addUnobserved vertex2 { X = 0; Y = 1 }
    |> Template.addUnobserved vertex3 { X = 0; Y = 2 }
    |> Template.addUnobserved vertex4 { X = 1; Y = 0 }
    |> Template.addUnobserved vertex6 { X = 1; Y = 2 }
    |> Template.addUnobserved vertex7 { X = 2; Y = 0 }
    |> Template.addUnobserved vertex8 { X = 2; Y = 1 }
    |> Template.addUnobserved vertex9 { X = 2; Y = 2 }
    |> Template.addPath vertex1 vertex2 (Guid.generate random)
    |> Template.addPath vertex2 vertex3 (Guid.generate random)
    |> Template.addPath vertex3 vertex6 (Guid.generate random)
    |> Template.addPath vertex6 vertex9 (Guid.generate random)
    |> Template.addPath vertex9 vertex8 (Guid.generate random)
    |> Template.addPath vertex8 vertex7 (Guid.generate random)
    |> Template.addPath vertex7 vertex4 (Guid.generate random)
    |> Template.addPath vertex4 vertex1 (Guid.generate random)

for _ in 0..9 do Console.WriteLine()

let level =
    Level.generate random (Guid.generate random) template
    |> Result.defaultWith (fun err ->
        printfn "Error generating level: %s" err
        exit 1
    )

level.Template.Positions |> Map.iter (fun vertexId position ->
    printfn "Vertex %A position (%d, %d)" vertexId position.X position.Y
)
