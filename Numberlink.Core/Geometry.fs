namespace Numberlink.Core

/// A 2D Cartesian coordinate for square orthogonal grid layouts, where each interior vertex has four neighbours.
[<Struct>]
type Cartesian = {
    X: int
    Y: int
}

/// A discrete polar coordinate for radial layouts, where R (Radius) is the ring index from the centre and T (Theta) is
/// the angular position within that ring.
[<Struct>]
type Polar = {
    R: int
    T: int
}

/// An axial coordinate for hexagonal grid layouts, using two of the three cube axes (Q, R) with S = -Q-R implicit.
[<Struct>]
type Axial = {
    Q: int
    R: int
}

/// A 2D coordinate for triangular grid layouts, where triangle orientation is implied by the parity of X + Y, giving
/// each vertex three neighbours.
[<Struct>]
type Triangular = {
    X: int
    Y: int
}

/// A 2D floating-point screen position used as the basis for cell shape rendering.
[<Struct>]
type Point = {
    X: float
    Y: float
}

/// An identifier for a border in a polygonal cell, used to identify shared borders across adjacent cells.
type [<Struct>] Border = Border of int

/// An edge in a polygonal cell shape, either a straight line segment or a circular arc.
[<RequireQualifiedAccess>]
type PolygonEdge =
    | Line of Point * Point
    | Arc of center: Point * radius: float * startAngle: float * endAngle: float

/// An arbitrary polygonal cell shape defined by its centre and a keyed map of edges forming its border.
type Polygonal = {
    Center: Point
    Edges: Map<Border, PolygonEdge>
}
