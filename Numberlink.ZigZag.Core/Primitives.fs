namespace Numberlink.ZigZag.Core

/// An identifier for a vertex.
type [<Struct>] Vertex = Vertex of int

/// An identifier for an edge.
type [<Struct>] Edge = Edge of int

/// An identifier for a line, used to group the different parts of the graph that are a part of the same line.
type [<Struct>] Line = Line of int

/// An identifier for a bridge lane, used to pair up edges through a bridge vertex.
type [<Struct>] Lane = Lane of int
