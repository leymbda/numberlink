namespace Numberlink.ZigZag.Core

/// An identifier for a vertex.
type [<Struct>] Vertex = Vertex of int

/// An identifier for an edge.
type [<Struct>] Edge = Edge of int

/// An identifier for a terminal, used to indicate terminal pairs.
type [<Struct>] Terminal = Terminal of int

/// An identifier for a bridge lane, used to pair up edges paths can take through a bridge.
type [<Struct>] BridgeLane = BridgeLane of int
