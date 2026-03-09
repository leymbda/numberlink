namespace Numberlink.ZigZag.Core

/// The multi-commodity flow of a solution, mapping each line to the list of edges that it passes through.
type Flow = Map<Line, Edge list>

module Flow =
    /// An empty flow.
    let empty: Flow = Map.empty

    // TODO: Add utility functions for necessary functionality (e.g. constructing from SmtSolver)
