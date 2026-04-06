# Numberlink

[Numberlink](https://en.wikipedia.org/wiki/Numberlink) is a cross-platform game where you connect pairs of points with lines using vertex-disjoint paths. This project is based on the Zig-Zag variation of Numberlink, where all cells of a level must be filled with lines, and paths must not cross or touch themselves.

A proper name for this project is pending further progress.

## Project Structure

```
/<solution-root>
  /FSharp.Collections.Graphs - Custom library for abstract graph data structures
  /Numberlink.Core - Domain types and core logic for the game
  /Numberlink.Generator - Level generation algorithms
  /Numberlink.Solver - Level solving algorithms
```
