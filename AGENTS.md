# Flux

Flux is a cross-platform-compatible Zig-Zag Numberlink game build in F#. The game involves connecting pairs of terminals with non-overlapping paths on a graph such that the entire graph is filled, all terminals are connected, and no paths intersect. It features several special connectors, such as bridges and warps. While the focus is on orthogonal levels, Flux also supports any arbitrary shape due to its graph-based design.

## Game Rules

1. All lines must connect pairs of terminals of the same color
2. Lines cannot intersect or overlap, except through special connectors such as bridges
3. A segment of a line cannot neighbor another segments of the same line
4. Levels must be solvable with a unique solution, meaning reflective symmetry is not allowed
5. A valid solution must fill the entire graph leaving no empty spaces
6. A warp cannot neighbor another bridge, terminal, or other vertex that neighbors another warp

## Key Concepts

1. Graph vertices represent cells in the level, and edges represent possible connections between cells a line may take
2. Bridges and terminals are types of vertices, whereas warps are a special type of edge
3. Vertices store their position in the level visually as metadata as the data structure does not imply any spatial representation
4. A template is a graph which defines the shape of a level through a partial implementation
5. Generating complete levels is done through Wave Function Collapse (WFC) to create many unique levels from a single template where possible

## Project Structure

```
/<solution-root>
	/Numberlink.ZigZag.Core - Core game logic, level generation, and features inherent to the game
	/Flux.Tui - Terminal user interface for playing the game in a console environment
```

## Project Architecture

For all projects, the following key architectural decisions have been made:

- Necessary extensions to existing types/modules are located in `/Extensions`
- Algorithms, data structures, and utilities necessary for implementation but not specific to Flux are located in `/Lib`
- Loosely follow onion architecture principles to separate concerns and keep dependencies simple and extensible

### Numberlink.ZigZag.Core

This project is an F# class library used by all Flux applications.

- Key functions and types for the core project are located at the project root

### Flux.Tui

This project is an F# console application built using Elmish. It implements the game to be played in a terminal directly with keyboard controls.

- The game is rendered using half-block glyphs to render two pixels per character (`/Lib/TerminalDisplay.fs`)
- Elmish follows the Model-View-Update (MVU) architecture pattern
- Due to the minimal space available in the terminal window, it currently only supports orthogonal levels which can be compactly represented

## Coding Standards

To keep the project maintainable, try to write as little code as necessary. Functions should be small and focused, belonging to a relevant module. Keep code extensible and unopinionated where reasonable. No code should be deeply nested, as a few levels of indentation can be refactored into smaller functions.

### F#

- Always prefer modern F# constructs and idioms
- Favor immutability and functional programming principles
- Avoid private functions, instead preferring logical functions to implement parts of the larger function in dependent modules
- Avoid imperative constructs like if statements, for/while loops, and mutable state, instead preferring pattern matching, recursion, and higher-order functions
- Use discriminated unions and records to model data
- Modularize code into small, reusable functions
- Describe each small block of code with a comment with a few words as a header to explain its key purpose
- Add xml comments to function signatures explaining what they do and how edge cases are handled
- Do not write verbose multi-line comments. Code should be self-explanatory or self-evident based on business requirements
- Prefer type inference where possible, and only add type annotations when necessary or not immediately obvious
- Use pipelining (`|>`) and function composition (`>>`, `<<`) to enhance readability
- Follow F# syntax and style conventions, such as `->` instead of `=>`
- Where necessary, use async computation expressions over tasks for concurrency
- Follow a consistent code style where new code stylistically matches existing code
