# CMSC330_Projects

## Setup
To run `Battleship`:
- Install [Ruby](https://www.ruby-lang.org/en/downloads/)
- `ruby src/main.rb test/public/inputs/player1.txt test/public/inputs/player2.txt test/public/inputs/perfect_strat_p1.txt test/public/inputs/perfect_strat_p2.txt`

To run `Finite Automata`:
- Install [OCaml](https://ocaml.org/docs/install.html)
- Set environment variable and run tests: `env OCAMLPATH=dep dune runtest -f`

To run `Lexer_Parser_Interpreter`:
- Install [OCaml](https://ocaml.org/docs/install.html)
- To run tests: `dune runtest -f`
- To run and test from top level: `dune utop src`

To run `Turtles in Campus`:
- Install [Rust](https://www.rust-lang.org/tools/install)
- To compile, run `cargo build`
- To test, run `cargo test`

## Description
### `Battleship`
- This project used the [rules](https://www.hasbro.com/common/instruct/Battleship.PDF) of the game of Battleship to run the game. The objective of this project was to be able to process text files containing strategies to attack the other ship. The point was to get a way to get familiar with Ruby's syntax, code blocks and built in data structures.

### `Finite Automata`
- This project works with NFA's, DFA's and regular expressions in OCaml. The objective was to learn how each one of the three worked and to be able to convert from a string into one of these states. A huge goal in this was to take a regular expression and convert that into an NFA and a DFA. Together with all these parts together, it is assembled into an Interpreter but in this project it isn't put together as it was meant to teach how each part worked individually.

### `Lexer_Parser_Interpreter`
- In this project, the objective is to create a dynamically-typed version of OCaml with limited features and as a way to use it an implementation of a version of utop (mutop) was needed. This program takes a string of this dynamically-typed version of OCaml and breaks it up into individual tokens to be read by the parser to produce an abstract symbol tree (AST) for an expression or a mutop directive. Then an interpreter will execute this AST with the use of operational semantics.

### `Turtles in Campus`
- In this project, the objective was to get comfortable with using Rust by creating a project in which Turtle references were meant to occupy a campus, where every Turtle is unique. A Turtle had different elements and different abilities. A Turtle was to be able to be created, added to a campus, able to return its own information back, get the fastest turtle and even breed two turtles. Every time a Turtle was searched by name, it was to be entered into a cache where it could be retrieved faster, rather than going through all the list of Turtles searching by name. This project helped to teach the use of wrappers and ways to reference and deference as well as passing references between functions.
