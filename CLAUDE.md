# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Sinho is a Clojure library that provides a matcher called `=*` for writing tests with a high signal-to-noise ratio. It's designed to make test output more readable by showing only relevant differences between expected and actual values.

## Common Commands

### Run all tests
```bash
./bin/kaocha
# or
clojure -A:test -m kaocha.runner
```

### Run a specific test namespace
```bash
./bin/kaocha test/me/lomin/sinho/matcher_test.clj
```

### Start a REPL with test dependencies
```bash
clojure -A:test:nrepl -m nrepl.cmdline --port 7888
```

### Release/deploy
```bash
clojure -A:release -m applied-science.deps-library
```

## Architecture

The library consists of two main components:

### 1. **Matcher (`me.lomin.sinho.matcher`)**
   - Implements the `=*` test assertion that integrates with `clojure.test`
   - Uses A* search algorithm (via chatda library) to find minimal differences between data structures
   - Handles various data types: atoms, sequences, maps, and sets
   - Key functions:
     - `=*`: Main matcher function that returns a diff or the expected value
     - `children`: Multimethod that generates child nodes for the search
     - `heuristic`: Estimates the cost of transforming one structure to another

### 2. **Diff Engine (`me.lomin.sinho.diff`)**
   - Transforms the path differences found by the matcher into visual diff output
   - Uses Specter for efficient navigation and transformation of nested data structures
   - Integrates with lambdaisland/deep-diff2 for diff visualization
   - Key functions:
     - `diff`: Main function that converts paths to a visual diff
     - `grow-path-tree`: Builds a tree representation of transformations
     - `path-tree->diff-transformer`: Converts the tree into Specter navigators

## Key Dependencies

- **me.lomin/chatda**: Provides A* search implementation
- **com.rpl/specter**: Used for efficient data structure navigation
- **lambdaisland/kaocha**: Test runner
- **lambdaisland/deep-diff**: Provides diff visualization types (Mismatch, Deletion, Insertion)

## Testing Approach

The project uses Kaocha as the test runner. Test files are located in the `test/` directory and follow the naming convention `*_test.clj`. The library itself is tested using its own `=*` matcher, demonstrating its capabilities.