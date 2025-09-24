# day10-tui

Terminal UI for browsing opam repository build results stored in Apache Arrow parquet files.

## Features

- Browse git commits with availability indicators for build result data
- Interactive package/compiler matrix view showing build statuses
- Detailed build logs and solver output for failed builds
- Multi-commit comparison showing package additions, removals, and status changes
- Fast data loading with smart caching and column selection
- Server availability checking with automatic remote detection

## Installation

```bash
opam switch create . --deps-only
dune build
```

## Usage

```bash
# Use default opam-repository location in $HOME/opam-repository
dune exec -- day10-tui

# Specify custom repository path
dune exec -- day10-tui /path/to/opam-repository

# Show 100 commits instead of default 50
dune exec -- day10-tui --commits 100

# Fetch latest changes before starting
dune exec -- day10-tui --fetch

# Combine options
dune exec -- day10-tui /path/to/repo --commits 25 --fetch
```

## Navigation

### Commit List
- **Arrow keys**: Navigate commits
- **Enter**: View build results for selected commit
- **Space**: Toggle commit selection for comparison
- **Q/Escape**: Quit

### Package/Compiler Matrix
- **Arrow keys**: Navigate packages and compilers
- **Enter**: View detailed logs for selected package/compiler
- **Q/Escape**: Return to commit list

### Detail View
- **Arrow keys**: Scroll through build logs
- **Q/Escape**: Return to matrix view

### Diff View
- **Arrow keys**: Scroll through comparison results
- **Q/Escape**: Return to commit list

## Multi-commit Comparison

1. Select multiple commits using Space bar (marked with [*])
2. Press Enter to generate comparison
3. View new packages, removed packages, and status changes

