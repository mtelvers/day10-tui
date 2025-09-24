(lang dune 3.18)

(name day10-tui)

(generate_opam_files true)

(source
 (github mtelvers/day10-tui))

(authors "Mark Elvers <mark.elvers@tunbury.org")

(maintainers "Mark Elvers <mark.elvers@tunbury.org")

(license MIT)

(documentation https://github.com/mtelvers/day10-tui)

(package
 (name day10-tui)
 (synopsis "Terminal UI for browsing opam repository build results")
 (description "A fast terminal-based interface for analyzing Apache Arrow parquet files containing opam repository build results. Features include commit browsing, package/compiler matrix views, detailed build logs, and multi-commit diff comparisons.")
 (depends ocaml ctypes ctypes-foreign ptime cmdliner)
 (tags
  ("tui" "terminal" "opam" "build-results" "arrow" "parquet")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
