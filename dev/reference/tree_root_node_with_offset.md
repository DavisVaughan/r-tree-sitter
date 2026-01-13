# Retrieve an offset root node

`tree_root_node_with_offset()` is similar to
[`tree_root_node()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/tree_root_node.md),
but the returned root node's position has been shifted by the given
number of bytes, rows, and columns.

This function allows you to parse a subset of a document with
[`parser_parse()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/parser-parse.md)
as if it were a self-contained document, but then later access the
syntax tree in the coordinate space of the larger document.

Note that the underlying `text` within `x` is not what you are
offsetting into. Instead, you should assume that the `text` you provided
to
[`parser_parse()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/parser-parse.md)
already contained the entire subset of the document you care about, and
the offset you are providing is how far into the document the beginning
of `text` is.

## Usage

``` r
tree_root_node_with_offset(x, byte, point)
```

## Arguments

- x:

  `[tree_sitter_tree]`

  A tree.

- byte, point:

  `[double(1), tree_sitter_point]`

  A byte and point offset combination.

## Value

An offset root node.

## Examples

``` r
language <- treesitter.r::language()
parser <- parser(language)

text <- "fn <- function() { 1 + 1 }"
tree <- parser_parse(parser, text)

# If `text` was the whole document, you can just use `tree_root_node()`
node <- tree_root_node(tree)

# If `text` represents a subset of the document, use
# `tree_root_node_with_offset()` to be able to get positions in the
# coordinate space of the original document.
byte <- 5
point <- point(5, 0)
node_offset <- tree_root_node_with_offset(tree, byte, point)

# The position of `fn` if you treat `text` as the whole document
node |>
  node_child(1) |>
  node_child(1)
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> fn
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (identifier [(0, 0), (0, 2)])

# The position of `fn` if you treat `text` as a subset of a larger document
node_offset |>
  node_child(1) |>
  node_child(1)
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> fn
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (identifier [(5, 0), (5, 2)])
```
