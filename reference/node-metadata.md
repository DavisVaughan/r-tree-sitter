# Node metadata

These functions return metadata about the current node.

- `node_is_named()` reports if the current node is named or anonymous.

- `node_is_missing()` reports if the current node is `MISSING`, i.e. if
  it was implied through error recovery.

- `node_is_extra()` reports if the current node is an "extra" from the
  grammar.

- `node_is_error()` reports if the current node is an `ERROR` node.

- `node_has_error()` reports if the current node is an `ERROR` node, or
  if any descendants of the current node are `ERROR` or `MISSING` nodes.

## Usage

``` r
node_is_named(x)

node_is_missing(x)

node_is_extra(x)

node_is_error(x)

node_has_error(x)
```

## Arguments

- x:

  `[tree_sitter_node]`

  A node.

## Value

`TRUE` or `FALSE`.

## Examples

``` r
language <- treesitter.r::language()
parser <- parser(language)

text <- "fn <- function() { 1 + 1 }"
tree <- parser_parse(parser, text)
node <- tree_root_node(tree)

node <- node_child(node, 1)

fn <- node_child(node, 1)
operator <- node_child(node, 2)

fn
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> fn
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (identifier [(0, 0), (0, 2)])
node_is_named(fn)
#> [1] TRUE

operator
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> <-
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> "<-" [(0, 3), (0, 5)]
node_is_named(operator)
#> [1] FALSE

# Examples of `TRUE` cases for these are a bit hard to come up with, because
# they are dependent on the exact state of the grammar and the error recovery
# algorithm
node_is_missing(node)
#> [1] FALSE
node_is_extra(node)
#> [1] FALSE
```
