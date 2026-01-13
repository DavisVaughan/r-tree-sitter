# Node parse states

These are advanced functions that return information about the internal
parse states.

- `node_parse_state()` returns the parse state of the current node.

- `node_next_parse_state()` returns the parse state after this node.

See
[`language_next_state()`](https://davisvaughan.github.io/r-tree-sitter/reference/language_next_state.md)
for more information.

## Usage

``` r
node_parse_state(x)

node_next_parse_state(x)
```

## Arguments

- x:

  `[tree_sitter_node]`

  A node.

## Value

A single integer representing a parse state.

## Examples

``` r
language <- treesitter.r::language()
parser <- parser(language)

text <- "fn <- function() { 1 + 1 }"
tree <- parser_parse(parser, text)
node <- tree_root_node(tree)

node <- node_child(node, 1)

# Parse states are grammar dependent
node_parse_state(node)
#> [1] 447
node_next_parse_state(node)
#> [1] 165
```
