# Node symbol

`node_symbol()` returns the symbol id of the current node as an integer.

## Usage

``` r
node_symbol(x)
```

## Arguments

- x:

  `[tree_sitter_node]`

  A node.

## Value

A single integer.

## Examples

``` r
language <- treesitter.r::language()
parser <- parser(language)

text <- "fn <- function() { 1 + 1 }"
tree <- parser_parse(parser, text)
node <- tree_root_node(tree)

# Top level program node
node_symbol(node)
#> [1] 81

# The whole `<-` binary operator node
node <- node_child(node, 1)
node_symbol(node)
#> [1] 105

# Just the literal `<-` operator itself
node <- node_child_by_field_name(node, "operator")
node_symbol(node)
#> [1] 15
```
