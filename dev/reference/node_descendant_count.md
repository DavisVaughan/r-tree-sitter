# Node descendant count

Returns the number of descendants of this node, including this node in
the count.

## Usage

``` r
node_descendant_count(x)
```

## Arguments

- x:

  `[tree_sitter_node]`

  A node.

## Value

A single double.

## Examples

``` r
language <- treesitter.r::language()
parser <- parser(language)

text <- "fn <- function() { 1 + 1 }"
tree <- parser_parse(parser, text)
node <- tree_root_node(tree)

# Top level program node
node_descendant_count(node)
#> [1] 16

# The whole `<-` binary operator node
node <- node_child(node, 1)
node_descendant_count(node)
#> [1] 15

# Just the literal `<-` operator itself
node <- node_child_by_field_name(node, "operator")
node_descendant_count(node)
#> [1] 1
```
