# "Raw" S-expression

`node_raw_s_expression()` returns the "raw" s-expression as seen by
tree-sitter. Most of the time,
[`node_show_s_expression()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node_show_s_expression.md)
provides a better view of the tree, but occasionally it can be useful to
see exactly what the underlying C library is using.

## Usage

``` r
node_raw_s_expression(x)
```

## Arguments

- x:

  `[tree_sitter_node]`

  A node.

## Value

A single string containing the raw s-expression.

## Examples

``` r
language <- treesitter.r::language()
parser <- parser(language)

text <- "1 + foo"
tree <- parser_parse(parser, text)
node <- tree_root_node(tree)

node_raw_s_expression(node)
#> [1] "(program (binary_operator lhs: (float) rhs: (identifier)))"
```
