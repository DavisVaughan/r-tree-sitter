# Get a node's underlying text

`node_text()` returns the document text underlying a node.

## Usage

``` r
node_text(x)
```

## Arguments

- x:

  `[tree_sitter_node]`

  A node.

## Value

A single string containing the node's text.

## Examples

``` r
language <- treesitter.r::language()
parser <- parser(language)

text <- "1 + foo"
tree <- parser_parse(parser, text)
node <- tree_root_node(tree)

node |>
  node_child(1) |>
  node_child_by_field_name("rhs") |>
  node_text()
#> [1] "foo"
```
