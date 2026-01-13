# Get a node's underlying language

`node_language()` returns the document text underlying a node.

## Usage

``` r
node_language(x)
```

## Arguments

- x:

  `[tree_sitter_node]`

  A node.

## Value

A `tree_sitter_language` object.

## Examples

``` r
language <- treesitter.r::language()
parser <- parser(language)

text <- "1 + foo"
tree <- parser_parse(parser, text)
node <- tree_root_node(tree)

node_language(node)
#> <tree_sitter_language>
#> Language: r
```
