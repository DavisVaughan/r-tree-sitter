# Node type

`node_type()` returns the "type" of the current node as a string.

This is a very useful function for making decisions about how to handle
the current node.

## Usage

``` r
node_type(x)
```

## Arguments

- x:

  `[tree_sitter_node]`

  A node.

## Value

A single string.

## Examples

``` r
language <- treesitter.r::language()
parser <- parser(language)

text <- "fn <- function() { 1 + 1 }"
tree <- parser_parse(parser, text)
node <- tree_root_node(tree)

# Top level program node
node_type(node)
#> [1] "program"

# The whole `<-` binary operator node
node <- node_child(node, 1)
node
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> fn <- function() { 1 + 1 }
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (binary_operator [(0, 0), (0, 26)]
#>   lhs: (identifier [(0, 0), (0, 2)])
#>   operator: "<-" [(0, 3), (0, 5)]
#>   rhs: (function_definition [(0, 6), (0, 26)]
#>     name: "function" [(0, 6), (0, 14)]
#>     parameters: (parameters [(0, 14), (0, 16)]
#>       open: "(" [(0, 14), (0, 15)]
#>       close: ")" [(0, 15), (0, 16)]
#>     )
#>     body: (braced_expression [(0, 17), (0, 26)]
#>       open: "{" [(0, 17), (0, 18)]
#>       body: (binary_operator [(0, 19), (0, 24)]
#>         lhs: (float [(0, 19), (0, 20)])
#>         operator: "+" [(0, 21), (0, 22)]
#>         rhs: (float [(0, 23), (0, 24)])
#>       )
#>       close: "}" [(0, 25), (0, 26)]
#>     )
#>   )
#> )
node_type(node)
#> [1] "binary_operator"

# Just the literal `<-` operator itself
node <- node_child_by_field_name(node, "operator")
node
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> <-
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> "<-" [(0, 3), (0, 5)]
node_type(node)
#> [1] "<-"
```
