# Node descendants

These functions return the smallest node within this node that spans the
given range of bytes or points. If the ranges are out of bounds, or no
smaller node can be determined, the input is returned.

## Usage

``` r
node_descendant_for_byte_range(x, start, end)

node_named_descendant_for_byte_range(x, start, end)

node_descendant_for_point_range(x, start, end)

node_named_descendant_for_point_range(x, start, end)
```

## Arguments

- x:

  `[tree_sitter_node]`

  A node.

- start, end:

  `[integer(1) / tree_sitter_point]`

  For the byte range functions, start and end bytes to search within.

  For the point range functions, start and end points created by
  [`point()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/points.md)
  to search within.

## Value

A node.

## Examples

``` r
language <- treesitter.r::language()
parser <- parser(language)

text <- "fn <- function() { 1 + 1 }"
tree <- parser_parse(parser, text)
node <- tree_root_node(tree)

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

# The byte range points to a location in the word `function`
node_descendant_for_byte_range(node, 7, 9)
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> function
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> "function" [(0, 6), (0, 14)]
node_named_descendant_for_byte_range(node, 7, 9)
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> function() { 1 + 1 }
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (function_definition [(0, 6), (0, 26)]
#>   name: "function" [(0, 6), (0, 14)]
#>   parameters: (parameters [(0, 14), (0, 16)]
#>     open: "(" [(0, 14), (0, 15)]
#>     close: ")" [(0, 15), (0, 16)]
#>   )
#>   body: (braced_expression [(0, 17), (0, 26)]
#>     open: "{" [(0, 17), (0, 18)]
#>     body: (binary_operator [(0, 19), (0, 24)]
#>       lhs: (float [(0, 19), (0, 20)])
#>       operator: "+" [(0, 21), (0, 22)]
#>       rhs: (float [(0, 23), (0, 24)])
#>     )
#>     close: "}" [(0, 25), (0, 26)]
#>   )
#> )

start <- point(0, 14)
end <- point(0, 15)

node_descendant_for_point_range(node, start, end)
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> (
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> "(" [(0, 14), (0, 15)]
node_named_descendant_for_point_range(node, start, end)
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> ()
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (parameters [(0, 14), (0, 16)]
#>   open: "(" [(0, 14), (0, 15)]
#>   close: ")" [(0, 15), (0, 16)]
#> )

# OOB returns the input
node_descendant_for_byte_range(node, 25, 29)
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
```
