# Node sibling accessors

These functions return siblings of the current node, i.e. if you looked
"left" or "right" from the current node rather "up" (parent) or "down"
(child).

- `node_next_sibling()` and `node_next_named_sibling()` return the next
  sibling.

- `node_previous_sibling()` and `node_previous_named_sibling()` return
  the previous sibling.

## Usage

``` r
node_next_sibling(x)

node_next_named_sibling(x)

node_previous_sibling(x)

node_previous_named_sibling(x)
```

## Arguments

- x:

  `[tree_sitter_node]`

  A node.

## Value

A sibling node, or `NULL` if there is no sibling node.

## Examples

``` r
language <- treesitter.r::language()
parser <- parser(language)

text <- "fn <- function() { 1 + 1 }"
tree <- parser_parse(parser, text)
node <- tree_root_node(tree)

# Navigate to first child
node <- node_child(node, 1)

# Navigate to function definition node
node <- node_child(node, 3)
node
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

node_previous_sibling(node)
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> <-
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> "<-" [(0, 3), (0, 5)]

# Skip anonymous operator node
node_previous_named_sibling(node)
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> fn
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (identifier [(0, 0), (0, 2)])

# There isn't one!
node_next_sibling(node)
#> NULL
```
