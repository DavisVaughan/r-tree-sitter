# Get a child's field name by index

These functions return the field name for the `i`th child of `x`.

- `node_field_name_for_child()` considers both named and anonymous
  children.

- `node_field_name_for_named_child()` considers only named children.

Nodes themselves don't know their own field names, because they don't
know if they are fields or not. You must have access to their parents to
query their field names.

## Usage

``` r
node_field_name_for_child(x, i)

node_field_name_for_named_child(x, i)
```

## Arguments

- x:

  `[tree_sitter_node]`

  A node.

- i:

  `[integer(1)]`

  The index of the child to get the field name for.

## Value

The field name for the `i`th child of `x`, or `NA_character_` if that
child doesn't exist.

## Examples

``` r
language <- treesitter.r::language()
parser <- parser(language)

text <- "fn <- function() { 1 + 1 }"
tree <- parser_parse(parser, text)
node <- tree_root_node(tree)

# Navigate to first child
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

# Get the field name of the first few children (note that anonymous children
# are considered)
node_field_name_for_child(node, 1)
#> [1] "lhs"
node_field_name_for_child(node, 2)
#> [1] "operator"

# Get the field name of the first few named children (note that anonymous
# children are not considered)
node_field_name_for_named_child(node, 1)
#> [1] "lhs"
node_field_name_for_named_child(node, 2)
#> [1] "rhs"

# 10th child doesn't exist, this returns `NA_character_`
node_field_name_for_child(node, 10)
#> [1] NA
```
