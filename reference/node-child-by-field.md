# Get a node's child by field id or name

These functions return children of `x` by field id or name.

- `node_child_by_field_id()` retrieves a child by field id.

- `node_child_by_field_name()` retrieves a child by field name.

Use
[`language_field_id_for_name()`](https://davisvaughan.github.io/r-tree-sitter/reference/language_field_id_for_name.md)
to get the field id for a field name.

## Usage

``` r
node_child_by_field_id(x, id)

node_child_by_field_name(x, name)
```

## Arguments

- x:

  `[tree_sitter_node]`

  A node.

- id:

  `[integer(1)]`

  The field id of the child to return.

- name:

  `[character(1)]`

  The field name of the child to return.

## Value

A child of `x`, or `NULL` if no matching child can be found.

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

# Get the field name of the first child
name <- node_field_name_for_child(node, 1)
name
#> [1] "lhs"

# Now get the child again by that field name
node_child_by_field_name(node, name)
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> fn
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (identifier [(0, 0), (0, 2)])

# If you need to look up by field name many times, you can look up the
# more direct field id first and use that instead
id <- language_field_id_for_name(language, name)
id
#> [1] 11

node_child_by_field_id(node, id)
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> fn
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (identifier [(0, 0), (0, 2)])

# Returns `NULL` if no matching child
node_child_by_field_id(node, 10000)
#> NULL
```
