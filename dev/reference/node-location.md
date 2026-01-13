# Node byte and point accessors

These functions return information about the location of `x` in the
document. The byte, row, and column locations are all 0-indexed.

- `node_start_byte()` returns the start byte.

- `node_end_byte()` returns the end byte.

- `node_start_point()` returns the start point, containing a row and
  column location within the document. Use accessors like
  [`point_row()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/points.md)
  to extract the row and column positions.

- `node_end_point()` returns the end point, containing a row and column
  location within the document. Use accessors like
  [`point_row()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/points.md)
  to extract the row and column positions.

- `node_range()` returns a range object that contains all of the above
  information. Use accessors like
  [`range_start_point()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/ranges.md)
  to extract individual pieces from the range.

## Usage

``` r
node_start_byte(x)

node_end_byte(x)

node_start_point(x)

node_end_point(x)

node_range(x)
```

## Arguments

- x:

  `[tree_sitter_node]`

  A node.

## Value

- `node_start_byte()` and `node_end_byte()` return a single numeric
  value.

- `node_start_point()` and `node_end_point()` return single points.

- `node_range()` returns a range.

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

node_start_byte(node)
#> [1] 6
node_end_byte(node)
#> [1] 26

node_start_point(node)
#> <tree_sitter_point>
#> Row: 0
#> Column: 6
node_end_point(node)
#> <tree_sitter_point>
#> Row: 0
#> Column: 26

node_range(node)
#> <tree_sitter_range>
#> Start <byte: 6, row: 0, column: 6>
#> End <byte: 26, row: 0, column: 26>
```
