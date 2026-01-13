# Pretty print a `node`'s s-expression

`node_show_s_expression()` prints a nicely formatted s-expression to the
console. It powers the print methods of nodes and trees.

## Usage

``` r
node_show_s_expression(
  x,
  ...,
  max_lines = NULL,
  show_anonymous = TRUE,
  show_locations = TRUE,
  show_parentheses = TRUE,
  dangling_parenthesis = TRUE,
  color_parentheses = TRUE,
  color_locations = TRUE
)
```

## Arguments

- x:

  `[tree_sitter_node]`

  A node.

- ...:

  These dots are for future extensions and must be empty.

- max_lines:

  `[double(1) / NULL]`

  An optional maximum number of lines to print. If the maximum is hit,
  then `<truncated>` will be printed at the end.

- show_anonymous:

  `[bool]`

  Should anonymous nodes be shown? If `FALSE`, only named nodes are
  shown.

- show_locations:

  `[bool]`

  Should node locations be shown?

- show_parentheses:

  `[bool]`

  Should parentheses around each node be shown?

- dangling_parenthesis:

  `[bool]`

  Should the `)` parenthesis "dangle" on its own line? If `FALSE`, it is
  appended to the line containing the last child. This can be useful for
  conserving space.

- color_parentheses:

  `[bool]`

  Should parentheses be colored? Printing large s-expressions is faster
  if this is set to `FALSE`.

- color_locations:

  `[bool]`

  Should locations be colored? Printing large s-expressions is faster if
  this is set to `FALSE`.

## Value

`x` invisibly.

## Examples

``` r
language <- treesitter.r::language()
parser <- parser(language)

text <- "fn <- function(a, b = 2) { a + b + 2 }"
tree <- parser_parse(parser, text)
node <- tree_root_node(tree)

node_show_s_expression(node)
#> (program [(0, 0), (0, 38)]
#>   (binary_operator [(0, 0), (0, 38)]
#>     lhs: (identifier [(0, 0), (0, 2)])
#>     operator: "<-" [(0, 3), (0, 5)]
#>     rhs: (function_definition [(0, 6), (0, 38)]
#>       name: "function" [(0, 6), (0, 14)]
#>       parameters: (parameters [(0, 14), (0, 24)]
#>         open: "(" [(0, 14), (0, 15)]
#>         parameter: (parameter [(0, 15), (0, 16)]
#>           name: (identifier [(0, 15), (0, 16)])
#>         )
#>         (comma [(0, 16), (0, 17)])
#>         parameter: (parameter [(0, 18), (0, 23)]
#>           name: (identifier [(0, 18), (0, 19)])
#>           "=" [(0, 20), (0, 21)]
#>           default: (float [(0, 22), (0, 23)])
#>         )
#>         close: ")" [(0, 23), (0, 24)]
#>       )
#>       body: (braced_expression [(0, 25), (0, 38)]
#>         open: "{" [(0, 25), (0, 26)]
#>         body: (binary_operator [(0, 27), (0, 36)]
#>           lhs: (binary_operator [(0, 27), (0, 32)]
#>             lhs: (identifier [(0, 27), (0, 28)])
#>             operator: "+" [(0, 29), (0, 30)]
#>             rhs: (identifier [(0, 31), (0, 32)])
#>           )
#>           operator: "+" [(0, 33), (0, 34)]
#>           rhs: (float [(0, 35), (0, 36)])
#>         )
#>         close: "}" [(0, 37), (0, 38)]
#>       )
#>     )
#>   )
#> )

node_show_s_expression(node, max_lines = 5)
#> (program [(0, 0), (0, 38)]
#>   (binary_operator [(0, 0), (0, 38)]
#>     lhs: (identifier [(0, 0), (0, 2)])
#>     operator: "<-" [(0, 3), (0, 5)]
#>     rhs: (function_definition [(0, 6), (0, 38)]
#> <truncated>

# This is more like a typical abstract syntax tree
node_show_s_expression(
  node,
  show_anonymous = FALSE,
  show_locations = FALSE,
  dangling_parenthesis = FALSE
)
#> (program
#>   (binary_operator
#>     lhs: (identifier)
#>     rhs: (function_definition
#>       parameters: (parameters
#>         parameter: (parameter
#>           name: (identifier))
#>         (comma)
#>         parameter: (parameter
#>           name: (identifier)
#>           default: (float)))
#>       body: (braced_expression
#>         body: (binary_operator
#>           lhs: (binary_operator
#>             lhs: (identifier)
#>             rhs: (identifier))
#>           rhs: (float))))))
```
