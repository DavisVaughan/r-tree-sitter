# Language state advancement

Get the next state in the grammar.

## Usage

``` r
language_next_state(x, state, symbol)
```

## Arguments

- x:

  `[tree_sitter_language]`

  A tree-sitter language object.

- state, symbol:

  `[integer]`

  Vectors of equal length containing the current state and symbol
  information.

## Value

A single integer representing the next state.

## Examples

``` r
language <- treesitter.r::language()
parser <- parser(language)

text <- "fn <- function() { 1 + 1 }"
tree <- parser_parse(parser, text)
node <- tree_root_node(tree)

# Navigate to function definition
node <- node_child(node, 1)
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

state <- node_parse_state(node)
symbol <- node_grammar_symbol(node)

# Function definition symbol
language_symbol_name(language, 85)
#> [1] "_parameter_with_default"

# Next state (this is all grammar dependent)
language_next_state(language, state, symbol)
#> [1] 198
```
