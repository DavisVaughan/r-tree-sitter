# Is `x` a node?

Checks if `x` is a `tree_sitter_node` or not.

## Usage

``` r
is_node(x)
```

## Arguments

- x:

  `[object]`

  An object.

## Value

`TRUE` if `x` is a `tree_sitter_node`, otherwise `FALSE`.

## Examples

``` r
language <- treesitter.r::language()
parser <- parser(language)

text <- "fn <- function() { 1 + 1 }"
tree <- parser_parse(parser, text)
node <- tree_root_node(tree)

is_node(node)
#> [1] TRUE

is_node(1)
#> [1] FALSE
```
