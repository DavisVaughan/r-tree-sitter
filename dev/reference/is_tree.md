# Is `x` a tree?

Checks if `x` is a `tree_sitter_tree` or not.

## Usage

``` r
is_tree(x)
```

## Arguments

- x:

  `[object]`

  An object.

## Value

`TRUE` if `x` is a `tree_sitter_tree`, otherwise `FALSE`.

## Examples

``` r
language <- treesitter.r::language()
parser <- parser(language)

text <- "fn <- function() { 1 + 1 }"
tree <- parser_parse(parser, text)

is_tree(tree)
#> [1] TRUE

is_tree(1)
#> [1] FALSE
```
