# Is `x` a parser?

Checks if `x` is a `tree_sitter_parser` or not.

## Usage

``` r
is_parser(x)
```

## Arguments

- x:

  `[object]`

  An object.

## Value

`TRUE` if `x` is a `tree_sitter_parser`, otherwise `FALSE`.

## Examples

``` r
language <- treesitter.r::language()
parser <- parser(language)

is_parser(parser)
#> [1] TRUE

is_parser(1)
#> [1] FALSE
```
