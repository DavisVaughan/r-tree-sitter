# Is `x` a query?

Checks if `x` is a `tree_sitter_query` or not.

## Usage

``` r
is_query(x)
```

## Arguments

- x:

  `[object]`

  An object.

## Value

`TRUE` if `x` is a `tree_sitter_query`, otherwise `FALSE`.

## Examples

``` r
source <- "(identifier) @id"
language <- treesitter.r::language()

query <- query(language, source)

is_query(query)
#> [1] TRUE

is_query(1)
#> [1] FALSE
```
