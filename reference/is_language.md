# Is `x` a language?

Use `is_language()` to determine if an object has a class of
`"tree_sitter_language"`.

## Usage

``` r
is_language(x)
```

## Arguments

- x:

  `[object]`

  An object.

## Value

- `TRUE` if `x` is a `"tree_sitter_language"`.

- `FALSE` otherwise.

## Examples

``` r
language <- treesitter.r::language()
is_language(language)
#> [1] TRUE
```
