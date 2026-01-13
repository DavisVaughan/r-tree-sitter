# Language field count

Get the number of fields contained within a language.

## Usage

``` r
language_field_count(x)
```

## Arguments

- x:

  `[tree_sitter_language]`

  A tree-sitter language object.

## Value

A single double value.

## Examples

``` r
language <- treesitter.r::language()
language_field_count(language)
#> [1] 20
```
