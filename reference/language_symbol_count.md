# Language symbol count

Get the number of symbols contained within a language.

## Usage

``` r
language_symbol_count(x)
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
language_symbol_count(language)
#> [1] 135
```
