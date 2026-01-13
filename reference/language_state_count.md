# Language state count

Get the number of states traversable within a language.

## Usage

``` r
language_state_count(x)
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
language_state_count(language)
#> [1] 2072
```
