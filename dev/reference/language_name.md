# Language name

Extract a language object's language name.

## Usage

``` r
language_name(x)
```

## Arguments

- x:

  `[tree_sitter_language]`

  A tree-sitter language object.

## Value

A string.

## Examples

``` r
language <- treesitter.r::language()
language_name(language)
#> [1] "r"
```
