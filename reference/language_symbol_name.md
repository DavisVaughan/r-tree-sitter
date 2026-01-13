# Language symbol names

Get the name for a particular language symbol ID. Can be useful for
exploring a grammar.

## Usage

``` r
language_symbol_name(x, symbol)
```

## Arguments

- x:

  `[tree_sitter_language]`

  A tree-sitter language object.

- symbol:

  `[positive integer]`

  The language symbols to look up names for.

## Value

A character vector the same length as `symbol` containing:

- The name of the symbol, if known.

- `NA`, if the symbol was not known.

## See also

[`language_symbol_for_name()`](https://davisvaughan.github.io/r-tree-sitter/reference/language_symbol_for_name.md)

## Examples

``` r
language <- treesitter.r::language()
language_symbol_name(language, 1)
#> [1] "identifier"
```
