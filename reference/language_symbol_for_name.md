# Language symbols

Get the integer symbol ID for a particular node name. Can be useful for
exploring the grammar.

## Usage

``` r
language_symbol_for_name(x, name, ..., named = TRUE)
```

## Arguments

- x:

  `[tree_sitter_language]`

  A tree-sitter language object.

- name:

  `[character]`

  The names to look up symbols for.

- ...:

  These dots are for future extensions and must be empty.

- named:

  `[logical]`

  Should named or anonymous nodes be looked up? Recycled to the size of
  `name`.

## Value

An integer vector the same size as `name` containing either:

- The integer symbol ID of the node name, if known.

- `NA` if the node name was not known.

## See also

[`language_symbol_name()`](https://davisvaughan.github.io/r-tree-sitter/reference/language_symbol_name.md)

## Examples

``` r
language <- treesitter.r::language()
language_symbol_for_name(language, "identifier")
#> [1] 1
```
