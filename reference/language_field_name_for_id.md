# Language field names

Get the field name for a field identifier.

## Usage

``` r
language_field_name_for_id(x, id)
```

## Arguments

- x:

  `[tree_sitter_language]`

  A tree-sitter language object.

- id:

  `[integer]`

  The language field identifiers to look up field names for.

## Value

A character vector the same length as `id` containing:

- The field name for the field identifier, if known.

- `NA`, if the field identifier was not known.

## See also

[`language_field_id_for_name()`](https://davisvaughan.github.io/r-tree-sitter/reference/language_field_id_for_name.md)

## Examples

``` r
language <- treesitter.r::language()
language_field_name_for_id(language, 1)
#> [1] "alternative"
```
