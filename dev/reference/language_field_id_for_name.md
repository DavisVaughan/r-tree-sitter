# Language field identifiers

Get the integer field identifier for a field name. If you are going to
be using a field name repeatedly, it is often a little faster to use the
corresponding field identifier instead.

## Usage

``` r
language_field_id_for_name(x, name)
```

## Arguments

- x:

  `[tree_sitter_language]`

  A tree-sitter language object.

- name:

  `[character]`

  The language field names to look up field identifiers for.

## Value

An integer vector the same length as `name` containing:

- The field identifier for the field name, if known.

- `NA`, if the field name was not known.

## See also

[`language_field_name_for_id()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/language_field_name_for_id.md)

## Examples

``` r
language <- treesitter.r::language()
language_field_id_for_name(language, "lhs")
#> [1] 11
```
