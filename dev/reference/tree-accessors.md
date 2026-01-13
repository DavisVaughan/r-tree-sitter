# Tree accessors

- `tree_text()` retrieves the tree's `text` that it was parsed with.

- `tree_language()` retrieves the tree's `language` that it was parsed
  with.

- `tree_included_ranges()` retrieves the tree's `included_ranges` that
  were provided to
  [`parser_set_included_ranges()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/parser-adjustments.md).
  Note that if no ranges were provided originally, then this still
  returns a default that always covers the entire document.

## Usage

``` r
tree_included_ranges(x)

tree_text(x)

tree_language(x)
```

## Arguments

- x:

  `[tree_sitter_tree]`

  A tree.

## Value

- `tree_text()` returns a string.

- `tree_language()` returns a `tree_sitter_language`.

- `tree_included_ranges()` returns a list of
  [`range()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/ranges.md)
  objects.

## Examples

``` r
language <- treesitter.r::language()
parser <- parser(language)

text <- "1 + foo"
tree <- parser_parse(parser, text)

tree_text(tree)
#> [1] "1 + foo"
tree_language(tree)
#> <tree_sitter_language>
#> Language: r
tree_included_ranges(tree)
#> [[1]]
#> <tree_sitter_range>
#> Start <byte: 0, row: 0, column: 0>
#> End <byte: 4294967295, row: 4294967295, column: 4294967295>
#> 
```
