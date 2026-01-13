# Parser adjustments

- `parser_set_language()` sets the language of the parser. This is
  usually done by
  [`parser()`](https://davisvaughan.github.io/r-tree-sitter/reference/parser.md)
  though.

- `parser_set_timeout()` sets an optional timeout used when calling
  [`parser_parse()`](https://davisvaughan.github.io/r-tree-sitter/reference/parser-parse.md)
  or
  [`parser_reparse()`](https://davisvaughan.github.io/r-tree-sitter/reference/parser-parse.md).
  If the timeout is hit, an error occurs.

- `parser_set_included_ranges()` sets an optional list of ranges that
  are the only locations considered when parsing. The ranges are created
  by
  [`range()`](https://davisvaughan.github.io/r-tree-sitter/reference/ranges.md).

## Usage

``` r
parser_set_language(x, language)

parser_set_timeout(x, timeout)

parser_set_included_ranges(x, included_ranges)
```

## Arguments

- x:

  `[tree_sitter_parser]`

  A parser.

- language:

  `[tree_sitter_language]`

  A language.

- timeout:

  `[double(1)]`

  A single whole number corresponding to a timeout in microseconds to
  use when parsing.

- included_ranges:

  `[list_of<tree_sitter_range>]`

  A list of ranges constructed by
  [`range()`](https://davisvaughan.github.io/r-tree-sitter/reference/ranges.md).
  These are the only locations that will be considered when parsing.

  An empty list can be used to clear any existing ranges so that the
  parser will again parse the entire document.

## Value

A new parser.

## Examples

``` r
language <- treesitter.r::language()
parser <- parser(language)
parser_set_timeout(parser, 10000)
#> <tree_sitter_parser>
#> Language: r
```
