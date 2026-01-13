# Create a new parser

`parser()` constructs a parser from a tree-sitter `language` object. You
can use
[`parser_parse()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/parser-parse.md)
to parse language specific text with it.

## Usage

``` r
parser(language)
```

## Arguments

- language:

  `[tree_sitter_language]`

  A language object.

## Value

A new parser.

## Examples

``` r
language <- treesitter.r::language()
parser <- parser(language)
parser
#> <tree_sitter_parser>
#> Language: r

text <- "1 + foo"
tree <- parser_parse(parser, text)
tree
#> <tree_sitter_tree>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> 1 + foo
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (program [(0, 0), (0, 7)]
#>   (binary_operator [(0, 0), (0, 7)]
#>     lhs: (float [(0, 0), (0, 1)])
#>     operator: "+" [(0, 2), (0, 3)]
#>     rhs: (identifier [(0, 4), (0, 7)])
#>   )
#> )
```
