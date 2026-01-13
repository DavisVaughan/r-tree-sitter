# Parse a snippet of text

`text_parse()` is a convenience utility for quickly parsing a small
snippet of text using a particular language and getting access to its
root node. It is meant for demonstration purposes. If you are going to
need to reparse the text after an edit has been made, you should create
a full parser with
[`parser()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/parser.md)
and use
[`parser_parse()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/parser-parse.md)
instead.

## Usage

``` r
text_parse(x, language)
```

## Arguments

- x:

  `[string]`

  The text to parse.

- language:

  `[tree_sitter_language]`

  The language to parse with.

## Value

A root node.

## Examples

``` r
language <- treesitter.r::language()
text <- "map(xs, function(x) 1 + 1)"

# Note that this directly returns the root node, not the tree
text_parse(text, language)
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> map(xs, function(x) 1 + 1)
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (program [(0, 0), (0, 26)]
#>   (call [(0, 0), (0, 26)]
#>     function: (identifier [(0, 0), (0, 3)])
#>     arguments: (arguments [(0, 3), (0, 26)]
#>       open: "(" [(0, 3), (0, 4)]
#>       argument: (argument [(0, 4), (0, 6)]
#>         value: (identifier [(0, 4), (0, 6)])
#>       )
#>       (comma [(0, 6), (0, 7)])
#>       argument: (argument [(0, 8), (0, 25)]
#>         value: (function_definition [(0, 8), (0, 25)]
#>           name: "function" [(0, 8), (0, 16)]
#>           parameters: (parameters [(0, 16), (0, 19)]
#>             open: "(" [(0, 16), (0, 17)]
#>             parameter: (parameter [(0, 17), (0, 18)]
#>               name: (identifier [(0, 17), (0, 18)])
#>             )
#>             close: ")" [(0, 18), (0, 19)]
#>           )
#>           body: (binary_operator [(0, 20), (0, 25)]
#>             lhs: (float [(0, 20), (0, 21)])
#>             operator: "+" [(0, 22), (0, 23)]
#>             rhs: (float [(0, 24), (0, 25)])
#>           )
#>         )
#> <truncated>
```
