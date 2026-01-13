# Parse or reparse text

- `parser_parse()` performs an initial parse of `text`, a string
  typically containing contents of a file. It returns a `tree` for
  further manipulations.

- `parser_reparse()` performs a fast incremental reparse. `text` is
  typically a slightly modified version of the original `text` with a
  new "edit" applied. The position of the edit is described by the byte
  and point arguments to this function. The `tree` argument corresponds
  to the original `tree` returned by `parser_parse()`.

All bytes and points should be 0-indexed.

## Usage

``` r
parser_parse(x, text, ..., encoding = "UTF-8")

parser_reparse(
  x,
  text,
  tree,
  start_byte,
  start_point,
  old_end_byte,
  old_end_point,
  new_end_byte,
  new_end_point,
  ...,
  encoding = "UTF-8"
)
```

## Arguments

- x:

  `[tree_sitter_parser]`

  A parser.

- text:

  `[string]`

  The text to parse.

- ...:

  These dots are for future extensions and must be empty.

- encoding:

  `[string]`

  The expected encoding of the `text`. Either `"UTF-8"` or `"UTF-16"`.

- tree:

  `[tree_sitter_tree]`

  The original tree returned by `parser_parse()`. Components of the tree
  will be reused to perform the incremental reparse.

- start_byte, start_point:

  `[double(1) / tree_sitter_point]`

  The starting byte and starting point of the edit location.

- old_end_byte, old_end_point:

  `[double(1) / tree_sitter_point]`

  The old ending byte and old ending point of the edit location.

- new_end_byte, new_end_point:

  `[double(1) / tree_sitter_point]`

  The new ending byte and new ending point of the edit location.

## Value

A new `tree`.

## Examples

``` r
language <- treesitter.r::language()
parser <- parser(language)

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

text <- "1 + bar(foo)"
parser_reparse(
  parser,
  text,
  tree,
  start_byte = 4,
  start_point = point(0, 4),
  old_end_byte = 7,
  old_end_point = point(0, 7),
  new_end_byte = 12,
  new_end_point = point(0, 12)
)
#> <tree_sitter_tree>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> 1 + bar(foo)
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (program [(0, 0), (0, 12)]
#>   (binary_operator [(0, 0), (0, 12)]
#>     lhs: (float [(0, 0), (0, 1)])
#>     operator: "+" [(0, 2), (0, 3)]
#>     rhs: (call [(0, 4), (0, 12)]
#>       function: (identifier [(0, 4), (0, 7)])
#>       arguments: (arguments [(0, 7), (0, 12)]
#>         open: "(" [(0, 7), (0, 8)]
#>         argument: (argument [(0, 8), (0, 11)]
#>           value: (identifier [(0, 8), (0, 11)])
#>         )
#>         close: ")" [(0, 11), (0, 12)]
#>       )
#>     )
#>   )
#> )
```
