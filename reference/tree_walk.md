# Generate a `TreeCursor` iterator

`tree_walk()` creates a
[TreeCursor](https://davisvaughan.github.io/r-tree-sitter/reference/TreeCursor.md)
starting at the root node. You can use it to "walk" the tree more
efficiently than using
[`node_child()`](https://davisvaughan.github.io/r-tree-sitter/reference/node-child.md)
and other similar node functions.

## Usage

``` r
tree_walk(x)
```

## Arguments

- x:

  `[tree_sitter_tree]`

  A tree.

## Value

A `TreeCursor` object.

## Examples

``` r
language <- treesitter.r::language()
parser <- parser(language)

text <- "1 + foo"
tree <- parser_parse(parser, text)

cursor <- tree_walk(tree)

cursor$goto_first_child()
#> [1] TRUE
cursor$goto_first_child()
#> [1] TRUE
cursor$node()
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> 1
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (float [(0, 0), (0, 1)])
cursor$goto_next_sibling()
#> [1] TRUE
cursor$node()
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> +
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> "+" [(0, 2), (0, 3)]
```
