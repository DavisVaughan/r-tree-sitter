# Generate a `TreeCursor` iterator

`node_walk()` creates a
[TreeCursor](https://davisvaughan.github.io/r-tree-sitter/dev/reference/TreeCursor.md)
starting at the current node. You can use it to "walk" the tree more
efficiently than using
[`node_child()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node-child.md)
and other similar node functions.

## Usage

``` r
node_walk(x)
```

## Arguments

- x:

  `[tree_sitter_node]`

  A node.

## Value

A `TreeCursor` object.

## Examples

``` r
language <- treesitter.r::language()
parser <- parser(language)

text <- "1 + foo"
tree <- parser_parse(parser, text)
node <- tree_root_node(tree)

cursor <- node_walk(node)

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
