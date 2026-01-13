# Node grammar types and symbols

- `node_grammar_type()` gets the node's type as it appears in the
  grammar, *ignoring aliases*.

- `node_grammar_symbol()` gets the node's symbol (the type as a numeric
  id) as it appears in the grammar, *ignoring aliases*. This should be
  used in
  [`language_next_state()`](https://davisvaughan.github.io/r-tree-sitter/reference/language_next_state.md)
  rather than
  [`node_symbol()`](https://davisvaughan.github.io/r-tree-sitter/reference/node_symbol.md).

## Usage

``` r
node_grammar_type(x)

node_grammar_symbol(x)
```

## Arguments

- x:

  `[tree_sitter_node]`

  A node.

## See also

[`node_type()`](https://davisvaughan.github.io/r-tree-sitter/reference/node_type.md),
[`node_symbol()`](https://davisvaughan.github.io/r-tree-sitter/reference/node_symbol.md)

## Examples

``` r
language <- treesitter.r::language()
parser <- parser(language)

text <- "fn <- function() { 1 + 1 }"
tree <- parser_parse(parser, text)
node <- tree_root_node(tree)

# Examples for these functions are highly specific to the grammar,
# because they relies on the placement of `alias()` calls in the grammar.
node_grammar_type(node)
#> [1] "program"
node_grammar_symbol(node)
#> [1] 81
```
