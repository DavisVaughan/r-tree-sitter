# Tree cursors

`TreeCursor` is an R6 class that allows you to walk a tree in a more
efficient way than calling `node_*()` functions like
[`node_child()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node-child.md)
repeatedly.

You can also more elegantly create a cursor with
[`node_walk()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node_walk.md)
and
[`tree_walk()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/tree_walk.md).

## Value

R6 object representing the tree cursor.

## Methods

### Public methods

- [`TreeCursor$new()`](#method-tree_sitter_tree_cursor-new)

- [`TreeCursor$reset()`](#method-tree_sitter_tree_cursor-reset)

- [`TreeCursor$node()`](#method-tree_sitter_tree_cursor-node)

- [`TreeCursor$field_name()`](#method-tree_sitter_tree_cursor-field_name)

- [`TreeCursor$field_id()`](#method-tree_sitter_tree_cursor-field_id)

- [`TreeCursor$descendant_index()`](#method-tree_sitter_tree_cursor-descendant_index)

- [`TreeCursor$goto_parent()`](#method-tree_sitter_tree_cursor-goto_parent)

- [`TreeCursor$goto_next_sibling()`](#method-tree_sitter_tree_cursor-goto_next_sibling)

- [`TreeCursor$goto_previous_sibling()`](#method-tree_sitter_tree_cursor-goto_previous_sibling)

- [`TreeCursor$goto_first_child()`](#method-tree_sitter_tree_cursor-goto_first_child)

- [`TreeCursor$goto_last_child()`](#method-tree_sitter_tree_cursor-goto_last_child)

- [`TreeCursor$depth()`](#method-tree_sitter_tree_cursor-depth)

- [`TreeCursor$goto_first_child_for_byte()`](#method-tree_sitter_tree_cursor-goto_first_child_for_byte)

- [`TreeCursor$goto_first_child_for_point()`](#method-tree_sitter_tree_cursor-goto_first_child_for_point)

------------------------------------------------------------------------

### Method `new()`

Create a new tree cursor.

#### Usage

    TreeCursor$new(node)

#### Arguments

- `node`:

  `[tree_sitter_node]`

  The node to start walking from.

------------------------------------------------------------------------

### Method `reset()`

Reset the tree cursor to a new root node.

#### Usage

    TreeCursor$reset(node)

#### Arguments

- `node`:

  `[tree_sitter_node]`

  The node to start walking from.

------------------------------------------------------------------------

### Method `node()`

Get the current node that the cursor points to.

#### Usage

    TreeCursor$node()

------------------------------------------------------------------------

### Method `field_name()`

Get the field name of the current node.

#### Usage

    TreeCursor$field_name()

------------------------------------------------------------------------

### Method `field_id()`

Get the field id of the current node.

#### Usage

    TreeCursor$field_id()

------------------------------------------------------------------------

### Method `descendant_index()`

Get the descendent index of the current node.

#### Usage

    TreeCursor$descendant_index()

------------------------------------------------------------------------

### Method `goto_parent()`

Go to the current node's parent.

Returns `TRUE` if a parent was found, and `FALSE` if not.

#### Usage

    TreeCursor$goto_parent()

------------------------------------------------------------------------

### Method `goto_next_sibling()`

Go to the current node's next sibling.

Returns `TRUE` if a sibling was found, and `FALSE` if not.

#### Usage

    TreeCursor$goto_next_sibling()

------------------------------------------------------------------------

### Method `goto_previous_sibling()`

Go to the current node's previous sibling.

Returns `TRUE` if a sibling was found, and `FALSE` if not.

#### Usage

    TreeCursor$goto_previous_sibling()

------------------------------------------------------------------------

### Method `goto_first_child()`

Go to the current node's first child.

Returns `TRUE` if a child was found, and `FALSE` if not.

#### Usage

    TreeCursor$goto_first_child()

------------------------------------------------------------------------

### Method `goto_last_child()`

Go to the current node's last child.

Returns `TRUE` if a child was found, and `FALSE` if not.

#### Usage

    TreeCursor$goto_last_child()

------------------------------------------------------------------------

### Method `depth()`

Get the depth of the current node.

#### Usage

    TreeCursor$depth()

------------------------------------------------------------------------

### Method `goto_first_child_for_byte()`

Move the cursor to the first child of its current node that extends
beyond the given byte offset.

Returns `TRUE` if a child was found, and `FALSE` if not.

#### Usage

    TreeCursor$goto_first_child_for_byte(byte)

#### Arguments

- `byte`:

  `[double(1)]`

  The byte to move the cursor past.

------------------------------------------------------------------------

### Method `goto_first_child_for_point()`

Move the cursor to the first child of its current node that extends
beyond the given point.

Returns `TRUE` if a child was found, and `FALSE` if not.

#### Usage

    TreeCursor$goto_first_child_for_point(point)

#### Arguments

- `point`:

  `[tree_sitter_point]`

  The point to move the cursor past.

## Examples

``` r
language <- treesitter.r::language()
parser <- parser(language)

text <- "fn <- function(a, b) { a + b }"

tree <- parser_parse(parser, text)
node <- tree_root_node(tree)

cursor <- TreeCursor$new(node)

cursor$node()
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> fn <- function(a, b) { a + b }
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (program [(0, 0), (0, 30)]
#>   (binary_operator [(0, 0), (0, 30)]
#>     lhs: (identifier [(0, 0), (0, 2)])
#>     operator: "<-" [(0, 3), (0, 5)]
#>     rhs: (function_definition [(0, 6), (0, 30)]
#>       name: "function" [(0, 6), (0, 14)]
#>       parameters: (parameters [(0, 14), (0, 20)]
#>         open: "(" [(0, 14), (0, 15)]
#>         parameter: (parameter [(0, 15), (0, 16)]
#>           name: (identifier [(0, 15), (0, 16)])
#>         )
#>         (comma [(0, 16), (0, 17)])
#>         parameter: (parameter [(0, 18), (0, 19)]
#>           name: (identifier [(0, 18), (0, 19)])
#>         )
#>         close: ")" [(0, 19), (0, 20)]
#>       )
#>       body: (braced_expression [(0, 21), (0, 30)]
#>         open: "{" [(0, 21), (0, 22)]
#>         body: (binary_operator [(0, 23), (0, 28)]
#>           lhs: (identifier [(0, 23), (0, 24)])
#>           operator: "+" [(0, 25), (0, 26)]
#>           rhs: (identifier [(0, 27), (0, 28)])
#>         )
#>         close: "}" [(0, 29), (0, 30)]
#> <truncated>
cursor$goto_first_child()
#> [1] TRUE
cursor$goto_first_child()
#> [1] TRUE
cursor$node()
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> fn
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> (identifier [(0, 0), (0, 2)])
cursor$goto_next_sibling()
#> [1] TRUE
cursor$node()
#> <tree_sitter_node>
#> 
#> ── Text ───────────────────────────────────────────────────────────────
#> <-
#> 
#> ── S-Expression ───────────────────────────────────────────────────────
#> "<-" [(0, 3), (0, 5)]
```
