# Points

- `point()` creates a new tree-sitter point.

- `point_row()` and `point_column()` access a point's row and column
  value, respectively.

- `is_point()` determines whether or not an object is a point.

Note that points are 0-indexed. This is typically the easiest form to
work with them in, since most of the time when you are provided row and
column information from third party libraries, they will already be
0-indexed. It is also consistent with bytes, which are also 0-indexed
and are often provided alongside their corresponding points.

## Usage

``` r
point(row, column)

point_row(x)

point_column(x)

is_point(x)
```

## Arguments

- row:

  `[double(1)]`

  A 0-indexed row to place the point at.

- column:

  `[double(1)]`

  A 0-indexed column to place the point at.

- x:

  `[tree_sitter_point]`

  A point.

## Value

- `point()` returns a new point.

- `point_row()` and `point_column()` return a single double.

- `is_point()` returns `TRUE` or `FALSE`.

## Examples

``` r
x <- point(1, 2)

point_row(x)
#> [1] 1
point_column(x)
#> [1] 2

is_point(x)
#> [1] TRUE
```
