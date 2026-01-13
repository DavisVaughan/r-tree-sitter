# Ranges

- `range()` creates a new tree-sitter range.

- `range_start_byte()` and `range_end_byte()` access a range's start and
  end bytes, respectively.

- `range_start_point()` and `range_end_point()` access a range's start
  and end points, respectively.

- `is_range()` determines whether or not an object is a range.

Note that the bytes and points used in ranges are 0-indexed.

## Usage

``` r
range(start_byte, start_point, end_byte, end_point)

range_start_byte(x)

range_start_point(x)

range_end_byte(x)

range_end_point(x)

is_range(x)
```

## Arguments

- start_byte, end_byte:

  `[double(1)]`

  0-indexed bytes for the start and end of the range, respectively.

- start_point, end_point:

  `[tree_sitter_point]`

  0-indexed points for the start and end of the range, respectively.

- x:

  `[tree_sitter_range]`

  A range.

## Value

- `range()` returns a new range.

- `range_start_byte()` and `range_end_byte()` return a single double.

- `range_start_point()` and `range_end_point()` return a
  [`point()`](https://davisvaughan.github.io/r-tree-sitter/reference/points.md).

- `is_range()` returns `TRUE` or `FALSE`.

## See also

[`node_range()`](https://davisvaughan.github.io/r-tree-sitter/reference/node-location.md)

## Examples

``` r
x <- range(5, point(1, 3), 7, point(1, 5))
x
#> <tree_sitter_range>
#> Start <byte: 5, row: 1, column: 3>
#> End <byte: 7, row: 1, column: 5>

range_start_byte(x)
#> [1] 5
range_end_byte(x)
#> [1] 7

range_start_point(x)
#> <tree_sitter_point>
#> Row: 1
#> Column: 3
range_end_point(x)
#> <tree_sitter_point>
#> Row: 1
#> Column: 5

is_range(x)
#> [1] TRUE
```
