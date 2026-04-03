# Changelog

## treesitter 0.3.2

CRAN release: 2026-04-03

- Removed usage of more non-API C functions.

## treesitter 0.3.1

CRAN release: 2026-01-13

- Removed usage of non-API `ATTRIB()` and `SET_ATTRIB()`
  ([\#41](https://github.com/DavisVaughan/r-tree-sitter/issues/41)).

## treesitter 0.3.0

CRAN release: 2025-06-06

- New
  [`node_field_name_for_named_child()`](https://davisvaughan.github.io/r-tree-sitter/reference/node-field-name-for-child.md)
  ([\#35](https://github.com/DavisVaughan/r-tree-sitter/issues/35)).

- Updated to [tree-sitter
  v0.24.7](https://github.com/tree-sitter/tree-sitter/releases/tag/v0.24.7)
  ([\#35](https://github.com/DavisVaughan/r-tree-sitter/issues/35)).

## treesitter 0.2.0

CRAN release: 2025-04-08

- New
  [`query_end_byte_for_pattern()`](https://davisvaughan.github.io/r-tree-sitter/reference/query-accessors.md)
  ([\#22](https://github.com/DavisVaughan/r-tree-sitter/issues/22)).

- [`query_captures()`](https://davisvaughan.github.io/r-tree-sitter/reference/query-matches-and-captures.md)
  and
  [`query_matches()`](https://davisvaughan.github.io/r-tree-sitter/reference/query-matches-and-captures.md)
  gain support for `any-` style predicates (initial PR by
  [@kylebutts](https://github.com/kylebutts),
  [\#30](https://github.com/DavisVaughan/r-tree-sitter/issues/30)).

- [`query_captures()`](https://davisvaughan.github.io/r-tree-sitter/reference/query-matches-and-captures.md)
  and
  [`query_matches()`](https://davisvaughan.github.io/r-tree-sitter/reference/query-matches-and-captures.md)
  now have an expansive examples section
  ([\#30](https://github.com/DavisVaughan/r-tree-sitter/issues/30)).

- Fixed an issue with `#eq?` predicates where extra partial matches
  could be returned
  ([\#28](https://github.com/DavisVaughan/r-tree-sitter/issues/28)).

- Updated to [tree-sitter
  v0.23.0](https://github.com/tree-sitter/tree-sitter/releases/tag/v0.23.0)
  ([\#23](https://github.com/DavisVaughan/r-tree-sitter/issues/23)).

- Removed usage of non-API C calls
  ([\#33](https://github.com/DavisVaughan/r-tree-sitter/issues/33)).

## treesitter 0.1.0

CRAN release: 2024-06-24

- Initial CRAN submission.
