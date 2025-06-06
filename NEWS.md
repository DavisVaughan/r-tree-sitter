# treesitter 0.3.0

* New `node_field_name_for_named_child()` (#35).

* Updated to [tree-sitter v0.24.7](https://github.com/tree-sitter/tree-sitter/releases/tag/v0.24.7) (#35).

# treesitter 0.2.0

* New `query_end_byte_for_pattern()` (#22).

* `query_captures()` and `query_matches()` gain support for `any-` style predicates (initial PR by @kylebutts, #30).

* `query_captures()` and `query_matches()` now have an expansive examples section (#30).

* Fixed an issue with `#eq?` predicates where extra partial matches could be returned (#28).

* Updated to [tree-sitter v0.23.0](https://github.com/tree-sitter/tree-sitter/releases/tag/v0.23.0) (#23).

* Removed usage of non-API C calls (#33).

# treesitter 0.1.0

* Initial CRAN submission.
