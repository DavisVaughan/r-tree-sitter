# Package index

## Parser

- [`parser_set_language()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/parser-adjustments.md)
  [`parser_set_timeout()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/parser-adjustments.md)
  [`parser_set_included_ranges()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/parser-adjustments.md)
  : Parser adjustments

- [`parser_parse()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/parser-parse.md)
  [`parser_reparse()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/parser-parse.md)
  : Parse or reparse text

- [`parser()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/parser.md)
  : Create a new parser

- [`is_parser()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/is_parser.md)
  :

  Is `x` a parser?

- [`text_parse()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/text_parse.md)
  : Parse a snippet of text

## Tree

- [`tree_included_ranges()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/tree-accessors.md)
  [`tree_text()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/tree-accessors.md)
  [`tree_language()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/tree-accessors.md)
  : Tree accessors

- [`tree_root_node()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/tree_root_node.md)
  : Retrieve the root node of the tree

- [`tree_root_node_with_offset()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/tree_root_node_with_offset.md)
  : Retrieve an offset root node

- [`tree_walk()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/tree_walk.md)
  :

  Generate a `TreeCursor` iterator

- [`is_tree()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/is_tree.md)
  :

  Is `x` a tree?

## Node

- [`node_child_by_field_id()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node-child-by-field.md)
  [`node_child_by_field_name()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node-child-by-field.md)
  : Get a node's child by field id or name

- [`node_child_count()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node-child-count.md)
  [`node_named_child_count()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node-child-count.md)
  : Get a node's child count

- [`node_child()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node-child.md)
  [`node_named_child()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node-child.md)
  : Get a node's child by index

- [`node_children()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node-children.md)
  [`node_named_children()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node-children.md)
  : Get a node's children

- [`node_descendant_for_byte_range()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node-descendant.md)
  [`node_named_descendant_for_byte_range()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node-descendant.md)
  [`node_descendant_for_point_range()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node-descendant.md)
  [`node_named_descendant_for_point_range()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node-descendant.md)
  : Node descendants

- [`node_field_name_for_child()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node-field-name-for-child.md)
  [`node_field_name_for_named_child()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node-field-name-for-child.md)
  : Get a child's field name by index

- [`node_first_child_for_byte()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node-first-child-byte.md)
  [`node_first_named_child_for_byte()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node-first-child-byte.md)
  : Get the first child that extends beyond the given byte offset

- [`node_grammar_type()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node-grammar.md)
  [`node_grammar_symbol()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node-grammar.md)
  : Node grammar types and symbols

- [`node_start_byte()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node-location.md)
  [`node_end_byte()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node-location.md)
  [`node_start_point()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node-location.md)
  [`node_end_point()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node-location.md)
  [`node_range()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node-location.md)
  : Node byte and point accessors

- [`node_is_named()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node-metadata.md)
  [`node_is_missing()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node-metadata.md)
  [`node_is_extra()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node-metadata.md)
  [`node_is_error()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node-metadata.md)
  [`node_has_error()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node-metadata.md)
  : Node metadata

- [`node_parse_state()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node-parse-state.md)
  [`node_next_parse_state()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node-parse-state.md)
  : Node parse states

- [`node_next_sibling()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node-sibling.md)
  [`node_next_named_sibling()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node-sibling.md)
  [`node_previous_sibling()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node-sibling.md)
  [`node_previous_named_sibling()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node-sibling.md)
  : Node sibling accessors

- [`node_descendant_count()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node_descendant_count.md)
  : Node descendant count

- [`node_language()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node_language.md)
  : Get a node's underlying language

- [`node_parent()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node_parent.md)
  : Get a node's parent

- [`node_raw_s_expression()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node_raw_s_expression.md)
  : "Raw" S-expression

- [`node_show_s_expression()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node_show_s_expression.md)
  :

  Pretty print a `node`'s s-expression

- [`node_symbol()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node_symbol.md)
  : Node symbol

- [`node_text()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node_text.md)
  : Get a node's underlying text

- [`node_type()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node_type.md)
  : Node type

- [`node_walk()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/node_walk.md)
  :

  Generate a `TreeCursor` iterator

- [`is_node()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/is_node.md)
  :

  Is `x` a node?

## Query

- [`query_pattern_count()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/query-accessors.md)
  [`query_capture_count()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/query-accessors.md)
  [`query_string_count()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/query-accessors.md)
  [`query_start_byte_for_pattern()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/query-accessors.md)
  [`query_end_byte_for_pattern()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/query-accessors.md)
  : Query accessors

- [`query_matches()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/query-matches-and-captures.md)
  [`query_captures()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/query-matches-and-captures.md)
  : Query matches and captures

- [`query()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/query.md)
  : Queries

- [`is_query()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/is_query.md)
  :

  Is `x` a query?

## Language

- [`language_field_count()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/language_field_count.md)
  : Language field count

- [`language_field_id_for_name()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/language_field_id_for_name.md)
  : Language field identifiers

- [`language_field_name_for_id()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/language_field_name_for_id.md)
  : Language field names

- [`language_name()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/language_name.md)
  : Language name

- [`language_next_state()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/language_next_state.md)
  : Language state advancement

- [`language_state_count()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/language_state_count.md)
  : Language state count

- [`language_symbol_count()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/language_symbol_count.md)
  : Language symbol count

- [`language_symbol_for_name()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/language_symbol_for_name.md)
  : Language symbols

- [`language_symbol_name()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/language_symbol_name.md)
  : Language symbol names

- [`is_language()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/is_language.md)
  :

  Is `x` a language?

## Point

- [`point()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/points.md)
  [`point_row()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/points.md)
  [`point_column()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/points.md)
  [`is_point()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/points.md)
  : Points

## Range

- [`range()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/ranges.md)
  [`range_start_byte()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/ranges.md)
  [`range_start_point()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/ranges.md)
  [`range_end_byte()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/ranges.md)
  [`range_end_point()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/ranges.md)
  [`is_range()`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/ranges.md)
  : Ranges

## Tree cursor

- [`TreeCursor`](https://davisvaughan.github.io/r-tree-sitter/dev/reference/TreeCursor.md)
  : Tree cursors
