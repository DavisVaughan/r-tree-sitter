# checks for valid object

    Code
      language_name(1)
    Condition
      Error in `language_name()`:
      ! `x` must be a <tree_sitter_language>, not the number 1.

---

    Code
      language_version(1)
    Condition
      Error in `language_version()`:
      ! `x` must be a <tree_sitter_language>, not the number 1.

# recycles `named` to size of `kind`

    Code
      language_id_for_node_kind(r(), c("program", "else"), named = c(TRUE, FALSE,
        TRUE))
    Condition
      Error in `language_id_for_node_kind()`:
      ! Can't recycle `named` (size 3) to size 2.

# language_id_for_node_kind() validates inputs

    Code
      language_id_for_node_kind(1, "foo")
    Condition
      Error in `language_id_for_node_kind()`:
      ! `x` must be a <tree_sitter_language>, not the number 1.

---

    Code
      language_id_for_node_kind(r(), 1)
    Condition
      Error in `language_id_for_node_kind()`:
      ! Can't convert `kind` <double> to <character>.

---

    Code
      language_id_for_node_kind(r(), "foo", named = "x")
    Condition
      Error in `language_id_for_node_kind()`:
      ! Can't convert `named` <character> to <logical>.

# errors on bad IDs

    Code
      language_node_kind_for_id(r(), -1L)
    Condition
      Error in `language_node_kind_for_id()`:
      ! Can't convert `id` to `TSSymbol`. `id` must be within the range of `[0, UINT16_MAX]`.

# checks language type

    Code
      language_node_kind_for_id(1, 1L)
    Condition
      Error in `language_node_kind_for_id()`:
      ! `x` must be a <tree_sitter_language>, not the number 1.

