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

# recycles `named` to size of `name`

    Code
      language_symbol_for_name(r(), c("program", "else"), named = c(TRUE, FALSE, TRUE))
    Condition
      Error in `language_symbol_for_name()`:
      ! Can't recycle `named` (size 3) to size 2.

# language_symbol_for_name() validates inputs

    Code
      language_symbol_for_name(1, "foo")
    Condition
      Error in `language_symbol_for_name()`:
      ! `x` must be a <tree_sitter_language>, not the number 1.

---

    Code
      language_symbol_for_name(r(), 1)
    Condition
      Error in `language_symbol_for_name()`:
      ! Can't convert `name` <double> to <character>.

---

    Code
      language_symbol_for_name(r(), "foo", named = "x")
    Condition
      Error in `language_symbol_for_name()`:
      ! Can't convert `named` <character> to <logical>.

# errors on bad symbol IDs

    Code
      language_symbol_name(r(), -1L)
    Condition
      Error in `language_symbol_name()`:
      ! Can't convert `symbol` to `TSSymbol`. `symbol` must be within the range of `[0, UINT16_MAX]`.

# checks language type

    Code
      language_symbol_name(1, 1L)
    Condition
      Error in `language_symbol_name()`:
      ! `x` must be a <tree_sitter_language>, not the number 1.

# Language objects that are too old can be caught

    Code
      check_language_abi(x, min = 13L, max = 14L)
    Condition
      Error:
      ! `x` is an incompatible tree-sitter language object.
      i `x` has an ABI version of 12.
      i Minimum allowed ABI version is 13.
      i This typically means you should update the grammar R package associated with this language object, i.e. treesitter.{language}. If that doesn't work, please open an issue on GitHub.

# Language objects that are too new can be caught

    Code
      check_language_abi(x, min = 10L, max = 11L)
    Condition
      Error:
      ! `x` is an incompatible tree-sitter language object.
      i `x` has an ABI version of 12.
      i Maximum allowed ABI version is 11.
      i This typically means you should update the treesitter R package. If that doesn't work, please open an issue on GitHub.

# expected print method

    Code
      r()
    Output
      <tree_sitter_language>
      Language: r

