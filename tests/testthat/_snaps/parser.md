# can parse and reparse non-destructively

    Code
      tree
    Output
      <tree_sitter_tree>
      
      -- Text ------------------------------------------------------------------------
      1 + 1
      
      -- S-Expression ----------------------------------------------------------------
      (program [(0, 0), (0, 5)]
        (binary_operator [(0, 0), (0, 5)]
          lhs: (float [(0, 0), (0, 1)])
          operator: "+" [(0, 2), (0, 3)]
          rhs: (float [(0, 4), (0, 5)])
        )
      )

---

    Code
      new_tree
    Output
      <tree_sitter_tree>
      
      -- Text ------------------------------------------------------------------------
      xy + 1
      
      -- S-Expression ----------------------------------------------------------------
      (program [(0, 0), (0, 6)]
        (binary_operator [(0, 0), (0, 6)]
          lhs: (identifier [(0, 0), (0, 2)])
          operator: "+" [(0, 3), (0, 4)]
          rhs: (float [(0, 5), (0, 6)])
        )
      )

# expected print method

    Code
      parser(r())
    Output
      <tree_sitter_parser>
      Language: r

