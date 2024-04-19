# single quoted strings throw an error

    Code
      query(language, source)
    Condition
      Error in `query()`:
      ! Can't initialize this query.
      i Syntax error at offset 31 in `source`.
      
      ```
      
      (binary_operator
        operator: <HERE>'+'
      )
        
      ```

# returns ordered list of captures

    Code
      captures$node
    Output
      [[1]]
      <tree_sitter_node>
      
      -- S-Expression ----------------------------------------------------------------
      (identifier [(1, 0), (1, 1)])
      
      -- Text ------------------------------------------------------------------------
      a
      
      [[2]]
      <tree_sitter_node>
      
      -- S-Expression ----------------------------------------------------------------
      (identifier [(1, 4), (1, 5)])
      
      -- Text ------------------------------------------------------------------------
      b
      
      [[3]]
      <tree_sitter_node>
      
      -- S-Expression ----------------------------------------------------------------
      (identifier [(1, 8), (1, 9)])
      
      -- Text ------------------------------------------------------------------------
      a
      
      [[4]]
      <tree_sitter_node>
      
      -- S-Expression ----------------------------------------------------------------
      (identifier [(2, 4), (2, 5)])
      
      -- Text ------------------------------------------------------------------------
      a
      

