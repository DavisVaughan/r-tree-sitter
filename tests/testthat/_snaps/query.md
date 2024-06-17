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
      
      -- Text ------------------------------------------------------------------------
      a
      
      -- S-Expression ----------------------------------------------------------------
      (identifier [(1, 0), (1, 1)])
      
      [[2]]
      <tree_sitter_node>
      
      -- Text ------------------------------------------------------------------------
      b
      
      -- S-Expression ----------------------------------------------------------------
      (identifier [(1, 4), (1, 5)])
      
      [[3]]
      <tree_sitter_node>
      
      -- Text ------------------------------------------------------------------------
      a
      
      -- S-Expression ----------------------------------------------------------------
      (identifier [(1, 8), (1, 9)])
      
      [[4]]
      <tree_sitter_node>
      
      -- Text ------------------------------------------------------------------------
      a
      
      -- S-Expression ----------------------------------------------------------------
      (identifier [(2, 4), (2, 5)])
      

