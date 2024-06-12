# can pretty print `node`s

    Code
      node_show_s_expression(node)
    Output
      (program [(1, 0), (9, 2)]
        (binary_operator [(1, 2), (6, 3)]
          lhs: (identifier [(1, 2), (1, 4)])
          operator: "<-" [(1, 5), (1, 7)]
          rhs: (function_definition [(1, 8), (6, 3)]
            name: "function" [(1, 8), (1, 16)]
            parameters: (parameters [(1, 16), (1, 18)]
              open: "(" [(1, 16), (1, 17)]
              close: ")" [(1, 17), (1, 18)]
            )
            body: (braced_expression [(1, 19), (6, 3)]
              open: "{" [(1, 19), (1, 20)]
              body: (binary_operator [(2, 4), (2, 14)]
                lhs: (identifier [(2, 4), (2, 5)])
                operator: "<-" [(2, 6), (2, 8)]
                rhs: (binary_operator [(2, 9), (2, 14)]
                  lhs: (float [(2, 9), (2, 10)])
                  operator: "+" [(2, 11), (2, 12)]
                  rhs: (float [(2, 13), (2, 14)])
                )
              )
              body: (if_statement [(3, 4), (5, 5)]
                "if" [(3, 4), (3, 6)]
                open: "(" [(3, 7), (3, 8)]
                condition: (binary_operator [(3, 8), (3, 13)]
                  lhs: (identifier [(3, 8), (3, 9)])
                  operator: ">" [(3, 10), (3, 11)]
                  rhs: (float [(3, 12), (3, 13)])
                )
                close: ")" [(3, 13), (3, 14)]
                consequence: (braced_expression [(3, 15), (5, 5)]
                  open: "{" [(3, 15), (3, 16)]
                  body: (true [(4, 6), (4, 10)])
                  close: "}" [(5, 4), (5, 5)]
                )
              )
              close: "}" [(6, 2), (6, 3)]
            )
          )
        )
        (binary_operator [(8, 2), (8, 7)]
          lhs: (float [(8, 2), (8, 3)])
          operator: "+" [(8, 4), (8, 5)]
          rhs: (float [(8, 6), (8, 7)])
        )
      )

---

    Code
      node_show_s_expression(node, show_anonymous = FALSE)
    Output
      (program [(1, 0), (9, 2)]
        (binary_operator [(1, 2), (6, 3)]
          lhs: (identifier [(1, 2), (1, 4)])
          rhs: (function_definition [(1, 8), (6, 3)]
            parameters: (parameters [(1, 16), (1, 18)])
            body: (braced_expression [(1, 19), (6, 3)]
              body: (binary_operator [(2, 4), (2, 14)]
                lhs: (identifier [(2, 4), (2, 5)])
                rhs: (binary_operator [(2, 9), (2, 14)]
                  lhs: (float [(2, 9), (2, 10)])
                  rhs: (float [(2, 13), (2, 14)])
                )
              )
              body: (if_statement [(3, 4), (5, 5)]
                condition: (binary_operator [(3, 8), (3, 13)]
                  lhs: (identifier [(3, 8), (3, 9)])
                  rhs: (float [(3, 12), (3, 13)])
                )
                consequence: (braced_expression [(3, 15), (5, 5)]
                  body: (true [(4, 6), (4, 10)])
                )
              )
            )
          )
        )
        (binary_operator [(8, 2), (8, 7)]
          lhs: (float [(8, 2), (8, 3)])
          rhs: (float [(8, 6), (8, 7)])
        )
      )

---

    Code
      node_show_s_expression(node, show_parentheses = FALSE)
    Output
      program [(1, 0), (9, 2)]
        binary_operator [(1, 2), (6, 3)]
          lhs: identifier [(1, 2), (1, 4)]
          operator: "<-" [(1, 5), (1, 7)]
          rhs: function_definition [(1, 8), (6, 3)]
            name: "function" [(1, 8), (1, 16)]
            parameters: parameters [(1, 16), (1, 18)]
              open: "(" [(1, 16), (1, 17)]
              close: ")" [(1, 17), (1, 18)]
            body: braced_expression [(1, 19), (6, 3)]
              open: "{" [(1, 19), (1, 20)]
              body: binary_operator [(2, 4), (2, 14)]
                lhs: identifier [(2, 4), (2, 5)]
                operator: "<-" [(2, 6), (2, 8)]
                rhs: binary_operator [(2, 9), (2, 14)]
                  lhs: float [(2, 9), (2, 10)]
                  operator: "+" [(2, 11), (2, 12)]
                  rhs: float [(2, 13), (2, 14)]
              body: if_statement [(3, 4), (5, 5)]
                "if" [(3, 4), (3, 6)]
                open: "(" [(3, 7), (3, 8)]
                condition: binary_operator [(3, 8), (3, 13)]
                  lhs: identifier [(3, 8), (3, 9)]
                  operator: ">" [(3, 10), (3, 11)]
                  rhs: float [(3, 12), (3, 13)]
                close: ")" [(3, 13), (3, 14)]
                consequence: braced_expression [(3, 15), (5, 5)]
                  open: "{" [(3, 15), (3, 16)]
                  body: true [(4, 6), (4, 10)]
                  close: "}" [(5, 4), (5, 5)]
              close: "}" [(6, 2), (6, 3)]
        binary_operator [(8, 2), (8, 7)]
          lhs: float [(8, 2), (8, 3)]
          operator: "+" [(8, 4), (8, 5)]
          rhs: float [(8, 6), (8, 7)]

---

    Code
      node_show_s_expression(node, show_parentheses = FALSE, show_anonymous = FALSE,
        show_locations = FALSE)
    Output
      program
        binary_operator
          lhs: identifier
          rhs: function_definition
            parameters: parameters
            body: braced_expression
              body: binary_operator
                lhs: identifier
                rhs: binary_operator
                  lhs: float
                  rhs: float
              body: if_statement
                condition: binary_operator
                  lhs: identifier
                  rhs: float
                consequence: braced_expression
                  body: true
        binary_operator
          lhs: float
          rhs: float

---

    Code
      node_show_s_expression(node, dangling_parenthesis = FALSE)
    Output
      (program [(1, 0), (9, 2)]
        (binary_operator [(1, 2), (6, 3)]
          lhs: (identifier [(1, 2), (1, 4)])
          operator: "<-" [(1, 5), (1, 7)]
          rhs: (function_definition [(1, 8), (6, 3)]
            name: "function" [(1, 8), (1, 16)]
            parameters: (parameters [(1, 16), (1, 18)]
              open: "(" [(1, 16), (1, 17)]
              close: ")" [(1, 17), (1, 18)])
            body: (braced_expression [(1, 19), (6, 3)]
              open: "{" [(1, 19), (1, 20)]
              body: (binary_operator [(2, 4), (2, 14)]
                lhs: (identifier [(2, 4), (2, 5)])
                operator: "<-" [(2, 6), (2, 8)]
                rhs: (binary_operator [(2, 9), (2, 14)]
                  lhs: (float [(2, 9), (2, 10)])
                  operator: "+" [(2, 11), (2, 12)]
                  rhs: (float [(2, 13), (2, 14)])))
              body: (if_statement [(3, 4), (5, 5)]
                "if" [(3, 4), (3, 6)]
                open: "(" [(3, 7), (3, 8)]
                condition: (binary_operator [(3, 8), (3, 13)]
                  lhs: (identifier [(3, 8), (3, 9)])
                  operator: ">" [(3, 10), (3, 11)]
                  rhs: (float [(3, 12), (3, 13)]))
                close: ")" [(3, 13), (3, 14)]
                consequence: (braced_expression [(3, 15), (5, 5)]
                  open: "{" [(3, 15), (3, 16)]
                  body: (true [(4, 6), (4, 10)])
                  close: "}" [(5, 4), (5, 5)]))
              close: "}" [(6, 2), (6, 3)])))
        (binary_operator [(8, 2), (8, 7)]
          lhs: (float [(8, 2), (8, 3)])
          operator: "+" [(8, 4), (8, 5)]
          rhs: (float [(8, 6), (8, 7)])))

---

    Code
      node_show_s_expression(node, show_locations = FALSE)
    Output
      (program
        (binary_operator
          lhs: (identifier)
          operator: "<-"
          rhs: (function_definition
            name: "function"
            parameters: (parameters
              open: "("
              close: ")"
            )
            body: (braced_expression
              open: "{"
              body: (binary_operator
                lhs: (identifier)
                operator: "<-"
                rhs: (binary_operator
                  lhs: (float)
                  operator: "+"
                  rhs: (float)
                )
              )
              body: (if_statement
                "if"
                open: "("
                condition: (binary_operator
                  lhs: (identifier)
                  operator: ">"
                  rhs: (float)
                )
                close: ")"
                consequence: (braced_expression
                  open: "{"
                  body: (true)
                  close: "}"
                )
              )
              close: "}"
            )
          )
        )
        (binary_operator
          lhs: (float)
          operator: "+"
          rhs: (float)
        )
      )

---

    Code
      node_show_s_expression(node, dangling_parenthesis = TRUE, show_anonymous = FALSE)
    Output
      (program [(1, 0), (9, 2)]
        (binary_operator [(1, 2), (6, 3)]
          lhs: (identifier [(1, 2), (1, 4)])
          rhs: (function_definition [(1, 8), (6, 3)]
            parameters: (parameters [(1, 16), (1, 18)])
            body: (braced_expression [(1, 19), (6, 3)]
              body: (binary_operator [(2, 4), (2, 14)]
                lhs: (identifier [(2, 4), (2, 5)])
                rhs: (binary_operator [(2, 9), (2, 14)]
                  lhs: (float [(2, 9), (2, 10)])
                  rhs: (float [(2, 13), (2, 14)])
                )
              )
              body: (if_statement [(3, 4), (5, 5)]
                condition: (binary_operator [(3, 8), (3, 13)]
                  lhs: (identifier [(3, 8), (3, 9)])
                  rhs: (float [(3, 12), (3, 13)])
                )
                consequence: (braced_expression [(3, 15), (5, 5)]
                  body: (true [(4, 6), (4, 10)])
                )
              )
            )
          )
        )
        (binary_operator [(8, 2), (8, 7)]
          lhs: (float [(8, 2), (8, 3)])
          rhs: (float [(8, 6), (8, 7)])
        )
      )

---

    Code
      node_show_s_expression(node, show_locations = FALSE)
    Output
      (program
        (binary_operator
          lhs: (identifier)
          operator: "<-"
          rhs: (function_definition
            name: "function"
            parameters: (parameters
              open: "("
              close: ")"
            )
            body: (braced_expression
              open: "{"
              body: (binary_operator
                lhs: (identifier)
                operator: "<-"
                rhs: (binary_operator
                  lhs: (float)
                  operator: "+"
                  rhs: (float)
                )
              )
              body: (if_statement
                "if"
                open: "("
                condition: (binary_operator
                  lhs: (identifier)
                  operator: ">"
                  rhs: (float)
                )
                close: ")"
                consequence: (braced_expression
                  open: "{"
                  body: (true)
                  close: "}"
                )
              )
              close: "}"
            )
          )
        )
        (binary_operator
          lhs: (float)
          operator: "+"
          rhs: (float)
        )
      )

---

    Code
      node_show_s_expression(node, show_anonymous = FALSE, show_locations = FALSE)
    Output
      (program
        (binary_operator
          lhs: (identifier)
          rhs: (function_definition
            parameters: (parameters)
            body: (braced_expression
              body: (binary_operator
                lhs: (identifier)
                rhs: (binary_operator
                  lhs: (float)
                  rhs: (float)
                )
              )
              body: (if_statement
                condition: (binary_operator
                  lhs: (identifier)
                  rhs: (float)
                )
                consequence: (braced_expression
                  body: (true)
                )
              )
            )
          )
        )
        (binary_operator
          lhs: (float)
          rhs: (float)
        )
      )

---

    Code
      node_show_s_expression(node, max_lines = 1)
    Output
      (program [(1, 0), (9, 2)]
      <truncated>

---

    Code
      node_show_s_expression(node, max_lines = 10)
    Output
      (program [(1, 0), (9, 2)]
        (binary_operator [(1, 2), (6, 3)]
          lhs: (identifier [(1, 2), (1, 4)])
          operator: "<-" [(1, 5), (1, 7)]
          rhs: (function_definition [(1, 8), (6, 3)]
            name: "function" [(1, 8), (1, 16)]
            parameters: (parameters [(1, 16), (1, 18)]
              open: "(" [(1, 16), (1, 17)]
              close: ")" [(1, 17), (1, 18)]
            )
      <truncated>

---

    Code
      node_show_s_expression(node, max_lines = 10, dangling_parenthesis = FALSE)
    Output
      (program [(1, 0), (9, 2)]
        (binary_operator [(1, 2), (6, 3)]
          lhs: (identifier [(1, 2), (1, 4)])
          operator: "<-" [(1, 5), (1, 7)]
          rhs: (function_definition [(1, 8), (6, 3)]
            name: "function" [(1, 8), (1, 16)]
            parameters: (parameters [(1, 16), (1, 18)]
              open: "(" [(1, 16), (1, 17)]
              close: ")" [(1, 17), (1, 18)])
            body: (braced_expression [(1, 19), (6, 3)]))
      <truncated>

---

    Code
      node_show_s_expression(node, max_lines = 10, dangling_parenthesis = FALSE,
        show_anonymous = FALSE)
    Output
      (program [(1, 0), (9, 2)]
        (binary_operator [(1, 2), (6, 3)]
          lhs: (identifier [(1, 2), (1, 4)])
          rhs: (function_definition [(1, 8), (6, 3)]
            parameters: (parameters [(1, 16), (1, 18)])
            body: (braced_expression [(1, 19), (6, 3)]
              body: (binary_operator [(2, 4), (2, 14)]
                lhs: (identifier [(2, 4), (2, 5)])
                rhs: (binary_operator [(2, 9), (2, 14)]
                  lhs: (float [(2, 9), (2, 10)]))))
      <truncated>

# truncation doesn't show if you are exactly at `max_lines`

    Code
      node_show_s_expression(node, max_lines = 2, dangling_parenthesis = FALSE)
    Output
      (program [(0, 0), (0, 1)]
        (float [(0, 0), (0, 1)]))

# Named `MISSING` nodes are shown

    Code
      node_show_s_expression(node)
    Output
      (program [(0, 0), (2, 3)]
        (binary_operator [(0, 0), (2, 0)]
          lhs: (float [(0, 0), (0, 1)])
          operator: "+" [(0, 2), (0, 3)]
          rhs: (identifier MISSING [(2, 0), (2, 0)])
        )
      )

# Anonymous `MISSING` nodes are shown

    Code
      node_show_s_expression(node)
    Output
      (program [(0, 0), (0, 13)]
        (for_statement [(0, 0), (0, 13)]
          "for" [(0, 0), (0, 3)]
          open: "(" [(0, 4), (0, 5)]
          variable: (identifier [(0, 5), (0, 6)])
          "in" [(0, 7), (0, 9)]
          sequence: (identifier [(0, 10), (0, 13)])
          close: ")" MISSING [(0, 13), (0, 13)]
        )
      )

