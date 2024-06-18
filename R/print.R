#' Pretty print a `node`'s s-expression
#'
#' `node_show_s_expression()` prints a nicely formatted s-expression to the
#' console. It powers the print methods of nodes and trees.
#'
#' @inheritParams x_tree_sitter_node
#' @inheritParams rlang::args_dots_empty
#'
#' @param max_lines `[double(1) / NULL]`
#'
#'   An optional maximum number of lines to print. If the maximum is hit, then
#'   `<truncated>` will be printed at the end.
#'
#' @param show_anonymous `[bool]`
#'
#'   Should anonymous nodes be shown? If `FALSE`, only named nodes are shown.
#'
#' @param show_locations `[bool]`
#'
#'   Should node locations be shown?
#'
#' @param show_parentheses `[bool]`
#'
#'   Should parentheses around each node be shown?
#'
#' @param dangling_parenthesis `[bool]`
#'
#'   Should the `)` parenthesis "dangle" on its own line? If `FALSE`, it is
#'   appended to the line containing the last child. This can be useful for
#'   conserving space.
#'
#' @param color_parentheses `[bool]`
#'
#'   Should parentheses be colored? Printing large s-expressions is faster if
#'   this is set to `FALSE`.
#'
#' @param color_locations `[bool]`
#'
#'   Should locations be colored? Printing large s-expressions is faster if
#'   this is set to `FALSE`.
#'
#' @returns
#' `x` invisibly.
#'
#' @export
#' @examplesIf rlang::is_installed("treesitter.r")
#' language <- treesitter.r::language()
#' parser <- parser(language)
#'
#' text <- "fn <- function(a, b = 2) { a + b + 2 }"
#' tree <- parser_parse(parser, text)
#' node <- tree_root_node(tree)
#'
#' node_show_s_expression(node)
#'
#' node_show_s_expression(node, max_lines = 5)
#'
#' # This is more like a typical abstract syntax tree
#' node_show_s_expression(
#'   node,
#'   show_anonymous = FALSE,
#'   show_locations = FALSE,
#'   dangling_parenthesis = FALSE
#' )
node_show_s_expression <- function(
  x,
  ...,
  max_lines = NULL,
  show_anonymous = TRUE,
  show_locations = TRUE,
  show_parentheses = TRUE,
  dangling_parenthesis = TRUE,
  color_parentheses = TRUE,
  color_locations = TRUE
) {
  check_dots_empty0(...)

  info <- node_format_s_expression(
    x = x,
    max_lines = max_lines,
    show_anonymous = show_anonymous,
    show_locations = show_locations,
    show_parentheses = show_parentheses,
    dangling_parenthesis = dangling_parenthesis,
    color_parentheses = color_parentheses,
    color_locations = color_locations
  )

  text <- info$text
  truncated <- info$truncated

  cat_line(text)

  if (truncated) {
    cat_line(cli::style_italic("<truncated>"))
  }

  invisible(x)
}

node_format_s_expression <- function(
  x,
  ...,
  max_lines = NULL,
  show_anonymous = TRUE,
  show_locations = TRUE,
  show_parentheses = TRUE,
  dangling_parenthesis = TRUE,
  color_parentheses = TRUE,
  color_locations = TRUE
) {
  check_dots_empty0(...)

  check_node(x)
  check_number_whole(max_lines, min = 1, allow_null = TRUE)
  check_bool(show_anonymous)
  check_bool(show_locations)
  check_bool(show_parentheses)
  check_bool(dangling_parenthesis)
  check_bool(color_parentheses)
  check_bool(color_locations)

  options <- list(
    tabs = 0L,
    truncated = FALSE,
    n_lines = 1L,
    max_lines = max_lines,
    show_anonymous = show_anonymous,
    show_locations = show_locations,
    show_parentheses = show_parentheses,
    dangling_parenthesis = dangling_parenthesis,
    color_parentheses = color_parentheses,
    color_locations = color_locations
  )

  # Rough count of expected size
  # 1 for node itself, 1 for `(`, 1 for `)`,
  # 2 more for good measure based on some rough testing
  capacity <- node_descendant_count(x) * 5L
  capacity <- pmax(capacity, 1L)

  tokens <- new_dyn_chr(capacity)
  options <- node_format_s_expression_recurse(x, tokens, options)
  tokens <- dyn_unwrap(tokens)
  text <- paste0(tokens, collapse = "")

  list(
    text = text,
    truncated = options$truncated
  )
}

node_format_s_expression_recurse <- function(x, tokens, options) {
  is_named <- node_is_named(x)
  is_missing <- node_is_missing(x)

  if (options$show_parentheses && is_named) {
    dyn_chr_push_back(tokens, color("(", options))
  }

  options$tabs <- options$tabs + 1L

  type <- node_type(x)
  if (!is_named) {
    type <- encodeString(type, quote = "\"")
  }
  if (is_missing) {
    type <- paste0(type, " MISSING")
  }
  dyn_chr_push_back(tokens, type)

  if (options$show_locations) {
    location <- node_format_location(x, options)
    dyn_chr_push_back(tokens, " ")
    dyn_chr_push_back(tokens, location)
  }

  children <- node_children(x)
  n_visible_children <- 0L

  for (i in seq_along(children)) {
    child <- children[[i]]

    if (!options$show_anonymous && !node_is_named(child)) {
      next
    }

    n_visible_children <- n_visible_children + 1L

    if (lines_at_max(options)) {
      options <- lines_truncated(options)
      return(options)
    }
    dyn_chr_push_back(tokens, "\n")
    options <- lines_increment(options)

    dyn_chr_push_back(tokens, tab(options$tabs))

    field_name <- node_field_name_for_child(x, i)
    if (!is.na(field_name)) {
      field_name <- paste0(field_name, ": ")
      dyn_chr_push_back(tokens, field_name)
    }

    options <- node_format_s_expression_recurse(child, tokens, options)
  }

  options$tabs <- options$tabs - 1L

  if (options$show_parentheses && is_named) {
    if (options$dangling_parenthesis && n_visible_children != 0L) {
      # If the node had any visible children, put the closing `)`
      # on its own line aligned with the opening field name or `(`
      if (lines_at_max(options)) {
        options <- lines_truncated(options)
        return(options)
      }
      dyn_chr_push_back(tokens, "\n")
      options <- lines_increment(options)

      dyn_chr_push_back(tokens, tab(options$tabs))
    }

    dyn_chr_push_back(tokens, color(")", options))
  }

  options
}

node_format_location <- function(x, options) {
  start_point <- node_start_point(x)
  start_row <- point_row(start_point)
  start_column <- point_column(start_point)

  end_point <- node_end_point(x)
  end_row <- point_row(end_point)
  end_column <- point_column(end_point)

  # `format_inline()` is a bit slow for repeated usage here
  location <- sprintf(
    "[(%s, %s), (%s, %s)]",
    start_row,
    start_column,
    end_row,
    end_column
  )

  if (options$color_locations) {
    location <- cli::col_grey(location)
  }

  location
}

lines_at_max <- function(options) {
  max_lines <- options$max_lines

  if (is.null(max_lines)) {
    return(FALSE)
  }

  n_lines <- options$n_lines

  n_lines == max_lines
}

lines_increment <- function(options) {
  options$n_lines <- options$n_lines + 1L
  options
}

lines_truncated <- function(options) {
  options$truncated <- TRUE
  options
}

tab <- function(n) {
  strrep("  ", n)
}

color <- function(text, options) {
  if (!options$color_parentheses) {
    return(text)
  }

  fns <- list(
    cli::col_blue,
    cli::col_green,
    cli::col_red,
    cli::col_cyan,
    cli::col_yellow,
    cli::col_magenta
  )

  # Rotate between these 6 colors based on the level
  # of `tabs` nesting we are at
  index <- options$tabs %% length(fns)
  index <- index + 1L

  fn <- fns[[index]]

  fn(text)
}
