#' @export
node_print_s_expression <- function(
  x,
  ...,
  anonymous = TRUE,
  compact = TRUE,
  locations = TRUE,
  color_parentheses = TRUE,
  color_locations = TRUE,
  max_lines = NULL
) {
  check_dots_empty0(...)

  text <- node_format_s_expression(
    x = x,
    anonymous = anonymous,
    compact = compact,
    locations = locations,
    color_parentheses = color_parentheses,
    color_locations = color_locations,
    max_lines = max_lines
  )

  cat_line(text)

  invisible(x)
}

node_format_s_expression <- function(
  x,
  ...,
  anonymous = TRUE,
  compact = TRUE,
  locations = TRUE,
  color_parentheses = TRUE,
  color_locations = TRUE,
  max_lines = NULL
) {
  check_dots_empty0(...)

  check_node(x)
  check_bool(anonymous)
  check_bool(compact)
  check_bool(locations)
  check_bool(color_parentheses)
  check_bool(color_locations)
  check_number_whole(max_lines, min = 1, allow_null = TRUE)

  options <- list(
    tabs = 0L,
    anonymous = anonymous,
    compact = compact,
    locations = locations,
    color_parentheses = color_parentheses,
    color_locations = color_locations,
    n_lines = 1L,
    max_lines = max_lines
  )

  # Rough count of expected size
  # 1 for node itself, 1 for `(`, 1 for `)`,
  # 2 more for good measure based on some rough testing
  capacity <- node_descendant_count(x) * 5L
  capacity <- pmax(capacity, 1L)

  tokens <- new_dyn_chr(capacity)
  options <- node_format_s_expression_recurse(x, tokens, options)
  tokens <- dyn_unwrap(tokens)
  tokens <- paste0(tokens, collapse = "")

  if (lines_at_max(options)) {
    footer <- cli::format_inline("<truncated>")
    footer <- cli::style_italic(footer)
    tokens <- paste0(tokens, "\n", footer)
  }

  tokens
}

node_format_s_expression_recurse <- function(x, tokens, options) {
  if (node_is_named(x)) {
    node_format_s_expression_named(x, tokens, options)
  } else {
    node_format_s_expression_anonymous(x, tokens, options)
  }
}

node_format_s_expression_named <- function(x, tokens, options) {
  dyn_chr_push_back(tokens, color("(", options))
  options$tabs <- options$tabs + 1L

  type <- node_type(x)
  dyn_chr_push_back(tokens, type)

  if (options$locations) {
    location <- node_format_location(x, options)
    dyn_chr_push_back(tokens, " ")
    dyn_chr_push_back(tokens, location)
  }

  children <- node_children(x)
  n_visible_children <- 0L

  for (i in seq_along(children)) {
    child <- children[[i]]

    if (!options$anonymous && !node_is_named(child)) {
      next
    }

    n_visible_children <- n_visible_children + 1L

    if (lines_at_max(options)) {
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

  if (!options$compact && n_visible_children != 0L) {
    # If the node had any visible children, put the closing `)`
    # on its own line aligned with the opening field name or `(`
    if (lines_at_max(options)) {
      return(options)
    }
    dyn_chr_push_back(tokens, "\n")
    options <- lines_increment(options)

    dyn_chr_push_back(tokens, tab(options$tabs))
  }

  dyn_chr_push_back(tokens, color(")", options))

  options
}

node_format_s_expression_anonymous <- function(x, tokens, options) {
  type <- node_type(x)
  type <- encodeString(type, quote = "\"")
  dyn_chr_push_back(tokens, type)

  if (options$locations) {
    location <- node_format_location(x, options)
    dyn_chr_push_back(tokens, " ")
    dyn_chr_push_back(tokens, location)
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
