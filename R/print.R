#' @export
node_print_s_expression <- function(
  x,
  ...,
  anonymous = TRUE,
  compact = TRUE,
  locations = TRUE,
  color_parentheses = TRUE,
  color_locations = TRUE
) {
  check_dots_empty0(...)

  text <- node_format_s_expression(
    x = x,
    anonymous = anonymous,
    compact = compact,
    locations = locations,
    color_parentheses = color_parentheses,
    color_locations = color_locations
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
  color_locations = TRUE
) {
  check_dots_empty0(...)

  check_node(x)
  check_bool(anonymous)
  check_bool(compact)
  check_bool(locations)
  check_bool(color_parentheses)
  check_bool(color_locations)

  options <- list(
    tabs = 0L,
    anonymous = anonymous,
    compact = compact,
    locations = locations,
    color_parentheses = color_parentheses,
    color_locations = color_locations
  )

  # Rough count of expected size
  # 1 for node itself, 1 for `(`, 1 for `)`,
  # 2 more for good measure based on some rough testing
  capacity <- node_descendant_count(x) * 5L
  capacity <- pmax(capacity, 1L)

  tokens <- new_dyn_chr(capacity)
  tokens <- node_format_s_expression_recurse(x, tokens, options)
  tokens <- dyn_unwrap(tokens)
  tokens <- paste0(tokens, collapse = "")

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
  size <- length(children)

  for (i in seq_len(size)) {
    child <- children[[i]]

    if (!options$anonymous && !node_is_named(child)) {
      next
    }

    dyn_chr_push_back(tokens, "\n")
    dyn_chr_push_back(tokens, tab(options$tabs))

    field_name <- node_field_name_for_child(x, i)
    if (!is.na(field_name)) {
      field_name <- paste0(field_name, ": ")
      dyn_chr_push_back(tokens, field_name)
    }

    tokens <- node_format_s_expression_recurse(child, tokens, options)
  }

  options$tabs <- options$tabs - 1L

  if (!options$compact && size != 0L) {
    # If the node had any children, put the closing `)`
    # on its own line aligned with the opening field name or `(`
    dyn_chr_push_back(tokens, "\n")
    dyn_chr_push_back(tokens, tab(options$tabs))
  }

  dyn_chr_push_back(tokens, color(")", options))

  tokens
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

  tokens
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
