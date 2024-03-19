check_positive <- function(
  x,
  ...,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (is_true(x > 0L)) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    "a positive value",
    ...,
    arg = arg,
    call = call
  )
}

cat_line <- function(...) {
  out <- paste0(..., collapse = "\n")
  cat(out, "\n", sep = "", file = stdout(), append = TRUE)
}
