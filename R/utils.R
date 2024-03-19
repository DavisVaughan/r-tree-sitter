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
