is_language_pointer <- function(x) {
  typeof(x) == "externalptr"
}

check_language_pointer <- function(
  x,
  ...,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (is_language_pointer(x)) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    "an external pointer",
    ...,
    arg = arg,
    call = call
  )
}
