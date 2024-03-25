cat_line <- function(...) {
  out <- paste0(..., collapse = "\n")
  cat(out, "\n", sep = "", file = stdout(), append = TRUE)
}

check_no_missing <- function(
  x,
  ...,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (!vec_any_missing(x)) {
    return(invisible(NULL))
  }

  locations <- vec_detect_missing(x)
  locations <- which(locations)

  if (length(locations) > 5) {
    locations <- vec_slice(locations, 1:5)
  }

  message <- c(
    "{.arg {arg}} can't contain missing values.",
    i = "Missing values detected at locations: {locations}."
  )

  cli::cli_abort(message, arg = arg, call = call)
}
