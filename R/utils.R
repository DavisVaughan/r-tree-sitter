cat_line <- function(...) {
  out <- paste0(..., collapse = "\n")
  cat(out, "\n", sep = "", file = stdout(), append = TRUE)
}
