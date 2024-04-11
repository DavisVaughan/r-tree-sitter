r <- function() {
  if (!is_installed("treesitter.r")) {
    skip("treesitter.r is not installed.")
  }
  treesitter.r::language()
}
