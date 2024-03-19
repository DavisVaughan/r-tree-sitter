r <- function() {
  if (!is_installed("treesitterr")) {
    skip("treesitterr is not installed.")
  }
  treesitterr::language()
}
