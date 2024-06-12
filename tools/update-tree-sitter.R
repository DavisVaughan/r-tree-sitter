# To update, supply either a tag to a stable release, or a specific sha.
# One of these two must be `NULL`, but not both.
tag <- "v0.22.6"
sha <- NULL

# Returns a `path` to the top level of the downloaded tree-sitter sources
download_tree_sitter_source <- function(tag, sha) {
  # Build URL to requested tag or sha
  if (!is.null(tag)) {
    url <- url <- sprintf(
      "https://github.com/tree-sitter/tree-sitter/archive/refs/tags/%s.zip",
      tag
    )
    dir_base <- sprintf("tree-sitter-%s", tag)
  } else if (!is.null(sha)) {
    url <- sprintf(
      "https://github.com/tree-sitter/tree-sitter/archive/%s.zip",
      sha
    )
    dir_base <- sprintf("tree-sitter-%s", sha)
  } else {
    stop("Either `tag` or `sha` must be specified.")
  }

  dir <- tempdir()
  dir <- file.path(dir, "tree-sitter")
  dir <- file.path(dir, dir_base)

  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }

  zip <- file.path(dir, "tree-sitter.zip")
  download.file(url, destfile = zip)
  on.exit(unlink(zip), add = TRUE, after = FALSE)

  unzip(zip, exdir = dir)
  path <- dir(dir, pattern = "tree-sitter-", full.names = TRUE)

  if (length(path) != 1L) {
    stop("Should be exactly 1 file matched by `dir()`.")
  }

  path
}

# Copies new files in, but this removes our manual tweaks so we have to add
# them back in manually
copy_tree_sitter_files <- function(from, to) {
  from_src <- file.path(from, "lib", "src")
  to_src <- file.path(to, "src", "tree-sitter", "lib")

  from_include <- file.path(from, "lib", "include")
  to_include <- file.path(to, "src", "tree-sitter", "lib")

  file.copy(from = from_src, to = to_src, overwrite = TRUE, recursive = TRUE)
  file.copy(from = from_include, to = to_include, overwrite = TRUE, recursive = TRUE)
}

run <- function(tag, sha) {
  usethis::ui_info("Downloading tree-sitter sources")
  from <- download_tree_sitter_source(tag, sha)
  usethis::ui_done("Downloading tree-sitter sources - done")

  to <- getwd()

  usethis::ui_info("Copying in new tree-sitter sources")
  copy_tree_sitter_files(from, to)
  usethis::ui_done("Copying in new tree-sitter sources - done")

  usethis::ui_todo("Update `Makevars` with new `OBJECTS`.")

  # In particular:
  # - Comment out usage of `stderr` and `abort()` in `alloc.c`, we provide our own
  #   allocators anyways.
  # - Comment out usage of `stderr` in `stack.c` in `ts_stack_print_dot_graph()`.
  #   We never expose a way to print the dot graph.
  # - Replace all `#pragma` calls with `# pragma` in `array.h`
  usethis::ui_todo("Restore usage of `--- r-tree-sitter begin ---`")
}

run(tag, sha)
