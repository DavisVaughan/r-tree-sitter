grammars <- list(
  r = list(
    name = "r",
    name_pretty = "R",
    url = "https://github.com/r-lib/tree-sitter-r",
    sha = "03e6c381ba3d3d4802865e0108c56c86a6ef3f85"
  )
)

grammar_init <- function(name) {
  check_string(name)

  grammar <- grammars[[name]]

  if (is.null(grammar)) {
    cli::cli_abort("Can't find grammar named {.str name}.")
  }

  name <- grammar$name
  name_pretty <- grammar$name_pretty
  url <- grammar$url
  sha <- grammar$sha

  root <- clone_grammar(name, url, sha)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  write_c_binding(name)

  generate(root)
  copy_tree_sitter_files(name, root)

  write_r_language(name, name_pretty, url, sha)
}

# https://docs.github.com/en/repositories/working-with-files/using-files/downloading-source-code-archives#source-code-archive-urls
clone_grammar <- function(name, url, sha) {
  zip_url <- cli::format_inline("{url}/archive/{sha}.zip")

  dest <- file.path(tempdir(), "tree-sitter", name)

  if (!dir.exists(dest)) {
    dir.create(dest)
  }

  zip <- tempfile(name, tmpdir = dest, fileext = ".zip")
  unzip <- file.path(dest, "unzip")

  utils::download.file(zip_url, zip)
  unzip(zip, exdir = unzip)

  subdir <- paste0(basename(url), "-", sha)
  root <- file.path(unzip, subdir)

  root 
}

# We regenerate `parser.c` with our local version of tree-sitter,
# to guarantee that all generated languages have the same ABI as
# the bindings we ship.
# TODO: Tighten this up even further to be absolutely sure the cli version
# of tree-sitter matches the one we ship.
generate <- function(root) {
  check_installed("withr")
  withr::local_dir(root)
  system2("tree-sitter", "generate")
}

copy_tree_sitter_files <- function(name, root) {
  parser <- file.path(root, "src", "parser.c")
  header <- file.path(root, "src", "tree_sitter", "parser.h")
  scanner <- file.path(root, "src", "scanner.c")

  if (!file.exists(parser)) {
    abort("Can't find `parser.c` after running `tree-sitter generate`.")
  }
  if (!file.exists(header)) {
    abort("Can't find `parser.h` after running `tree-sitter generate`.")
  }

  dest <- file.path(path_root(), "src", "grammars", name, "parser.c")
  file.copy(parser, dest, overwrite = TRUE)
  patch_parser(dest)

  dest <- file.path(path_root(), "src", "grammars", name, "tree_sitter", "parser.h")
  if (!dir.exists(dirname(dest))) {
    dir.create(dirname(dest))
  }
  file.copy(header, dest, overwrite = TRUE)

  bullets <- c()

  bullet <- cli::format_inline("Add `grammars/{name}/parser.o` to `Makevars`.")
  bullets <- c(bullets, "*" = bullet)

  if (file.exists(scanner)) {
    dest <- file.path(path_root(), "src", "grammars", name, "scanner.c")
    file.copy(scanner, dest, overwrite = TRUE)

    bullet <- cli::format_inline("Add `grammars/{name}/scanner.o` to `Makevars`.")
    bullets <- c(bullets, "*" = bullet)
  }

  rlang::inform(bullets)

  invisible(NULL)
}

patch_parser <- function(path) {
  lines <- readLines(path)

  # We always have to patch these pragma calls for CRAN
  find <- "#pragma"
  replacement <- "# pragma"
  lines <- gsub(find, replacement, lines)

  writeLines(lines, path)
}

write_c_binding <- function(name) {
  dest <- file.path(path_root(), "src", "grammars", name, "binding.c")
  
  if (!dir.exists(dirname(dest))) {
    dir.create(dirname(dest), recursive = TRUE)
  }

  file <- path_template("binding.c")
  lines <- readLines(file)

  replace <- "TEMPLATE_NAME_LANGUAGE"
  lines <- gsub(replace, name, lines, fixed = TRUE)

  writeLines(lines, dest)

  bullets <- c()

  bullet <- cli::format_inline("Add {.code ffi_{name}_language()} to `init.c`.")
  bullets <- c(bullets, "*" = bullet)

  bullet <- cli::format_inline("Add `grammars/{name}/binding.o` to `Makevars`.")
  bullets <- c(bullets, "*" = bullet)

  rlang::inform(bullets)

  invisible(NULL)
}

write_r_language <- function(name, name_pretty, url, sha) {
  dest <- paste0("language-", name, ".R")
  dest <- file.path(path_root(), "R", dest)

  file <- path_template("language.R")
  lines <- readLines(file)

  replace <- "TEMPLATE_NAME_LANGUAGE"
  lines <- gsub(replace, name, lines, fixed = TRUE)

  replace <- "TEMPLATE_NAME_PRETTY"
  lines <- gsub(replace, name_pretty, lines, fixed = TRUE)

  replace <- "TEMPLATE_URL"
  lines <- gsub(replace, url, lines, fixed = TRUE)

  replace <- "TEMPLATE_SHA"
  lines <- gsub(replace, sha, lines, fixed = TRUE)

  writeLines(lines, dest)

  bullets <- c()

  bullet <- cli::format_inline("Run {.code devtools::document()}.")
  bullets <- c(bullets, "*" = bullet)

  rlang::inform(bullets)

  invisible(NULL)
}

path_template <- function(name) {
  file.path(path_root(), "tools", "templates", name)
}

path_root <- function() {
  getwd()
}
