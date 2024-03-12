.onLoad <- function(libname, pkgname) {
  ns <- asNamespace(pkgname)
  initialize(ns)
}

initialize <- function(ns) {
  .Call(ffi_initialize, ns)
}
