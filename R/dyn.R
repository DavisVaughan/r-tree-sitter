new_dyn_chr <- function(capacity) {
  .Call(ffi_new_dyn_chr, capacity)
}

dyn_chr_push_back <- function(x, value) {
  .Call(ffi_dyn_chr_push_back, x, value)
}

dyn_unwrap <- function(x) {
  .Call(ffi_dyn_unwrap, x)
}
