#include "dyn.h"

r_obj* ffi_new_dyn_chr(r_obj* ffi_capacity) {
  const r_ssize capacity = r_arg_as_ssize(ffi_capacity, "capacity");
  struct r_dyn_array* p_x = r_new_dyn_vector(R_TYPE_character, capacity);
  return p_x->shelter;
}

r_obj* ffi_dyn_chr_push_back(r_obj* ffi_x, r_obj* ffi_value) {
  struct r_dyn_array* p_x = r_shelter_deref(ffi_x);
  r_obj* value = r_chr_get(ffi_value, 0);
  r_dyn_chr_push_back(p_x, value);
  return r_null;
}

r_obj* ffi_dyn_unwrap(r_obj* ffi_x) {
  struct r_dyn_array* p_x = r_shelter_deref(ffi_x);
  return r_dyn_unwrap(p_x);
}
