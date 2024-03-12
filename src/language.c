#include "language.h"

#include "utils.h"

r_obj* ffi_language_version(r_obj* ffi_x) {
  const TSLanguage* x = ts_language_from_external_pointer(ffi_x);

  uint32_t version = ts_language_version(x);

  r_obj* out = KEEP(r_alloc_double(1));
  r_dbl_poke(out, 0, (double) version);

  FREE(1);
  return out;
}

r_obj* ffi_language_id_for_node_kind(r_obj* ffi_x, r_obj* kind, r_obj* named) {
  const TSLanguage* x = ts_language_from_external_pointer(ffi_x);

  const r_ssize size = r_length(kind);

  if (r_typeof(kind) != R_TYPE_character) {
    r_abort(
        "`kind` must be a character vector, not %s.", r_obj_type_friendly(kind)
    );
  }
  if (r_typeof(named) != R_TYPE_logical) {
    r_abort(
        "`named` must be a logical vector, not %s", r_obj_type_friendly(named)
    );
  }
  if (r_lgl_any_missing(named)) {
    r_abort("`named` can't contain missing values.");
  }
  if (r_length(named) != size) {
    r_stop_internal("`kind` and `named` must be the same length.");
  }

  r_obj* const* v_kind = r_chr_cbegin(kind);
  const int* v_named = r_lgl_cbegin(named);

  r_obj* out = KEEP(r_alloc_integer(size));
  int* v_out = r_int_begin(out);

  for (r_ssize i = 0; i < size; ++i) {
    r_obj* kind = v_kind[i];
    const char* c_kind = r_str_c_string(kind);
    const r_ssize kind_length = r_length(kind);

    const bool named = (bool) v_named[i];

    TSSymbol id = ts_language_symbol_for_name(x, c_kind, kind_length, named);

    if (id == (TSSymbol) 0) {
      v_out[i] = r_globals.na_int;
    } else {
      v_out[i] = (int) id;
    }
  }

  FREE(1);
  return out;
}

r_obj* ffi_language_node_kind_for_id(r_obj* ffi_x, r_obj* id) {
  const TSLanguage* x = ts_language_from_external_pointer(ffi_x);

  if (r_typeof(id) != R_TYPE_integer) {
    r_stop_internal("`id` must be an integer vector.");
  }
  if (r_int_any_missing(id)) {
    r_abort("`id` can't contain missing values.");
  }

  const r_ssize size = r_length(id);
  const int* v_id = r_int_cbegin(id);

  r_obj* out = KEEP(r_alloc_character(size));

  for (r_ssize i = 0; i < size; ++i) {
    const TSSymbol elt = r_int_as_TSSymbol(v_id[i], "id");
    const char* name = ts_language_symbol_name(x, elt);

    if (name == NULL) {
      r_chr_poke(out, i, r_globals.na_str);
    } else {
      r_chr_poke(out, i, r_str(name));
    }
  }

  FREE(1);
  return out;
}

const TSLanguage* ts_language_from_external_pointer(r_obj* x) {
  if (r_typeof(x) != R_TYPE_pointer) {
    r_abort("`x` must be an external pointer.");
  }

  const TSLanguage* out = (const TSLanguage*) R_ExternalPtrAddr(x);

  if (out == NULL) {
    r_abort("`x` must point to a `TSLanguage`.");
  }

  return out;
}
