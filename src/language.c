#include "language.h"

#include "external-pointer.h"
#include "utils.h"

r_obj* ffi_language_version(r_obj* ffi_x) {
  const TSLanguage* x = ts_language_from_external_pointer(ffi_x);

  uint32_t version = ts_language_version(x);

  r_obj* out = KEEP(r_alloc_double(1));
  r_dbl_poke(out, 0, (double) version);

  FREE(1);
  return out;
}

r_obj* ffi_language_symbol_for_name(r_obj* ffi_x, r_obj* name, r_obj* named) {
  const TSLanguage* x = ts_language_from_external_pointer(ffi_x);

  const r_ssize size = r_length(name);

  if (r_typeof(name) != R_TYPE_character) {
    r_abort(
        "`name` must be a character vector, not %s.", r_obj_type_friendly(name)
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
    r_stop_internal("`name` and `named` must be the same length.");
  }

  r_obj* const* v_name = r_chr_cbegin(name);
  const int* v_named = r_lgl_cbegin(named);

  r_obj* out = KEEP(r_alloc_integer(size));
  int* v_out = r_int_begin(out);

  for (r_ssize i = 0; i < size; ++i) {
    r_obj* elt = v_name[i];
    const char* elt_c = r_str_c_string(elt);
    const r_ssize elt_length = r_length(elt);

    const bool elt_named = (bool) v_named[i];

    TSSymbol symbol =
        ts_language_symbol_for_name(x, elt_c, elt_length, elt_named);

    if (symbol == (TSSymbol) 0) {
      v_out[i] = r_globals.na_int;
    } else {
      v_out[i] = (int) symbol;
    }
  }

  FREE(1);
  return out;
}

r_obj* ffi_language_symbol_name(r_obj* ffi_x, r_obj* symbol) {
  const TSLanguage* x = ts_language_from_external_pointer(ffi_x);

  if (r_typeof(symbol) != R_TYPE_integer) {
    r_stop_internal("`symbol` must be an integer vector.");
  }
  if (r_int_any_missing(symbol)) {
    r_abort("`symbol` can't contain missing values.");
  }

  const r_ssize size = r_length(symbol);
  const int* v_symbol = r_int_cbegin(symbol);

  r_obj* out = KEEP(r_alloc_character(size));

  for (r_ssize i = 0; i < size; ++i) {
    const TSSymbol elt = r_int_as_TSSymbol(v_symbol[i], "symbol");
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

r_obj* ffi_language_field_id_for_name(r_obj* ffi_x, r_obj* ffi_name) {
  const TSLanguage* x = ts_language_from_external_pointer(ffi_x);

  const r_ssize size = r_length(ffi_name);
  r_obj* const* v_name = r_chr_cbegin(ffi_name);

  r_obj* out = KEEP(r_alloc_integer(size));
  int* v_out = r_int_begin(out);

  for (r_ssize i = 0; i < size; ++i) {
    r_obj* elt = v_name[i];
    const char* elt_c = r_str_c_string(elt);
    const uint32_t elt_size = r_ssize_as_uint32(r_length(elt));
    const TSFieldId id = ts_language_field_id_for_name(x, elt_c, elt_size);
    v_out[i] = (id == 0) ? r_globals.na_int : r_TSFieldId_as_int(id);
  }

  FREE(1);
  return out;
}

r_obj* ffi_language_field_name_for_id(r_obj* ffi_x, r_obj* ffi_id) {
  const TSLanguage* x = ts_language_from_external_pointer(ffi_x);

  const r_ssize size = r_length(ffi_id);
  const int* v_id = r_int_cbegin(ffi_id);

  r_obj* out = KEEP(r_alloc_character(size));

  for (r_ssize i = 0; i < size; ++i) {
    TSFieldId elt = r_int_as_TSFieldId(v_id[i], "id");
    const char* name = ts_language_field_name_for_id(x, elt);
    r_chr_poke(out, i, (name == NULL) ? r_globals.na_str : r_str(name));
  }

  FREE(1);
  return out;
}

r_obj* ffi_language_symbol_count(r_obj* ffi_x) {
  const TSLanguage* x = ts_language_from_external_pointer(ffi_x);
  const uint32_t out = ts_language_symbol_count(x);
  return r_dbl(r_uint32_as_dbl(out));
}

r_obj* ffi_language_state_count(r_obj* ffi_x) {
  const TSLanguage* x = ts_language_from_external_pointer(ffi_x);
  const uint32_t out = ts_language_state_count(x);
  return r_dbl(r_uint32_as_dbl(out));
}

r_obj* ffi_language_field_count(r_obj* ffi_x) {
  const TSLanguage* x = ts_language_from_external_pointer(ffi_x);
  const uint32_t out = ts_language_field_count(x);
  return r_dbl(r_uint32_as_dbl(out));
}

r_obj*
ffi_language_next_state(r_obj* ffi_x, r_obj* ffi_state, r_obj* ffi_symbol) {
  const TSLanguage* x = ts_language_from_external_pointer(ffi_x);

  const int* v_state = r_int_cbegin(ffi_state);
  const int* v_symbol = r_int_cbegin(ffi_symbol);

  const r_ssize size = r_length(ffi_state);

  r_obj* out = KEEP(r_alloc_integer(size));
  int* v_out = r_int_begin(out);

  for (r_ssize i = 0; i < size; ++i) {
    const TSStateId state = r_int_as_TSStateId(v_state[i], "state");
    const TSSymbol symbol = r_int_as_TSSymbol(v_symbol[i], "symbol");
    const TSStateId next = ts_language_next_state(x, state, symbol);
    v_out[i] = r_TSStateId_as_int(next);
  }

  FREE(1);
  return out;
}

const TSLanguage* ts_language_from_external_pointer(r_obj* x) {
  TS_OBJECT_FROM_EXTERNAL_POINTER(x, const TSLanguage*);
}
