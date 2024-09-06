#include "query.h"

#include "decl/query-decl.h"
#include "external-pointer.h"
#include "language.h"
#include "node.h"
#include "utils.h"

r_obj* ffi_query_new(r_obj* ffi_source, r_obj* ffi_language) {
  const TSLanguage* language = ts_language_from_external_pointer(ffi_language);

  r_obj* source = r_chr_get(ffi_source, 0);
  const char* source_c = r_str_c_string(source);
  uint32_t source_size = r_ssize_as_uint32(r_length(source));

  uint32_t error_offset = 0;
  TSQueryError error_type = TSQueryErrorNone;

  TSQuery* query =
      ts_query_new(language, source_c, source_size, &error_offset, &error_type);

  if (query == NULL) {
    return query_error(error_offset, error_type);
  } else {
    return ts_query_as_external_pointer(query);
  }
}

r_obj* ffi_query_pattern_count(r_obj* ffi_query) {
  const TSQuery* query = ts_query_from_external_pointer(ffi_query);
  uint32_t out = ts_query_pattern_count(query);
  return r_dbl(r_uint32_as_dbl(out));
}

r_obj* ffi_query_capture_count(r_obj* ffi_query) {
  const TSQuery* query = ts_query_from_external_pointer(ffi_query);
  uint32_t out = ts_query_capture_count(query);
  return r_dbl(r_uint32_as_dbl(out));
}

r_obj* ffi_query_string_count(r_obj* ffi_query) {
  const TSQuery* query = ts_query_from_external_pointer(ffi_query);
  uint32_t out = ts_query_string_count(query);
  return r_dbl(r_uint32_as_dbl(out));
}

r_obj* ffi_query_capture_names(r_obj* ffi_query) {
  const TSQuery* query = ts_query_from_external_pointer(ffi_query);

  uint32_t size = ts_query_capture_count(query);

  r_obj* out = KEEP(r_alloc_character(r_uint32_as_r_ssize(size)));

  for (uint32_t i = 0; i < size; ++i) {
    uint32_t length = 0;
    const char* name = ts_query_capture_name_for_id(query, i, &length);
    r_chr_poke(
        out,
        r_uint32_as_r_ssize(i),
        Rf_mkCharLenCE(name, r_uint32_as_int(length, "length"), CE_UTF8)
    );
  }

  FREE(1);
  return out;
}

r_obj* ffi_query_start_byte_for_pattern(r_obj* ffi_query, r_obj* ffi_i) {
  return query_byte_for_pattern(ffi_query, ffi_i, true);
}

r_obj* ffi_query_end_byte_for_pattern(r_obj* ffi_query, r_obj* ffi_i) {
  return query_byte_for_pattern(ffi_query, ffi_i, false);
}

static r_obj*
query_byte_for_pattern(r_obj* ffi_query, r_obj* ffi_i, bool start) {
  const TSQuery* query = ts_query_from_external_pointer(ffi_query);

  // Validated on R side to be positive whole double of length 1, 1-indexed
  uint32_t i = r_dbl_as_uint32(r_dbl_get(ffi_i, 0) - 1, "i");

  // There is no safety about OOB `i` values, so we add our own
  if (i >= ts_query_pattern_count(query)) {
    return r_dbl(r_globals.na_dbl);
  } else {
    uint32_t out = start ? ts_query_start_byte_for_pattern(query, i)
                         : ts_query_end_byte_for_pattern(query, i);
    return r_dbl(r_uint32_as_dbl(out));
  }
}

static r_obj* query_error(uint32_t error_offset, TSQueryError error_type) {
  const char* error_type_name = NULL;

  switch (error_type) {
    case TSQueryErrorNone:
      r_stop_internal("Unexpected `None` case for `TSQueryError`.");
    case TSQueryErrorSyntax: {
      error_type_name = "Syntax";
      break;
    }
    case TSQueryErrorNodeType: {
      error_type_name = "Node type";
      break;
    }
    case TSQueryErrorField: {
      error_type_name = "Field";
      break;
    }
    case TSQueryErrorCapture: {
      error_type_name = "Capture";
      break;
    }
    case TSQueryErrorStructure: {
      error_type_name = "Structure";
      break;
    }
    case TSQueryErrorLanguage: {
      error_type_name = "Language";
      break;
    }
  }

  r_obj* out = KEEP(r_alloc_list(2));

  r_obj* names = r_alloc_character(2);
  r_attrib_poke_names(out, names);
  r_chr_poke(names, 0, r_str("offset"));
  r_chr_poke(names, 1, r_str("type"));

  r_list_poke(out, 0, r_dbl(error_offset + 1));
  r_list_poke(out, 1, r_chr(error_type_name));

  FREE(1);
  return out;
}

r_obj* ts_query_as_external_pointer(TSQuery* x) {
  return new_external_pointer((void*) x, query_finalize);
}

TSQuery* ts_query_from_external_pointer(r_obj* x) {
  TS_OBJECT_FROM_EXTERNAL_POINTER(x, TSQuery*);
}

static void query_finalize(r_obj* x) {
  if (r_typeof(x) != R_TYPE_pointer) {
    return;
  }

  TSQuery* query = (TSQuery*) R_ExternalPtrAddr(x);

  if (query == NULL) {
    return;
  }

  ts_query_delete(query);

  R_ClearExternalPtr(x);
}
