// Copyright (c) 2013 Austin T. Clements. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

#include <cstring>
#include <unordered_set>

#include "internal.hh"

using namespace std;

// XXX Make this more readily available?
namespace std {
template <>
struct hash<dwarf::DW_TAG> {
  using result_type = size_t;
  using argument_type = dwarf::DW_TAG;
  auto operator()(argument_type a) const -> result_type {
    return (result_type)a;
  }
};
}  // namespace std

DWARFPP_BEGIN_NAMESPACE

struct string_hash {
  using result_type = size_t;
  using argument_type = const char *;
  auto operator()(const char *s) const -> result_type {
    result_type h = 0;
    for (; *s; ++s) h += 33 * h + *s;
    return h;
  }
};

struct string_eq {
  using result_type = bool;
  using first_argument_type = const char *;
  using second_argument_type = const char *;
  auto operator()(const char *x, const char *y) const -> bool {
    return strcmp(x, y) == 0;
  }
};

struct die_str_map::impl {
  impl(const die &parent, DW_AT attr, const initializer_list<DW_TAG> &accept)
      : attr(attr),
        accept(accept.begin(), accept.end()),
        pos(parent.begin()),
        end(parent.end()) {}

  unordered_map<const char *, die, string_hash, string_eq> str_map;
  DW_AT attr;
  unordered_set<DW_TAG> accept;
  die::iterator pos, end;
  die invalid;
};

die_str_map::die_str_map(const die &parent, DW_AT attr,
                         const initializer_list<DW_TAG> &accept)
    : m(make_shared<impl>(parent, attr, accept)) {}

auto die_str_map::from_type_names(const die &parent) -> die_str_map {
  return die_str_map(parent, DW_AT::name,
                     // All DWARF type tags (this is everything that ends
                     // with _type except thrown_type).
                     {DW_TAG::array_type,       DW_TAG::class_type,
                      DW_TAG::enumeration_type, DW_TAG::pointer_type,
                      DW_TAG::reference_type,   DW_TAG::string_type,
                      DW_TAG::structure_type,   DW_TAG::subroutine_type,
                      DW_TAG::union_type,       DW_TAG::ptr_to_member_type,
                      DW_TAG::set_type,         DW_TAG::subrange_type,
                      DW_TAG::base_type,        DW_TAG::const_type,
                      DW_TAG::file_type,        DW_TAG::packed_type,
                      DW_TAG::volatile_type,    DW_TAG::restrict_type,
                      DW_TAG::interface_type,   DW_TAG::unspecified_type,
                      DW_TAG::shared_type,      DW_TAG::rvalue_reference_type});
}

auto die_str_map::operator[](const char *val) const -> const die & {
  // Do we have this value?
  auto it = m->str_map.find(val);
  if (it != m->str_map.end()) return it->second;
  // Read more until we find the value or the end
  while (m->pos != m->end) {
    const die &d = *m->pos;
    ++m->pos;

    if (!m->accept.count(d.tag) || !d.has(m->attr)) continue;
    value dval(d[m->attr]);
    if (dval.get_type() != value::type::string) continue;
    const char *dstr = dval.as_cstr();
    m->str_map[dstr] = d;
    if (strcmp(val, dstr) == 0) return m->str_map[dstr];
  }
  // Not found
  return m->invalid;
}

DWARFPP_END_NAMESPACE
