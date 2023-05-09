// Copyright (c) 2013 Austin T. Clements. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

#include <cstring>

#include "dwarf++.hh"

using namespace std;

DWARFPP_BEGIN_NAMESPACE

static const struct {
  const char *name;
  section_type type;
} sections[] = {
    {".debug_abbrev", section_type::abbrev},
    {".debug_aranges", section_type::aranges},
    {".debug_frame", section_type::frame},
    {".debug_info", section_type::info},
    {".debug_line", section_type::line},
    {".debug_loc", section_type::loc},
    {".debug_macinfo", section_type::macinfo},
    {".debug_pubnames", section_type::pubnames},
    {".debug_pubtypes", section_type::pubtypes},
    {".debug_ranges", section_type::ranges},
    {".debug_str", section_type::str},
    {".debug_types", section_type::types},

    {".debug_addr", section_type::addr},
    {".debug_line_str", section_type::line_str},
    {".debug_loclists", section_type::loclists},  ///< V5
    {".debug_rnglists", section_type::rnglists},  ///< V5
    {".debug_macro", section_type::macro},
    {".debug_sup", section_type::sup},
    {".debug_altlink", section_type::altlink},
    {".debug_str_offsets", section_type::str_offsets},
    {".debug_abbrev_dwo", section_type::abbrev_dwo},
    {".debug_info_dwo", section_type::info_dwo},
    {".debug_line_dwo", section_type::line_dwo},
    {".debug_loclists_dwo", section_type::loclists_dwo},
    {".debug_macro_dwo", section_type::macro_dwo},
    {".debug_str_dwo", section_type::str_dwo},
    {".debug_str_offset_dwo", section_type::str_offset_dwo},
    {".debug_cu_index", section_type::cu_index},
    {".debug_tu_index", section_type::tu_index},
};

bool elf::section_name_to_type(const char *name, section_type *out) {
  return std::ranges::any_of(sections, [out, name](auto &x) {
    if (strcmp(x.name, name) == 0) {
      *out = x.type;
      return true;
    }
    return false;
  });
}

const char *elf::section_type_to_name(section_type type) {
  for (auto &sec : sections) {
    if (sec.type == type) return sec.name;
  }
  return nullptr;
}

DWARFPP_END_NAMESPACE
