// Copyright (c) 2013 Austin T. Clements. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

#include <utility>

#include "internal.hh"

using namespace std;

DWARFPP_BEGIN_NAMESPACE

//////////////////////////////////////////////////////////////////
// class dwarf
//

struct dwarf::impl {
  explicit impl(std::shared_ptr<loader> l) : l(std::move(l)) {}

  std::shared_ptr<loader> l;

  std::shared_ptr<section> sec_info;
  std::shared_ptr<section> sec_abbrev;

  std::vector<compilation_unit> compilation_units;

  std::unordered_map<uint64_t, type_unit> type_units;
  bool have_type_units{false};

  std::map<section_type, std::shared_ptr<section>> sections;
};

dwarf::dwarf(const std::shared_ptr<loader> &_loader)
    : m(make_shared<impl>(_loader)) {
  const void *data;
  size_t size;

  // Get required sections
  data = _loader->load(section_type::info, &size);
  if (!data) throw format_error("required .debug_info section missing");
  m->sec_info =
      make_shared<section>(section_type::info, data, size, byte_order::lsb);

  // Sniff the endianness from the version field of the first
  // CU. This is always a small but non-zero integer.
  cursor endcur(m->sec_info);
  // Skip length.
  section_length length = endcur.fixed<u32>();
  if (length == 0xffffffff) endcur.fixed<uint64_t>();
  // Get version in both little and big endian.
  auto version = endcur.fixed<u16>();
  u16 versionbe = (version >> 8) | ((version & 0xFF) << 8);
  if (versionbe < version) {
    m->sec_info =
        make_shared<section>(section_type::info, data, size, byte_order::msb);
  }

  data = _loader->load(section_type::abbrev, &size);
  if (!data) throw format_error("required .debug_abbrev section missing");
  m->sec_abbrev =
      make_shared<section>(section_type::abbrev, data, size, m->sec_info->ord);

  // Get compilation units.  Everything derives from these, so
  // there's no point in doing it lazily.
  cursor infocur(m->sec_info);
  while (!infocur.end()) {
    // XXX Circular reference.  Given that we now require
    // the dwarf object to stick around for DIEs, maybe we
    // might as well require that for units, too.
    m->compilation_units.emplace_back(*this, infocur.get_section_offset());
    infocur.subsection();
  }
}

dwarf::~dwarf() = default;

auto dwarf::compilation_units() const -> const std::vector<compilation_unit> & {
  static std::vector<compilation_unit> empty;
  if (!m) return empty;
  return m->compilation_units;
}

auto dwarf::get_type_unit(uint64_t type_signature) const -> const type_unit & {
  if (!m->have_type_units) {
    cursor tucur(get_section(section_type::types));
    while (!tucur.end()) {
      // XXX Circular reference
      type_unit tu(*this, tucur.get_section_offset());
      m->type_units[tu.get_type_signature()] = tu;
      tucur.subsection();
    }
    m->have_type_units = true;
  }
  if (!m->type_units.count(type_signature))
    throw out_of_range("type signature 0x" + to_hex(type_signature));
  return m->type_units[type_signature];
}

auto dwarf::get_section(section_type type) const -> std::shared_ptr<section> {
  if (type == section_type::info) return m->sec_info;
  if (type == section_type::abbrev) return m->sec_abbrev;

  auto it = m->sections.find(type);
  if (it != m->sections.end()) return it->second;

  size_t size;
  const void *data = m->l->load(type, &size);
  if (!data)
    throw format_error(std::string(elf::section_type_to_name(type)) +
                       " section missing");
  m->sections[type] = std::make_shared<section>(section_type::str, data, size,
                                                m->sec_info->ord);
  return m->sections[type];
}

//////////////////////////////////////////////////////////////////
// class unit
//

/**
 * Implementation of a unit.
 */
struct unit::impl {
  const dwarf file;
  const section_offset offset;
  const std::shared_ptr<section> subsec;
  const section_offset debug_abbrev_offset;
  const section_offset root_offset;

  // Type unit-only values
  const uint64_t type_signature;
  const section_offset type_offset;

  // Lazily constructed root and type DIEs
  die root, type;

  // Lazily constructed line table
  line_table lt;

  // Map from abbrev code to abbrev.  If the map is dense, it
  // will be stored in the vector; otherwise it will be stored
  // in the map.
  bool have_abbrevs{false};
  std::vector<abbrev_entry> abbrevs_vec;
  std::unordered_map<abbrev_code, abbrev_entry> abbrevs_map;

  impl(dwarf file, section_offset offset, std::shared_ptr<section> subsec,
       section_offset debug_abbrev_offset, section_offset root_offset,
       uint64_t type_signature = 0, section_offset type_offset = 0)
      : file(std::move(file)),
        offset(offset),
        subsec(std::move(subsec)),
        debug_abbrev_offset(debug_abbrev_offset),
        root_offset(root_offset),
        type_signature(type_signature),
        type_offset(type_offset) {}

  void force_abbrevs();
};

unit::~unit() = default;

auto unit::get_dwarf() const -> const dwarf & { return m->file; }

auto unit::get_section_offset() const -> section_offset { return m->offset; }

auto unit::root() const -> const die & {
  if (!m->root.valid()) {
    m->force_abbrevs();
    m->root = die(this);
    m->root.read(m->root_offset);
  }
  return m->root;
}

auto unit::data() const -> const std::shared_ptr<section> & {
  return m->subsec;
}

auto unit::get_abbrev(abbrev_code acode) const -> const abbrev_entry & {
  if (!m->have_abbrevs) m->force_abbrevs();

  if (!m->abbrevs_vec.empty()) {
    if (acode >= m->abbrevs_vec.size()) goto unknown;
    const abbrev_entry &entry = m->abbrevs_vec[acode];
    if (entry.code == 0) goto unknown;
    return entry;
  } else {
    auto it = m->abbrevs_map.find(acode);
    if (it == m->abbrevs_map.end()) goto unknown;
    return it->second;
  }

unknown:
  throw format_error("unknown abbrev code 0x" + to_hex(acode));
}

void unit::impl::force_abbrevs() {
  // XXX Compilation units can share abbrevs.  Parse each table
  // at most once.
  if (have_abbrevs) return;

  // Section 7.5.3
  cursor c(file.get_section(section_type::abbrev), debug_abbrev_offset);
  abbrev_entry entry;
  abbrev_code highest = 0;
  while (entry.read(&c)) {
    abbrevs_map[entry.code] = entry;
    if (entry.code > highest) highest = entry.code;
  }

  // Typically, abbrev codes are assigned linearly, so it's more
  // space efficient and time efficient to store the table in a
  // vector.  Convert to a vector if it's dense enough, by some
  // rough estimate of "enough".
  if (highest * 10 < abbrevs_map.size() * 15) {
    // Move the map into the vector
    abbrevs_vec.resize(highest + 1);
    for (auto &[k, v] : abbrevs_map) abbrevs_vec[k] = std::move(v);
    abbrevs_map.clear();
  }

  have_abbrevs = true;
}

//////////////////////////////////////////////////////////////////
// class compilation_unit
//

compilation_unit::compilation_unit(const dwarf &file, section_offset offset) {
  // Read the CU header (DWARF4 section 7.5.1.1)
  cursor cur(file.get_section(section_type::info), offset);
  std::shared_ptr<section> subsec = cur.subsection();
  cursor sub(subsec);
  sub.skip_initial_length();
  auto version = sub.fixed<u16>();
  if (version < 2 || version > 5)
    throw format_error("unknown compilation unit version " +
                       std::to_string(version));
  // .debug_abbrev-relative offset of this unit's abbrevs
  section_offset debug_abbrev_offset{};
  if (version == 5) {
    subsec->addr_size = sub.fixed<u8>();
    debug_abbrev_offset = sub.offset();
  } else {
    debug_abbrev_offset = sub.offset();
    subsec->addr_size = sub.fixed<u8>();
  }

  m = make_shared<impl>(file, offset, subsec, debug_abbrev_offset,
                        sub.get_section_offset());
}

auto compilation_unit::get_line_table() const -> const line_table & {
  if (!m->lt.valid()) {
    const die &d = root();
    if (!d.has(DW_AT::stmt_list) || !d.has(DW_AT::name)) goto done;

    shared_ptr<section> sec;
    try {
      sec = m->file.get_section(section_type::line);
    } catch (format_error &e) {
      goto done;
    }

    auto comp_dir = d.has(DW_AT::comp_dir) ? at_comp_dir(d) : "";

    m->lt = line_table(sec, d[DW_AT::stmt_list].as_sec_offset(),
                       m->subsec->addr_size, comp_dir, at_name(d));
  }
done:
  return m->lt;
}

//////////////////////////////////////////////////////////////////
// class type_unit
//

type_unit::type_unit(const dwarf &file, section_offset offset) {
  // Read the type unit header (DWARF4 section 7.5.1.2)
  cursor cur(file.get_section(section_type::types), offset);
  std::shared_ptr<section> subsec = cur.subsection();
  cursor sub(subsec);
  sub.skip_initial_length();
  auto version = sub.fixed<u16>();
  if (version != 4)
    throw format_error("unknown type unit version " + std::to_string(version));
  // .debug_abbrev-relative offset of this unit's abbrevs
  section_offset debug_abbrev_offset = sub.offset();
  auto address_size = sub.fixed<u8>();
  subsec->addr_size = address_size;
  auto type_signature = sub.fixed<uint64_t>();
  section_offset type_offset = sub.offset();

  m = make_shared<impl>(file, offset, subsec, debug_abbrev_offset,
                        sub.get_section_offset(), type_signature, type_offset);
}

auto type_unit::get_type_signature() const -> uint64_t {
  return m->type_signature;
}

auto type_unit::type() const -> const die & {
  if (!m->type.valid()) {
    m->force_abbrevs();
    m->type = die(this);
    m->type.read(m->type_offset);
  }
  return m->type;
}

DWARFPP_END_NAMESPACE
