// Copyright (c) 2013 Austin T. Clements. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

#include <cassert>
#include <utility>

#include "internal.hh"

using namespace std;

DWARFPP_BEGIN_NAMESPACE

// The expected number of arguments for standard opcodes.  This is
// used to check the opcode_lengths header field for compatibility.
static const int opcode_lengths[] = {0,
                                     // DW_LNS::copy
                                     0, 1, 1, 1, 1,
                                     // DW_LNS::negate_stmt
                                     0, 0, 0, 1, 0,
                                     // DW_LNS::set_epilogue_begin
                                     0, 1};

struct line_table::impl {
  shared_ptr<section> sec;

  // Header information
  section_offset program_offset{};
  u8 minimum_instruction_length{};
  u8 maximum_operations_per_instruction{};
  bool default_is_stmt{};
  s8 line_base{};
  u8 line_range{};
  u8 opcode_base{};
  vector<u8> standard_opcode_lengths;
  vector<string> include_directories;
  vector<file> file_names;

  // The offset in sec following the last read file name entry.
  // File name entries can appear both in the line table header
  // and in the line number program itself.  Since we can
  // iterate over the line number program repeatedly, this keeps
  // track of how far we've gotten so we don't add the same
  // entry twice.
  section_offset last_file_name_end{0};
  // If an iterator has traversed the entire program, then we
  // know we've gathered all file names.
  bool file_names_complete{false};

  impl(){};

  auto read_file_entry(cursor *cur, bool in_header) -> bool;
};

line_table::line_table(const shared_ptr<section> &sec, section_offset offset,
                       unsigned cu_addr_size, const string &cu_comp_dir,
                       const string &cu_name)
    : m(make_shared<impl>()) {
  // XXX DWARF2 and 3 give a weird specification for DW_AT_comp_dir

  string comp_dir, abs_path;
  if (cu_comp_dir.empty() || cu_comp_dir.back() == '/')
    comp_dir = cu_comp_dir;
  else
    comp_dir = cu_comp_dir + '/';

  // Read the line table header (DWARF2 section 6.2.4, DWARF3
  // section 6.2.4, DWARF4 section 6.2.3)
  cursor cur(sec, offset);
  m->sec = cur.subsection();
  cur = cursor(m->sec);
  cur.skip_initial_length();
  m->sec->addr_size = cu_addr_size;

  // Basic header information
  auto version = cur.fixed<u16>();
  if (version < 2 || version > 4)
    throw format_error("unknown line number table version " +
                       std::to_string(version));
  section_length header_length = cur.offset();
  m->program_offset = cur.get_section_offset() + header_length;
  m->minimum_instruction_length = cur.fixed<u8>();
  m->maximum_operations_per_instruction = 1;
  if (version == 4) m->maximum_operations_per_instruction = cur.fixed<u8>();
  if (m->maximum_operations_per_instruction == 0)
    throw format_error(
        "maximum_operations_per_instruction cannot"
        " be 0 in line number table");
  m->default_is_stmt = cur.fixed<u8>();
  m->line_base = cur.fixed<s8>();
  m->line_range = cur.fixed<u8>();
  if (m->line_range == 0)
    throw format_error("line_range cannot be 0 in line number table");
  m->opcode_base = cur.fixed<u8>();

  static_assert(sizeof(opcode_lengths) / sizeof(opcode_lengths[0]) == 13,
                "opcode_lengths table has wrong length");

  // Opcode length table
  m->standard_opcode_lengths.resize(m->opcode_base);
  m->standard_opcode_lengths[0] = 0;
  for (unsigned i = 1; i < m->opcode_base; i++) {
    auto length = cur.fixed<u8>();
    if (length != opcode_lengths[i])
      // The spec never says what to do if the
      // opcode length of a standard opcode doesn't
      // match the header.  Do the safe thing.
      throw format_error("expected " + std::to_string(opcode_lengths[i]) +
                         " arguments for line number opcode " +
                         std::to_string(i) + ", got " + std::to_string(length));
    m->standard_opcode_lengths[i] = length;
  }

  // Include directories list
  string incdir;
  // Include directory 0 is implicitly the compilation unit
  // current directory
  m->include_directories.push_back(comp_dir);
  while (true) {
    cur.string(incdir);
    if (incdir.empty()) break;
    if (incdir.back() != '/') incdir += '/';
    if (incdir[0] == '/')
      m->include_directories.push_back(std::move(incdir));
    else
      m->include_directories.push_back(comp_dir + incdir);
  }

  // File name list
  string file_name;
  // File name 0 is implicitly the compilation unit file name.
  // cu_name can be relative to comp_dir or absolute.
  if (!cu_name.empty() && cu_name[0] == '/')
    m->file_names.emplace_back(cu_name);
  else
    m->file_names.emplace_back(comp_dir + cu_name);
  while (m->read_file_entry(&cur, true))
    ;
}

auto line_table::begin() const -> line_table::iterator {
  if (!valid()) return {nullptr, 0};
  return {this, m->program_offset};
}

auto line_table::end() const -> line_table::iterator {
  if (!valid()) return {nullptr, 0};
  return {this, m->sec->size()};
}

auto line_table::find_address(taddr addr) const -> line_table::iterator {
  iterator prev = begin(), e = end();
  if (prev == e) return prev;

  iterator it = prev;
  for (++it; it != e; prev = it++) {
    if (prev->address <= addr && it->address > addr && !prev->end_sequence)
      return prev;
  }
  prev = e;
  return prev;
}

auto line_table::get_file(unsigned index) const -> const line_table::file * {
  if (index >= m->file_names.size()) {
    // It could be declared in the line table program.
    // This is unlikely, so we don't have to be
    // super-efficient about this.  Just force our way
    // through the whole line table program.
    if (!m->file_names_complete) {
      for (auto &ent : *this) (void)ent;
    }
    if (index >= m->file_names.size())
      throw out_of_range("file name index " + std::to_string(index) +
                         " exceeds file table size of " +
                         std::to_string(m->file_names.size()));
  }
  return &m->file_names[index];
}

auto line_table::impl::read_file_entry(cursor *cur, bool in_header) -> bool {
  assert(cur->sec == sec);

  string file_name;
  cur->string(file_name);
  if (in_header && file_name.empty()) return false;
  uint64_t dir_index = cur->uleb128();
  uint64_t mtime = cur->uleb128();
  uint64_t length = cur->uleb128();

  // Have we already processed this file entry?
  if (cur->get_section_offset() <= last_file_name_end) return true;
  last_file_name_end = cur->get_section_offset();

  if (file_name[0] == '/')
    file_names.emplace_back(std::move(file_name), mtime, length);
  else if (dir_index < include_directories.size())
    file_names.emplace_back(include_directories[dir_index] + file_name, mtime,
                            length);
  else
    throw format_error("file name directory index out of range: " +
                       std::to_string(dir_index));

  return true;
}

line_table::file::file(string path, uint64_t mtime, uint64_t length)
    : path(std::move(std::move(path))), mtime(mtime), length(length) {}

void line_table::entry::reset(bool is_stmt) {
  address = op_index = 0;
  file = nullptr;
  file_index = line = 1;
  column = 0;
  this->is_stmt = is_stmt;
  basic_block = end_sequence = prologue_end = epilogue_begin = false;
  isa = discriminator = 0;
}

auto line_table::entry::get_description() const -> string {
  string res = file->path;
  if (line) {
    res.append(":").append(std::to_string(line));
    if (column) res.append(":").append(std::to_string(column));
  }
  return res;
}

line_table::iterator::iterator(const line_table *table, section_offset pos)
    : table(table), pos(pos) {
  if (table) {
    regs.reset(table->m->default_is_stmt);
    ++(*this);
  }
}

auto line_table::iterator::operator++() -> line_table::iterator & {
  cursor cur(table->m->sec, pos);

  // Execute opcodes until we reach the end of the stream or an
  // opcode emits a line table row
  bool stepped = false, output = false;
  while (!cur.end() && !output) {
    output = step(&cur);
    stepped = true;
  }
  if (stepped && !output) throw format_error("unexpected end of line table");
  if (stepped && cur.end()) {
    // Record that all file names must be known now
    table->m->file_names_complete = true;
  }
  if (output) {
    // Resolve file name of entry
    if (entry.file_index < table->m->file_names.size())
      entry.file = &table->m->file_names[entry.file_index];
    else
      throw format_error("bad file index " + std::to_string(entry.file_index) +
                         " in line table");
  }

  pos = cur.get_section_offset();
  return *this;
}

auto line_table::iterator::step(cursor *cur) -> bool {
  struct line_table::impl *m = table->m.get();

  // Read the opcode (DWARF4 section 6.2.3)
  auto opcode = cur->fixed<u8>();
  if (opcode >= m->opcode_base) {
    // Special opcode (DWARF4 section 6.2.5.1)
    u8 adjusted_opcode = opcode - m->opcode_base;
    unsigned op_advance = adjusted_opcode / m->line_range;
    signed line_inc = m->line_base + (signed)adjusted_opcode % m->line_range;

    regs.line += line_inc;
    regs.address +=
        m->minimum_instruction_length *
        ((regs.op_index + op_advance) / m->maximum_operations_per_instruction);
    regs.op_index =
        (regs.op_index + op_advance) % m->maximum_operations_per_instruction;
    entry = regs;

    regs.basic_block = regs.prologue_end = regs.epilogue_begin = false;
    regs.discriminator = 0;

    return true;
  } else if (opcode != 0) {
    // Standard opcode (DWARF4 sections 6.2.3 and 6.2.5.2)
    //
    // According to the standard, any opcode between the
    // highest defined opcode for a given DWARF version
    // and opcode_base should be treated as a
    // vendor-specific opcode. However, the de facto
    // standard seems to be to process these as standard
    // opcodes even if they're from a later version of the
    // standard than the line table header claims.
    uint64_t uarg;
#pragma GCC diagnostic push
#pragma GCC diagnostic warning "-Wswitch-enum"
    switch ((DW_LNS)opcode) {
      case DW_LNS::copy:
        entry = regs;
        regs.basic_block = regs.prologue_end = regs.epilogue_begin = false;
        regs.discriminator = 0;
        break;
      case DW_LNS::advance_pc:
        // Opcode advance (as for special opcodes)
        uarg = cur->uleb128();
      advance_pc:
        regs.address +=
            m->minimum_instruction_length *
            ((regs.op_index + uarg) / m->maximum_operations_per_instruction);
        regs.op_index =
            (regs.op_index + uarg) % m->maximum_operations_per_instruction;
        break;
      case DW_LNS::advance_line:
        regs.line = (signed)regs.line + cur->sleb128();
        break;
      case DW_LNS::set_file:
        regs.file_index = cur->uleb128();
        break;
      case DW_LNS::set_column:
        regs.column = cur->uleb128();
        break;
      case DW_LNS::negate_stmt:
        regs.is_stmt = !regs.is_stmt;
        break;
      case DW_LNS::set_basic_block:
        regs.basic_block = true;
        break;
      case DW_LNS::const_add_pc:
        uarg = (255 - m->opcode_base) / m->line_range;
        goto advance_pc;
      case DW_LNS::fixed_advance_pc:
        regs.address += cur->fixed<u16>();
        regs.op_index = 0;
        break;
      case DW_LNS::set_prologue_end:
        regs.prologue_end = true;
        break;
      case DW_LNS::set_epilogue_begin:
        regs.epilogue_begin = true;
        break;
      case DW_LNS::set_isa:
        regs.isa = cur->uleb128();
        break;
      default:
        // XXX Vendor extensions
        throw format_error("unknown line number opcode " +
                           to_string((DW_LNS)opcode));
    }
    return ((DW_LNS)opcode == DW_LNS::copy);
  } else {  // opcode == 0
    // Extended opcode (DWARF4 sections 6.2.3 and 6.2.5.3)
    assert(opcode == 0);
    uint64_t length = cur->uleb128();
    section_offset end = cur->get_section_offset() + length;
    opcode = cur->fixed<u8>();
    switch ((DW_LNE)opcode) {
      case DW_LNE::end_sequence:
        regs.end_sequence = true;
        entry = regs;
        regs.reset(m->default_is_stmt);
        break;
      case DW_LNE::set_address:
        regs.address = cur->address();
        regs.op_index = 0;
        break;
      case DW_LNE::define_file:
        m->read_file_entry(cur, false);
        break;
      case DW_LNE::set_discriminator:
        // XXX Only DWARF4
        regs.discriminator = cur->uleb128();
        break;
      case DW_LNE::lo_user... DW_LNE::hi_user:
        // XXX Vendor extensions
        throw runtime_error("vendor line number opcode " +
                            to_string((DW_LNE)opcode) + " not implemented");
      default:
        // XXX Prior to DWARF4, any opcode number
        // could be a vendor extension
        throw format_error("unknown line number opcode " +
                           to_string((DW_LNE)opcode));
    }
#pragma GCC diagnostic pop
    if (cur->get_section_offset() > end)
      throw format_error("extended line number opcode exceeded its size");
    cur += end - cur->get_section_offset();
    return ((DW_LNE)opcode == DW_LNE::end_sequence);
  }
}

DWARFPP_END_NAMESPACE
