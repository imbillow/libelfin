// Copyright (c) 2013 Austin T. Clements. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

#ifndef DWARFPP_INTERNAL_HH_
#define DWARFPP_INTERNAL_HH_

#include <stdexcept>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <vector>

#include "../elf/to_hex.hh"
#include "dwarf++.hh"

DWARFPP_BEGIN_NAMESPACE

enum class format { unknown, dwarf32, dwarf64 };

enum class byte_order { lsb, msb };

/**
 * Return this system's native byte order.
 */
static inline auto native_order() -> byte_order {
  static const union {
    int i;
    char c[sizeof(int)];
  } test = {1};

  return test.c[0] == 1 ? byte_order::lsb : byte_order::msb;
}

/**
 * A single DWARF section or a slice of a section.  This also tracks
 * dynamic information necessary to decode values in this section.
 */
struct section {
  section_type type;
  const char *begin, *end;
  const format fmt;
  const byte_order ord;
  unsigned addr_size;
  void *unit_type{};

  section(section_type type, const void *begin, section_length length,
          byte_order ord, format fmt = format::unknown, unsigned addr_size = 0)
      : type(type),
        begin((char *)begin),
        end((char *)begin + length),
        fmt(fmt),
        ord(ord),
        addr_size(addr_size) {}

  section(const section &o) = default;

  auto slice(section_offset start, section_length len,
             format _fmt = format::unknown, unsigned _addr_size = 0)
      -> std::shared_ptr<section> {
    if (_fmt == format::unknown) _fmt = this->fmt;
    if (_addr_size == 0) _addr_size = this->addr_size;

    return std::make_shared<section>(
        type, begin + start, std::min(len, (section_length)(end - begin)), ord,
        _fmt, _addr_size);
  }

  [[nodiscard]] auto size() const -> size_t { return end - begin; }
};

/**
 * A cursor pointing into a DWARF section.  Provides deserialization
 * operations and bounds checking.
 */
struct cursor {
  // XXX There's probably a lot of overhead to maintaining the
  // shared pointer to the section from this.  Perhaps the rule
  // should be that all objects keep the dwarf::impl alive
  // (directly or indirectly) and that keeps the loader alive,
  // so a cursor just needs a regular section*.

  std::shared_ptr<section> sec;
  const char *pos;

  cursor() : pos(nullptr) {}
  explicit cursor(const std::shared_ptr<section> &sec,
                  section_offset offset = 0)
      : sec(sec), pos(sec->begin + offset) {}

  /**
   * Read a subsection.  The cursor must be at an initial
   * length.  After, the cursor will point just past the end of
   * the subsection.  The returned section has the appropriate
   * DWARF format and begins at the current location of the
   * cursor (so this is usually followed by a
   * skip_initial_length).
   */
  auto subsection() -> std::shared_ptr<section>;
  /**
   * Integers may be encoded using “Little-Endian Base 128” (LEB128) numbers.
   * LEB128 is a scheme for encoding integers densely that exploits the
   * assumption that most integers are small in magnitude.
   *
   * The encoding for signed, two’s complement LEB128 (SLEB128) numbers.
   */
  auto sleb128() -> std::int64_t;
  auto offset() -> section_offset;
  void string(std::string &out);
  auto cstr(size_t *size_out = nullptr) -> const char *;

  void ensure(section_offset bytes) {
    if ((section_offset)(sec->end - pos) < bytes || pos >= sec->end)
      underflow();
  }

  template <typename T>
  auto fixed() -> T {
    ensure(sizeof(T));
    static_assert(sizeof(T) <= 8, "T too big");
    uint64_t val = 0;
    const auto *p = (const unsigned char *)pos;
    if (sec->ord == byte_order::lsb) {
      for (unsigned i = 0; i < sizeof(T); i++)
        val |= ((uint64_t)p[i]) << (i * 8);
    } else {
      for (unsigned i = 0; i < sizeof(T); i++)
        val = (val << 8) | (uint64_t)p[i];
    }
    pos += sizeof(T);
    return (T)val;
  }

  /**
   * Integers may be encoded using “Little-Endian Base 128” (LEB128) numbers.
   * LEB128 is a scheme for encoding integers densely that exploits the
   * assumption that most integers are small in magnitude.
   */
  auto uleb128() -> std::uint64_t {
    // Appendix C
    // XXX Pre-compute all two byte ULEB's
    std::uint64_t result = 0;
    int shift = 0;
    while (pos < sec->end) {
      uint8_t byte = *(uint8_t *)(pos++);
      result |= (uint64_t)(byte & 0x7f) << shift;
      if ((byte & 0x80) == 0) return result;
      shift += 7;
    }
    underflow();
    return 0;
  }

  auto address() -> taddr {
    switch (sec->addr_size) {
      case 1:
        return fixed<uint8_t>();
      case 2:
        return fixed<uint16_t>();
      case 4:
        return fixed<uint32_t>();
      case 8:
        return fixed<uint64_t>();
      default:
        throw std::runtime_error("address size " +
                                 std::to_string(sec->addr_size) +
                                 " not supported");
    }
  }

  void skip_initial_length();
  void skip_form(DW_FORM form);

  auto operator+=(section_offset offset) -> cursor & {
    pos += offset;
    return *this;
  }

  auto operator+(section_offset offset) const -> cursor {
    return {sec, pos + offset};
  }

  auto operator<(const cursor &o) const -> bool { return pos < o.pos; }

  [[nodiscard]] auto end() const -> bool { return pos >= sec->end; }

  [[nodiscard]] auto valid() const -> bool { return pos != nullptr; }

  [[nodiscard]] auto get_section_offset() const -> section_offset {
    return pos - sec->begin;
  }

 private:
  cursor(std::shared_ptr<section> sec, const char *pos)
      : sec(std::move(sec)), pos(pos) {}

  static void underflow();
};

/**
 * An attribute specification in an abbrev.
 */
struct attribute_spec {
  DW_AT name;
  DW_FORM form;

  // Computed information
  value::type type;

  attribute_spec(DW_AT name, DW_FORM form);
};

using abbrev_code = std::uint64_t;

/**
 * An entry in .debug_abbrev.
 */
struct abbrev_entry {
  abbrev_code code{0};
  DW_TAG tag{};
  bool children{};
  std::vector<attribute_spec> attributes;

  abbrev_entry() {}

  auto read(cursor *cur) -> bool;
};

/**
 * A section header in .debug_pubnames or .debug_pubtypes.
 */
struct name_unit {
  u16 version{};
  section_offset debug_info_offset{};
  section_length debug_info_length{};
  // Cursor to the first name_entry in this unit.  This cursor's
  // section is limited to this unit.
  cursor entries;

  void read(cursor *cur) {
    // Section 7.19
    std::shared_ptr<section> subsec = cur->subsection();
    cursor sub(subsec);
    sub.skip_initial_length();
    version = sub.fixed<u16>();
    if (version != 2)
      throw format_error("unknown name unit version " +
                         std::to_string(version));
    debug_info_offset = sub.offset();
    debug_info_length = sub.offset();
    entries = sub;
  }
};

/**
 * An entry in a .debug_pubnames or .debug_pubtypes unit.
 */
struct name_entry {
  section_offset offset;
  std::string name;

  void read(cursor *cur) {
    offset = cur->offset();
    cur->string(name);
  }
};

DWARFPP_END_NAMESPACE

#endif
