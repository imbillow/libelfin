// Copyright (c) 2013 Austin T. Clements. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

#ifndef ELFPP_HH_
#define ELFPP_HH_

#include <cstddef>
#include <memory>
#include <stdexcept>
#include <vector>

#include "common.hh"
#include "data.hh"

ELFPP_BEGIN_NAMESPACE

class elf;
class loader;
class section;
class strtab;
class symtab;
class segment;
// XXX Audit for binary compatibility

// XXX Segments, other section types

/**
 * An exception indicating malformed ELF data.
 */
class format_error : public std::runtime_error {
 public:
  explicit format_error(const std::string &what_arg)
      : std::runtime_error(what_arg) {}
  explicit format_error(const char *what_arg) : std::runtime_error(what_arg) {}
};

/**
 * An ELF file.
 *
 * This class is internally reference counted and efficiently
 * copyable.
 *
 * Raw pointers to ELF data returned by any method of this object or
 * any object derived from this object point directly into loaded
 * section data.  Hence, callers must ensure that the loader passed to
 * this file remains live as long as any such pointer is in use.
 * Keeping any object that can return such a pointer live is
 * sufficient to keep the loader live.
 */
class elf {
 public:
  /**
   * Construct an ELF file that is backed by data read from the
   * given loader.
   */
  explicit elf(const std::shared_ptr<loader> &l);

  /**
   * Construct an ELF file that is initially not valid.  Calling
   * methods other than operator= and valid on this results in
   * undefined behavior.
   */
  elf() = default;
  elf(const elf &o) = default;
  elf(elf &&o) = default;

  auto operator=(const elf &o) -> elf & = default;

  [[nodiscard]] auto valid() const -> bool { return !!m; }

  /**
   * Return the ELF file header in canonical form (ELF64 in
   * native byte order).
   */
  [[nodiscard]] auto get_hdr() const -> const Ehdr<> &;

  /**
   * Return the loader used by this file.
   */
  [[nodiscard]] auto get_loader() const -> std::shared_ptr<loader>;

  /**
   * Return the segments in this file.
   */
  [[nodiscard]] auto segments() const -> const std::vector<segment> &;

  /**
   * Return the segment at the given index. If no such segment
   * is found, return an invalid segment.
   */
  [[nodiscard]] auto get_segment(unsigned index) const -> const segment &;

  /**
   * Return the sections in this file.
   */
  [[nodiscard]] auto sections() const -> const std::vector<section> &;

  /**
   * Return the section with the specified name. If no such
   * section is found, return an invalid section.
   */
  [[nodiscard]] auto get_section(const std::string &name) const
      -> const section &;

  /**
   * Return the section at the given index.  If no such section
   * is found, return an invalid section.
   */
  [[nodiscard]] auto get_section(unsigned index) const -> const section &;

 private:
  struct impl;
  std::shared_ptr<impl> m;
};

/**
 * An interface for loading sections of an ELF file.
 */
class loader {
 public:
  virtual ~loader() = default;

  /**
   * Load the requested file section into memory and return a
   * pointer to the beginning of it.  This memory must remain
   * valid and unchanged until the loader is destroyed.  If the
   * loader cannot satisfy the full request for any reason
   * (including a premature EOF), it must throw an exception.
   */
  virtual auto load(off_t offset, size_t size) -> const void * = 0;
};

/**
 * An mmap-based loader that maps requested sections on demand.  This
 * will close fd when done, so the caller should dup the file
 * descriptor if it intends to continue using it.
 */
auto create_mmap_loader(int fd) -> std::shared_ptr<loader>;

/**
 * An exception indicating that a section is not of the requested type.
 */
class section_type_mismatch : public std::logic_error {
 public:
  explicit section_type_mismatch(const std::string &what_arg)
      : std::logic_error(what_arg) {}
  explicit section_type_mismatch(const char *what_arg)
      : std::logic_error(what_arg) {}
};

/**
 * An ELF segment.
 *
 * This class is internally reference counted and efficiently
 * copyable.
 */
class segment {
 public:
  /**
   * Construct a segment that is initially not valid.  Calling
   * methods other than operator= and valid on this results in
   * undefined behavior.
   */
  segment() = default;

  segment(const elf &f, const void *hdr);
  segment(const segment &o) = default;
  segment(segment &&o) = default;

  /**
   * Return true if this segment is valid and corresponds to a
   * segment in the ELF file.
   */
  [[nodiscard]] auto valid() const -> bool { return !!m; }

  /**
   * Return the ELF section header in canonical form (ELF64 in
   * native byte order).
   */
  [[nodiscard]] auto get_hdr() const -> const Phdr<> &;

  /**
   * Return this segment's data. The returned buffer will
   * be file_size() bytes long.
   */
  [[nodiscard]] auto data() const -> const void *;

  /**
   * Return the on disk size of this segment in bytes.
   */
  [[nodiscard]] auto file_size() const -> size_t;

  /**
   * Return the in-memory size of this segment in bytes.
   * Bytes between file_size() and mem_size() are implicitly zeroes.
   */
  [[nodiscard]] auto mem_size() const -> size_t;

 private:
  struct impl;
  std::shared_ptr<impl> m;
};

/**
 * An ELF section.
 *
 * This class is internally reference counted and efficiently
 * copyable.
 */
class section {
 public:
  /**
   * Construct a section that is initially not valid.  Calling
   * methods other than operator= and valid on this results in
   * undefined behavior.
   */
  section() = default;

  section(const elf &f, const void *hdr);
  section(const section &o) = default;
  section(section &&o) = default;

  /**
   * Return true if this section is valid and corresponds to a
   * section in the ELF file.
   */
  [[nodiscard]] auto valid() const -> bool { return !!m; }

  /**
   * Return the ELF section header in canonical form (ELF64 in
   * native byte order).
   */
  [[nodiscard]] auto get_hdr() const -> const Shdr<> &;

  /**
   * Return this section's name.
   */
  auto get_name(size_t *len_out) const -> const char *;
  /**
   * Return this section's name.  The returned string copies its
   * data, so loader liveness requirements don't apply.
   */
  [[nodiscard]] auto get_name() const -> std::string;

  /**
   * Return this section's data.  If this is a NOBITS section,
   * return nullptr.
   */
  [[nodiscard]] auto data() const -> const void *;
  /**
   * Return the size of this section in bytes.
   */
  [[nodiscard]] auto size() const -> size_t;

  /**
   * Return this section as a strtab.  Throws
   * section_type_mismatch if this section is not a string
   * table.
   */
  [[nodiscard]] auto as_strtab() const -> strtab;

  /**
   * Return this section as a symtab.  Throws
   * section_type_mismatch if this section is not a symbol
   * table.
   */
  [[nodiscard]] auto as_symtab() const -> symtab;

 private:
  struct impl;
  std::shared_ptr<impl> m;
};

/**
 * A string table.
 *
 * This class is internally reference counted and efficiently
 * copyable.
 */
class strtab {
 public:
  /**
   * Construct a strtab that is initially not valid.  Calling
   * methods other than operator= and valid on this results in
   * undefined behavior.
   */
  strtab() = default;
  strtab(const elf &f, const void *data, size_t size);

  [[nodiscard]] auto valid() const -> bool { return !!m; }

  /**
   * Return the string at the given offset in this string table.
   * If the offset is out of bounds, throws std::range_error.
   * This is very efficient since the returned pointer points
   * directly into the loaded section, though this still
   * verifies that the returned string is NUL-terminated.
   */
  auto get(Elf64::Off offset, size_t *len_out) const -> const char *;
  /**
   * Return the string at the given offset in this string table.
   */
  [[nodiscard]] auto get(Elf64::Off offset) const -> std::string;

 private:
  struct impl;
  std::shared_ptr<impl> m;
};

/**
 * A symbol from a symbol table.
 */
class sym {
  const strtab strs;
  Sym<> data{};

 public:
  sym(const elf &f, const void *data, strtab strs);

  /**
   * Return this symbol's raw data.
   */
  [[nodiscard]] auto get_data() const -> const Sym<> & { return data; }

  /**
   * Return this symbol's name.
   *
   * This returns a pointer into the string table and, as such,
   * is very efficient.  If len_out is non-nullptr, *len_out
   * will be set the length of the returned string.
   */
  auto get_name(size_t *len_out) const -> const char *;

  /**
   * Return this symbol's name as a string.
   */
  [[nodiscard]] auto get_name() const -> std::string;
};

/**
 * A symbol table.
 *
 * This class is internally reference counted and efficiently
 * copyable.
 */
class symtab {
 public:
  /**
   * Construct a symtab that is initially not valid.  Calling
   * methods other than operator= and valid on this results in
   * undefined behavior.
   */
  symtab() = default;
  symtab(const elf &f, const void *data, size_t size, const strtab &strs);

  [[nodiscard]] auto valid() const -> bool { return !!m; }

  class iterator {
    const elf f;
    const strtab strs;
    const char *pos;
    size_t stride;

    iterator(const symtab &tab, const char *pos);
    friend class symtab;

   public:
    auto operator*() const -> sym { return sym(f, pos, strs); }

    auto operator++() -> iterator & { return *this += 1; }

    auto operator++(int) -> const iterator {
      iterator cur(*this);
      *this += 1;
      return cur;
    }

    auto operator+=(std::ptrdiff_t x) -> iterator & {
      pos += x * stride;
      return *this;
    }

    auto operator-=(std::ptrdiff_t x) -> iterator & {
      pos -= x * stride;
      return *this;
    }

    auto operator==(iterator &o) const -> bool { return pos == o.pos; }

    auto operator!=(iterator &o) const -> bool { return pos != o.pos; }
  };

  /**
   * Return an iterator to the first symbol.
   */
  [[nodiscard]] auto begin() const -> iterator;

  /**
   * Return an iterator just past the last symbol.
   */
  [[nodiscard]] auto end() const -> iterator;

 private:
  struct impl;
  std::shared_ptr<impl> m;
};

ELFPP_END_NAMESPACE

#endif
