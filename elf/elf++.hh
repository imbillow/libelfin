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

  elf &operator=(const elf &o) = default;

  [[nodiscard]] bool valid() const { return !!m; }

  /**
   * Return the ELF file header in canonical form (ELF64 in
   * native byte order).
   */
  [[nodiscard]] const Ehdr<> &get_hdr() const;

  /**
   * Return the loader used by this file.
   */
  [[nodiscard]] std::shared_ptr<loader> get_loader() const;

  /**
   * Return the segments in this file.
   */
  [[nodiscard]] const std::vector<segment> &segments() const;

  /**
   * Return the segment at the given index. If no such segment
   * is found, return an invalid segment.
   */
  [[nodiscard]] const segment &get_segment(unsigned index) const;

  /**
   * Return the sections in this file.
   */
  [[nodiscard]] const std::vector<section> &sections() const;

  /**
   * Return the section with the specified name. If no such
   * section is found, return an invalid section.
   */
  [[nodiscard]] const section &get_section(const std::string &name) const;

  /**
   * Return the section at the given index.  If no such section
   * is found, return an invalid section.
   */
  [[nodiscard]] const section &get_section(unsigned index) const;

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
  virtual const void *load(off_t offset, size_t size) = 0;
};

/**
 * An mmap-based loader that maps requested sections on demand.  This
 * will close fd when done, so the caller should dup the file
 * descriptor if it intends to continue using it.
 */
std::shared_ptr<loader> create_mmap_loader(int fd);

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
  [[nodiscard]] bool valid() const { return !!m; }

  /**
   * Return the ELF section header in canonical form (ELF64 in
   * native byte order).
   */
  [[nodiscard]] const Phdr<> &get_hdr() const;

  /**
   * Return this segment's data. The returned buffer will
   * be file_size() bytes long.
   */
  [[nodiscard]] const void *data() const;

  /**
   * Return the on disk size of this segment in bytes.
   */
  [[nodiscard]] size_t file_size() const;

  /**
   * Return the in-memory size of this segment in bytes.
   * Bytes between file_size() and mem_size() are implicitly zeroes.
   */
  [[nodiscard]] size_t mem_size() const;

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
  [[nodiscard]] bool valid() const { return !!m; }

  /**
   * Return the ELF section header in canonical form (ELF64 in
   * native byte order).
   */
  [[nodiscard]] const Shdr<> &get_hdr() const;

  /**
   * Return this section's name.
   */
  const char *get_name(size_t *len_out) const;
  /**
   * Return this section's name.  The returned string copies its
   * data, so loader liveness requirements don't apply.
   */
  [[nodiscard]] std::string get_name() const;

  /**
   * Return this section's data.  If this is a NOBITS section,
   * return nullptr.
   */
  [[nodiscard]] const void *data() const;
  /**
   * Return the size of this section in bytes.
   */
  [[nodiscard]] size_t size() const;

  /**
   * Return this section as a strtab.  Throws
   * section_type_mismatch if this section is not a string
   * table.
   */
  [[nodiscard]] strtab as_strtab() const;

  /**
   * Return this section as a symtab.  Throws
   * section_type_mismatch if this section is not a symbol
   * table.
   */
  [[nodiscard]] symtab as_symtab() const;

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

  [[nodiscard]] bool valid() const { return !!m; }

  /**
   * Return the string at the given offset in this string table.
   * If the offset is out of bounds, throws std::range_error.
   * This is very efficient since the returned pointer points
   * directly into the loaded section, though this still
   * verifies that the returned string is NUL-terminated.
   */
  const char *get(Elf64::Off offset, size_t *len_out) const;
  /**
   * Return the string at the given offset in this string table.
   */
  [[nodiscard]] std::string get(Elf64::Off offset) const;

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
  [[nodiscard]] const Sym<> &get_data() const { return data; }

  /**
   * Return this symbol's name.
   *
   * This returns a pointer into the string table and, as such,
   * is very efficient.  If len_out is non-nullptr, *len_out
   * will be set the length of the returned string.
   */
  const char *get_name(size_t *len_out) const;

  /**
   * Return this symbol's name as a string.
   */
  [[nodiscard]] std::string get_name() const;
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

  [[nodiscard]] bool valid() const { return !!m; }

  class iterator {
    const elf f;
    const strtab strs;
    const char *pos;
    size_t stride;

    iterator(const symtab &tab, const char *pos);
    friend class symtab;

   public:
    sym operator*() const { return sym(f, pos, strs); }

    iterator &operator++() { return *this += 1; }

    const iterator operator++(int) {
      iterator cur(*this);
      *this += 1;
      return cur;
    }

    iterator &operator+=(std::ptrdiff_t x) {
      pos += x * stride;
      return *this;
    }

    iterator &operator-=(std::ptrdiff_t x) {
      pos -= x * stride;
      return *this;
    }

    bool operator==(iterator &o) const { return pos == o.pos; }

    bool operator!=(iterator &o) const { return pos != o.pos; }
  };

  /**
   * Return an iterator to the first symbol.
   */
  [[nodiscard]] iterator begin() const;

  /**
   * Return an iterator just past the last symbol.
   */
  [[nodiscard]] iterator end() const;

 private:
  struct impl;
  std::shared_ptr<impl> m;
};

ELFPP_END_NAMESPACE

#endif
