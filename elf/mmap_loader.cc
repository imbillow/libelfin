// Copyright (c) 2013 Austin T. Clements. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include <cerrno>
#include <system_error>

#include "elf++.hh"

using namespace std;

ELFPP_BEGIN_NAMESPACE

class mmap_loader : public loader {
  void *base;
  size_t lim;

 public:
  explicit mmap_loader(int fd) {
    off_t end = lseek(fd, 0, SEEK_END);
    if (end == (off_t)-1)
      throw system_error(errno, system_category(), "finding file length");
    lim = end;

    base = mmap(nullptr, lim, PROT_READ, MAP_SHARED, fd, 0);
    if (base == MAP_FAILED)
      throw system_error(errno, system_category(), "mmap'ing file");
    close(fd);
  }

  ~mmap_loader() override { munmap(base, lim); }

  auto load(off_t offset, size_t size) -> const void * override {
    if (offset + size > lim) throw range_error("offset exceeds file size");
    return (const char *)base + offset;
  }
};

auto create_mmap_loader(int fd) -> std::shared_ptr<loader> {
  return make_shared<mmap_loader>(fd);
}

ELFPP_END_NAMESPACE
