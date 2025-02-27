// Copyright (c) 2013 Austin T. Clements. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

#ifndef ELFPP_TO_HEX_HH_
#define ELFPP_TO_HEX_HH_

#include <string>
#include <type_traits>

template <typename T>
auto to_hex(T v) -> std::string {
  static_assert(std::is_integral<T>::value,
                "to_hex applied to non-integral type");
  if (v == 0) return {"0"};
  char buf[sizeof(T) * 2 + 1];
  char *pos = &buf[sizeof(buf) - 1];
  *pos-- = '\0';
  while (v && pos >= buf) {
    int digit = v & 0xf;
    if (digit < 10)
      *pos = '0' + digit;
    else
      *pos = 'a' + (digit - 10);
    pos--;
    v >>= 4;
  }
  return {pos + 1};
}

#endif  // ELFPP_TO_HEX_HH_
