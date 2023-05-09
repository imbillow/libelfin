cmake_minimum_required(VERSION 3.12)
project(libelfin)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
add_compile_options(-Wall -Wextra -Wpedantic)


set(elf_to_stringcc ${CMAKE_SOURCE_DIR}/elf/to_string.cc)
add_custom_target(elf_to_string DEPENDS ${elf_to_stringcc}
        COMMAND make to_string.cc
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/elf
        )

file(GLOB_RECURSE ELF_SRC
        elf/*.cc elf/*.hh
        )
add_library(elf SHARED ${ELF_SRC} ${elf_to_stringcc})
add_dependencies(elf elf_to_string)


set(dwarf_to_stringcc ${CMAKE_SOURCE_DIR}/dwarf/to_string.cc)
add_custom_target(dwarf_to_string
        COMMAND make to_string.cc
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/dwarf
        )

file(GLOB_RECURSE DWARF_SRC
        dwarf/*.cc dwarf/*.hh
        )
add_library(dwarf SHARED ${DWARF_SRC} ${dwarf_to_stringcc})
add_dependencies(dwarf dwarf_to_string)

file(GLOB EXAMPLES examples/*.cc)
foreach (x ${EXAMPLES})
    string(REGEX MATCH "([a-zA-Z0-9-]+)\\.cc$" TGT ${x})
    string(REPLACE .cc "" TGT ${TGT})
    MESSAGE("ADD target ${TGT}")

    add_executable(${TGT} ${x})
    target_include_directories(${TGT} PRIVATE dwarf elf)
    target_link_libraries(${TGT} dwarf elf)
endforeach ()
