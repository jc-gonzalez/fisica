#======================================================================
# json - Simple JSON library
#======================================================================
# Library project file
# Author: J C Gonzalez - 2019
#======================================================================
cmake_minimum_required(VERSION 2.8)

include (../common.cmake)

project (json)

set (jsonLib_hdr
  json.h)

set (jsonLib_src
  json.cpp)

add_library (json SHARED ${jsonLib_src})
set_target_properties (json PROPERTIES LINKER_LANGUAGE CXX)

add_executable (jsontest main.cpp)
target_link_libraries(jsontest json)

install (TARGETS json
         RUNTIME DESTINATION bin
         ARCHIVE DESTINATION lib
         LIBRARY DESTINATION lib)
