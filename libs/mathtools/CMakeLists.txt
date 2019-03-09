#======================================================================
# mathtools - Simple MATHTOOLS library
#======================================================================
# Library project file
# Author: J C Gonzalez - 2019
#======================================================================
cmake_minimum_required(VERSION 2.8)

include (../common.cmake)

project (mathtools)

set (mathtoolsLib_hdr
  mathtools.h)

set (mathtoolsLib_src
  mathtools.cpp)

add_library (mathtools SHARED ${mathtoolsLib_src})
set_target_properties (mathtools PROPERTIES LINKER_LANGUAGE CXX)

add_executable (mathtoolstest main.cpp)
target_link_libraries(mathtoolstest mathtools)

install (TARGETS mathtools
         RUNTIME DESTINATION bin
         ARCHIVE DESTINATION lib
         LIBRARY DESTINATION lib)
