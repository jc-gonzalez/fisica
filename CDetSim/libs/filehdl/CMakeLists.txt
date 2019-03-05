#======================================================================
# CMakeLists.txt
# QPF - Prototype of QLA Processing Framework
# General Project File
#======================================================================
# Author: J C Gonzalez - 2015-2018
# Copyright (C) 2015-2018 Euclid SOC Team at ESAC
#======================================================================
include (../common.cmake)

project (filehdl)

set (libfilehdl_hdr
  genfhdl.h
  jsonfhdl.h
  qdtrephdl.h
)

set (libfilehdl_src
  jsonfhdl.cpp
  qdtrephdl.cpp
)

#===== Project sections/libraries =======

add_library(filehdl SHARED ${libfilehdl_src})
target_include_directories (filehdl PUBLIC .
  ${JSON_ROOT_DIR}
  ${INFIX_ROOT_DIR}
  ${TOOLS_ROOT_DIR}
  ${STR_ROOT_DIR}
  ${LOG_ROOT_DIR}
  ${FILEHDL_ROOT_DIR})
target_link_libraries (filehdl
  infix str log json tools pthread)
set_target_properties (filehdl PROPERTIES LINKER_LANGUAGE CXX)
install (TARGETS filehdl
         RUNTIME DESTINATION bin
         ARCHIVE DESTINATION lib
         LIBRARY DESTINATION lib)
