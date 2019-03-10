#=============================================================================
# CMakeLists.txt
# src directory
#
# Copyright (C) 2015, J C Gonzalez
#
# This file is part of infix
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#=============================================================================

SET (infix_src
  infix.cpp
  )

SET (infix_hdr
  infix.h
  )

SET (infix_inthdr
  )

ADD_LIBRARY (infix SHARED ${infix_src} )
TARGET_INCLUDE_DIRECTORIES (filehdl PUBLIC .
  ${JSON_ROOT_DIR}/src
  ${TOOLS_ROOT_DIR}/src)

SET_TARGET_PROPERTIES (infix PROPERTIES 
  LINKER_LANGUAGE CXX
  SOVERSION ${infix_VERSION}
  VERSION   ${infix_VERSION})

INSTALL (TARGETS infix
  RUNTIME DESTINATION bin
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib)

INSTALL (FILES ${infix_hdr}
  DESTINATION include)

