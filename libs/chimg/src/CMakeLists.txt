#=============================================================================
# CMakeLists.txt
# src directory
#
# Copyright (C) 2015, J C Gonzalez
#
# This file is part of chimg
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

SET (chimg_src
  moments.cpp
  )

SET (chimg_hdr
  moments.h
  )

SET (chimg_inthdr
  )

ADD_LIBRARY (chimg SHARED ${chimg_src} )

SET (LIBS_DIR /Users/jcgonzalez/opt)

INCLUDE_DIRECTORIES (. ${LIBS_DIR}/include)
LINK_DIRECTORIES (${LIBS_DIR}/include)

TARGET_INCLUDE_DIRECTORIES (chimg PUBLIC .)
TARGET_LINK_LIBRARIES (chimg mathtools)

SET_TARGET_PROPERTIES (chimg PROPERTIES 
  LINKER_LANGUAGE CXX
  SOVERSION ${chimg_VERSION}
  VERSION   ${chimg_VERSION})

INSTALL (TARGETS chimg
  RUNTIME DESTINATION bin
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib)

INSTALL (FILES ${chimg_hdr}
  DESTINATION include)
