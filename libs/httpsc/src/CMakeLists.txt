#=============================================================================
# CMakeLists.txt
# src directory
#
# Copyright (C) 2015, J C Gonzalez
#
# This file is part of httpsc
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

SET (httpsc_src
dbg.cpp
rwc.cpp
filetools.cpp
httpsc.cpp
  )

SET (httpsc_hdr
dbg.h
rwc.h
scopeexit.h
filetools.h
httpsc.h
  )

SET (httpsc_inthdr
  )

ADD_LIBRARY (httpsc SHARED ${httpsc_src} )

#SET (LIBS_DIR /Users/jcgonzalez/opt)

#INCLUDE_DIRECTORIES (. ${LIBS_DIR}/include)
#LINK_DIRECTORIES (${LIBS_DIR}/include)

TARGET_INCLUDE_DIRECTORIES (httpsc PUBLIC . /usr/include/curl)
TARGET_LINK_LIBRARIES (httpsc curl)

#SET_TARGET_PROPERTIES (cdetsim PROPERTIES 
#  LINKER_LANGUAGE CXX
#  VERSION   ${cdetsim_VERSION})

SET_TARGET_PROPERTIES (httpsc PROPERTIES 
  LINKER_LANGUAGE CXX
  SOVERSION ${httpsc_VERSION}
  VERSION   ${httpsc_VERSION})

INSTALL (TARGETS httpsc
  RUNTIME DESTINATION bin
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib)

INSTALL (FILES ${httpsc_hdr}
  DESTINATION include)

