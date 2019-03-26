#=============================================================================
# CMakeLists.txt
# src directory
#
# Copyright (C) 2015, J C Gonzalez
#
# This file is part of mathtools
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

SET (mathtools_src
  angles.cpp
  comparisons.cpp
  mathtools.cpp
  mathtools_tuples.cpp
  matrix.cpp
  quaternions.cpp
  surfaces.cpp
  vectors.cpp
  )

SET (mathtools_hdr
  mathtools.h
  angles.h
  comparisons.h
  matrix.h
  quaternions.h
  surfaces.h
  vectors.h
  )

SET (mathtools_inthdr
  )

ADD_LIBRARY (mathtools SHARED ${mathtools_src} )

SET_TARGET_PROPERTIES (mathtools PROPERTIES 
  LINKER_LANGUAGE CXX
  SOVERSION ${mathtools_VERSION}
  VERSION   ${mathtools_VERSION})

INSTALL (TARGETS mathtools
  RUNTIME DESTINATION bin
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib)

INSTALL (FILES ${mathtools_hdr}
  DESTINATION include)
