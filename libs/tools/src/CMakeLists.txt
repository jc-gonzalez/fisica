#=============================================================================
# CMakeLists.txt
# src directory
#
# Copyright (C) 2015, J C Gonzalez
#
# This file is part of tools
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

SET (tools_src
  alert.cpp
dbg.cpp
dwatcher.cpp
filetools.cpp
launcher.cpp
#rwc.cpp
sm.cpp
timer.cpp
tools.cpp
  )

SET (tools_hdr
  alert.h
dbg.h
dwatcher.h
filetools.h
launcher.h
metadatareader.h
process.h
propdef.h
#rwc.h
scopeexit.h
sm.h
timer.h
tools.h
  )

SET (tools_inthdr
  )

ADD_LIBRARY (tools SHARED ${tools_src} )

SET_TARGET_PROPERTIES (tools PROPERTIES 
  LINKER_LANGUAGE CXX
  SOVERSION ${tools_VERSION}
  VERSION   ${tools_VERSION})

INSTALL (TARGETS tools
  RUNTIME DESTINATION bin
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib)

INSTALL (FILES ${tools_hdr}
  DESTINATION include)

