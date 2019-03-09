#=============================================================================
# CMakeLists.txt
#
# Copyright (C) 2019, J C Gonzalez
#
# This file is part of json v. 0.1 
# 
# IniParser is free software: you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or (at your
# option) any later version.
# 
# IniParser is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
# License for more details.
# 
# You should have received a copy of the GNU Lesser General Public License
# along with iniparser.  If not, see <http://www.gnu.org/licenses/>
#=============================================================================

set (jsonLib_hdr
  json.h)

set (jsonLib_src
  json.cpp)

add_library (json SHARED ${jsonLib_src})
set_target_properties (json PROPERTIES
  LINKER_LANGUAGE CXX
  SOVERSION ${json_VERSION}
  VERSION ${json_VERSION})

install (TARGETS json
  RUNTIME DESTINATION bin
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib)

install (FILES ${jsonLib_hdr}
  DESTINATION include)

