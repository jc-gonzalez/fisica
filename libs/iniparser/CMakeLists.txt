#=============================================================================
# CMakeLists.txt
#
# Copyright (C) 2015, J C Gonzalez
#
# This file is part of IniParser v. 0.1 
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
cmake_minimum_required(VERSION 2.8)

project(IniParser)

set(PROJECT_NAME IniParser)

set(IniParser_MAJOR 0)
set(IniParser_MINOR 1)
set(IniParser_PATCH 0)

set(IniParser_VERSION ${IniParser_MAJOR}.${IniParser_MINOR}.${IniParser_PATCH})
message(STATUS "Building iniparser version ${IniParser_VERSION}")

add_subdirectory(src)
add_subdirectory(examples/iniex1)

set(CPACK_PACKAGE_VERSION_MAJOR ${IniParser_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${IniParser_MINOR})
set(CPACK_PACKAGE_VERSION_PATCH ${IniParser_PATCH})
set(CPACK_GENERATOR "TGZ")
set(CPACK_PACKAGING_INSTALL_PREFIX "")

include(CPack)

