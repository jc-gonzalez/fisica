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

include_directories(. ${json_SOURCE_DIR}/src)
link_directories(${json_BINARY_DIR}/src)

add_executable(jsontest main.cpp)

target_link_libraries(jsontest json)
