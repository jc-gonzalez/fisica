#=============================================================================
# CMakeLists.txt
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
cmake_minimum_required(VERSION 2.8)

project(infix)

set(PROJECT_NAME infix)

set(infix_MAJOR 0)
set(infix_MINOR 1)
set(infix_PATCH 0)

set(infix_VERSION ${infix_MAJOR}.${infix_MINOR}.${infix_PATCH})
message(STATUS "Building infix version ${infix_VERSION}")

add_subdirectory(src)
add_subdirectory(examples/ex1_infix)

set(CPACK_PACKAGE_VERSION_MAJOR ${infix_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${infix_MINOR})
set(CPACK_PACKAGE_VERSION_PATCH ${infix_PATCH})
set(CPACK_GENERATOR "TGZ")
set(CPACK_PACKAGING_INSTALL_PREFIX "")

include(CPack)

