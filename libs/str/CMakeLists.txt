#=============================================================================
# CMakeLists.txt
#
# Copyright (C) 2015, J C Gonzalez
#
# This file is part of str
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

project(str)

set(PROJECT_NAME str)

set(str_MAJOR 0)
set(str_MINOR 1)
set(str_PATCH 0)

set(str_VERSION ${str_MAJOR}.${str_MINOR}.${str_PATCH})
message(STATUS "Building str version ${str_VERSION}")

add_subdirectory(src)
add_subdirectory(examples/ex1_str)

set(CPACK_PACKAGE_VERSION_MAJOR ${str_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${str_MINOR})
set(CPACK_PACKAGE_VERSION_PATCH ${str_PATCH})
set(CPACK_GENERATOR "TGZ")
set(CPACK_PACKAGING_INSTALL_PREFIX "")

include(CPack)

