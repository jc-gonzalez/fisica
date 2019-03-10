#=============================================================================
# CMakeLists.txt
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
cmake_minimum_required(VERSION 2.8)

project(mathtools)

set(PROJECT_NAME mathtools)

set(mathtools_MAJOR 0)
set(mathtools_MINOR 1)
set(mathtools_PATCH 0)

set(mathtools_VERSION ${mathtools_MAJOR}.${mathtools_MINOR}.${mathtools_PATCH})
message(STATUS "Building mathtools version ${mathtools_VERSION}")

add_subdirectory(src)
add_subdirectory(examples/ex1_mathtools)

set(CPACK_PACKAGE_VERSION_MAJOR ${mathtools_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${mathtools_MINOR})
set(CPACK_PACKAGE_VERSION_PATCH ${mathtools_PATCH})
set(CPACK_GENERATOR "TGZ")
set(CPACK_PACKAGING_INSTALL_PREFIX "")

include(CPack)

