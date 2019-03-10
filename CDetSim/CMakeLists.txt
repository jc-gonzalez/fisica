#=============================================================================
# CMakeLists.txt
#
# Copyright (C) 2015, J C Gonzalez
#
# This file is part of cdetsim
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
cmake_policy (SET CMP0015 NEW)

include (common.cmake)

project(cdetsim)

set(PROJECT_NAME cdetsim)

#== Get project version
execute_process(
  COMMAND awk -F\" "/APP_RELEASE/{print $2;}"
  WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
  INPUT_FILE ./version.h
  OUTPUT_VARIABLE VERSION
  OUTPUT_STRIP_TRAILING_WHITESPACE)
set(cdetsim_VERSION ${VERSION})

#== Get compilation date
execute_process(
  COMMAND date +%Y%m%d%H%M%S
  OUTPUT_VARIABLE DATE
  OUTPUT_STRIP_TRAILING_WHITESPACE)

#== Get BUILD_ID
set (BUILD_ID $ENV{BUILD_ID})
#if ("${BUILD_ID}" STREQUAL "")
#  execute_process(
#    COMMAND git log --pretty=format:'%h' -n 1
#    WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
#    OUTPUT_VARIABLE COMMIT_NUMBER
#    OUTPUT_STRIP_TRAILING_WHITESPACE)
#  set (BUILD_ID ${DATE}_${COMMIT_NUMBER})
#  message (STATUS "Build id generated from git info")
#endif()

add_definitions(-DBUILD_ID="${BUILD_ID}")

include_directories(. $ENV{HOME}/opt/include)
link_directories($ENV{HOME}/opt/lib)

message ("Building ${CMAKE_PROJECT_NAME} version ${VERSION}, with build id. ${BUILD_ID}")

#==== Coverage

if (COV)
  message ("Coverage will be computed . . .")
  set(GCC_COVERAGE_COMPILE_FLAGS "-g -O0 -coverage -fprofile-arcs -ftest-coverage")
  set(GCC_COVERAGE_LINK_FLAGS    "-coverage -lgcov -fprofile-arcs -ftest-coverage")
  set(CMAKE_CXX_FLAGS  "${CMAKE_CXX_FLAGS} ${GCC_COVERAGE_COMPILE_FLAGS}" )
  set(CMAKE_EXE_LINKER_FLAGS  "${CMAKE_EXE_LINKER_FLAGS} ${GCC_COVERAGE_LINK_FLAGS}" )
endif()

#== Define subprojects

add_subdirectory (src)

if (HMI)
  message ("HMI component will be built . . .")
  add_subdirectory (qpfhmi)
endif()

if (TESTS)
  message ("Unitary tests will be compiled . . .")
  add_subdirectory (tests)
endif()

if (SAMPLES)
  message ("Samples will be generated . . .")
  add_subdirectory (sample)
endif()

#== CPack configuration
set (CPACK_PACKAGE_VERSION_MAJOR ${QPF_MAJOR})
set (CPACK_PACKAGE_VERSION_MINOR ${QPF_MINOR})
set (CPACK_PACKAGE_VERSION_PATCH ${QPF_PATCH})
set (CPACK_GENERATOR "TGZ")
set (CPACK_PACKAGING_INSTALL_PREFIX "")
include (CPack)

