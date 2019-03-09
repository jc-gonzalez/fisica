#======================================================================
# common.cmake
# Common settings for libraries
#======================================================================
# Copyright (C) 2019 J C Gonzalez
#======================================================================
cmake_minimum_required(VERSION 2.8)
cmake_policy (SET CMP0015 NEW)

set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

if (NOT DEFINED LIBS_DIR)
  set (LIBS_DIR "${CMAKE_SOURCE_DIR}/libs")
endif()

include (${LIBS_DIR}/libs.cmake)
