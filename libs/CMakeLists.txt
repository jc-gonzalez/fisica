#======================================================================
# CMakeLists.txt
# General Project File
#======================================================================
# Copyright (C) 2019 by J C Gonzalez
#======================================================================
cmake_minimum_required(VERSION 2.8)
cmake_policy (SET CMP0015 NEW)

include (common.cmake)

project (libs)

#== Define subprojects

add_subdirectory (filetools)
add_subdirectory (infix)
add_subdirectory (iniparser)
add_subdirectory (json)
add_subdirectory (log)
add_subdirectory (chimg)
add_subdirectory (mathtools)
add_subdirectory (str)
add_subdirectory (tools)
add_subdirectory (httpsc)
#add_subdirectory (uuid)
add_subdirectory (vos)

add_subdirectory (ranlib)

