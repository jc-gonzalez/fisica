#======================================================================
# CMakeLists.txt
# QPF - Prototype of QLA Processing Framework
# General Project File
#======================================================================
# Author: J C Gonzalez - 2015-2018
# Copyright (C) 2015-2018 Euclid SOC Team at ESAC
#======================================================================
cmake_minimum_required(VERSION 2.8)
#cmake_policy (SET CMP0015 OLD)

include (common.cmake)

project (libs)

#== Define subprojects

add_subdirectory (filehdl)
add_subdirectory (infix)
add_subdirectory (log)
add_subdirectory (str)
add_subdirectory (json)
add_subdirectory (tools)
add_subdirectory (mathtools)
#add_subdirectory (uuid)
#add_subdirectory (vos)
