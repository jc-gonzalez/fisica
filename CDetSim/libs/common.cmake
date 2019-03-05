#======================================================================
# CMakeLists.txt
# QPF - Prototype of QLA Processing Framework
# General Project File
#======================================================================
# Author: J C Gonzalez - 2015-2018
# Copyright (C) 2015-2018 Euclid SOC Team at ESAC
#======================================================================
cmake_minimum_required(VERSION 2.8.2)
cmake_policy (SET CMP0015 NEW)

set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

#===== Project libraries =====

set (TOPDIR ${CMAKE_SOURCE_DIR})
string(REGEX REPLACE "/libs$" "" TOPDIR "${TOPDIR}")
message("## TOPDIR is ${TOPDIR}")

set (FILEHDL_ROOT_DIR ${TOPDIR}/libs/filehdl)
set (INFIX_ROOT_DIR   ${TOPDIR}/libs/infix)
set (JSON_ROOT_DIR    ${TOPDIR}/libs/json)
set (LOG_ROOT_DIR     ${TOPDIR}/libs/log)
set (STR_ROOT_DIR     ${TOPDIR}/libs/str)
set (TOOLS_ROOT_DIR   ${TOPDIR}/libs/tools)
set (UUID_ROOT_DIR    ${TOPDIR}/libs/uuid)

#==== Common directives

link_directories (
  ${FILEHDL_ROOT_DIR}
  ${INFIX_ROOT_DIR}
  ${JSON_ROOT_DIR}
  ${LOG_ROOT_DIR}
  ${STR_ROOT_DIR}
  ${TOOLS_ROOT_DIR}
  ${UUID_ROOT_DIR}
)
