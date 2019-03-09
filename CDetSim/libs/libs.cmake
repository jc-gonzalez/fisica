#======================================================================
# CMakeLists.txt
# QPF - Prototype of QLA Processing Framework
# General Project File
#======================================================================
# Copyright (C) 2019 J C Gonzalez
#======================================================================

#===== Project libraries =====

set (TOPDIR ${CMAKE_SOURCE_DIR})
string(REGEX REPLACE "/libs$" "" TOPDIR "${TOPDIR}")
message("## TOPDIR is ${TOPDIR}")

set (FILEHDL_ROOT_DIR    ${TOPDIR}/libs/filehdl)
set (INFIX_ROOT_DIR      ${TOPDIR}/libs/infix)
set (INIPARSER_ROOT_DIR  ${TOPDIR}/libs/iniparser)
set (JSON_ROOT_DIR       ${TOPDIR}/libs/json)
set (LOG_ROOT_DIR        ${TOPDIR}/libs/log)
set (MATHTOOLS_ROOT_DIR  ${TOPDIR}/libs/mathtools)
set (STR_ROOT_DIR        ${TOPDIR}/libs/str)
set (TOOLS_ROOT_DIR      ${TOPDIR}/libs/tools)
set (UUID_ROOT_DIR       ${TOPDIR}/libs/uuid)

#==== Common directives

link_directories (
  ${FILEHDL_ROOT_DIR}
  ${INFIX_ROOT_DIR}
  ${INIPARSER_ROOT_DIR}
  ${JSON_ROOT_DIR}
  ${LOG_ROOT_DIR}
  ${MATHTOOLS_ROOT_DIR}
  ${STR_ROOT_DIR}
  ${TOOLS_ROOT_DIR}
  ${UUID_ROOT_DIR}
)
