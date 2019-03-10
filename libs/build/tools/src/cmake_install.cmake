# Install script for directory: /home/jcgonzalez/JC1/fisica.git/libs2/tools/src

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "/home/jcgonzalez/opt")
endif()
string(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
if(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  if(BUILD_TYPE)
    string(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  else()
    set(CMAKE_INSTALL_CONFIG_NAME "Debug")
  endif()
  message(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
endif()

# Set the component getting installed.
if(NOT CMAKE_INSTALL_COMPONENT)
  if(COMPONENT)
    message(STATUS "Install component: \"${COMPONENT}\"")
    set(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  else()
    set(CMAKE_INSTALL_COMPONENT)
  endif()
endif()

# Install shared libraries without execute permission?
if(NOT DEFINED CMAKE_INSTALL_SO_NO_EXE)
  set(CMAKE_INSTALL_SO_NO_EXE "1")
endif()

# Is this installation the result of a crosscompile?
if(NOT DEFINED CMAKE_CROSSCOMPILING)
  set(CMAKE_CROSSCOMPILING "FALSE")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  foreach(file
      "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libtools.so.0.1.0"
      "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libtools.so"
      )
    if(EXISTS "${file}" AND
       NOT IS_SYMLINK "${file}")
      file(RPATH_CHECK
           FILE "${file}"
           RPATH "")
    endif()
  endforeach()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib" TYPE SHARED_LIBRARY FILES
    "/home/jcgonzalez/JC1/fisica.git/libs2/build/tools/src/libtools.so.0.1.0"
    "/home/jcgonzalez/JC1/fisica.git/libs2/build/tools/src/libtools.so"
    )
  foreach(file
      "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libtools.so.0.1.0"
      "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libtools.so"
      )
    if(EXISTS "${file}" AND
       NOT IS_SYMLINK "${file}")
      if(CMAKE_INSTALL_DO_STRIP)
        execute_process(COMMAND "/usr/bin/strip" "${file}")
      endif()
    endif()
  endforeach()
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include" TYPE FILE FILES
    "/home/jcgonzalez/JC1/fisica.git/libs2/tools/src/alert.h"
    "/home/jcgonzalez/JC1/fisica.git/libs2/tools/src/dbg.h"
    "/home/jcgonzalez/JC1/fisica.git/libs2/tools/src/dwatcher.h"
    "/home/jcgonzalez/JC1/fisica.git/libs2/tools/src/filetools.h"
    "/home/jcgonzalez/JC1/fisica.git/libs2/tools/src/launcher.h"
    "/home/jcgonzalez/JC1/fisica.git/libs2/tools/src/metadatareader.h"
    "/home/jcgonzalez/JC1/fisica.git/libs2/tools/src/process.h"
    "/home/jcgonzalez/JC1/fisica.git/libs2/tools/src/propdef.h"
    "/home/jcgonzalez/JC1/fisica.git/libs2/tools/src/scopeexit.h"
    "/home/jcgonzalez/JC1/fisica.git/libs2/tools/src/sm.h"
    "/home/jcgonzalez/JC1/fisica.git/libs2/tools/src/timer.h"
    "/home/jcgonzalez/JC1/fisica.git/libs2/tools/src/tools.h"
    )
endif()

