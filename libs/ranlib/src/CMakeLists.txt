#=============================================================================
# CMakeLists.txt
# src directory
#=============================================================================

SET (ranlib_src
  com.c
  ranlib.c
  linpack.c
  )

SET (ranlib_hdr
  ranlib.h
  )

SET (ranlib_inthdr
  )

ADD_LIBRARY (ranlib SHARED ${ranlib_src} )
include_directories (.)

SET_TARGET_PROPERTIES (ranlib PROPERTIES 
  LINKER_LANGUAGE C
  SOVERSION ${ranlib_VERSION}
  VERSION   ${ranlib_VERSION})

INSTALL (TARGETS ranlib
  RUNTIME DESTINATION bin
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib)

INSTALL (FILES ${ranlib_hdr}
  DESTINATION include)

