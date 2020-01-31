#=============================================================================
# CMakeLists.txt
# Example - tstmid
#=============================================================================

link_directories(${ranlib_BINARY_DIR}/src)

add_executable(tstmid tstmid.c)
target_include_directories (tstmid PUBLIC . ../../src)
target_link_libraries(tstmid ranlib)
