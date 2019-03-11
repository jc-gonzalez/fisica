#=============================================================================
# CMakeLists.txt
# Example - tstgmn
#=============================================================================

link_directories(${ranlib_BINARY_DIR}/src)

add_executable(tstgmn tstgmn.c)
target_include_directories (tstgmn PUBLIC . ../../src)
target_link_libraries(tstgmn ranlib)
