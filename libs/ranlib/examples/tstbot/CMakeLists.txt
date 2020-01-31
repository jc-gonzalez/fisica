#=============================================================================
# CMakeLists.txt
# Example - tstbot
#=============================================================================

link_directories(${ranlib_BINARY_DIR}/src)

add_executable(tstbot tstbot.c)
target_include_directories (tstbot PUBLIC . ../../src)
target_link_libraries(tstbot ranlib)
