# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.10

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:


#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:


# Remove some rules from gmake that .SUFFIXES does not remove.
SUFFIXES =

.SUFFIXES: .hpux_make_needs_suffix_list


# Suppress display of executed commands.
$(VERBOSE).SILENT:


# A target that is always out of date.
cmake_force:

.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /usr/bin/cmake

# The command to remove a file.
RM = /usr/bin/cmake -E remove -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /home/jcgonzalez/JC1/fisica.git/libs2

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /home/jcgonzalez/JC1/fisica.git/libs2/build

# Include any dependencies generated for this target.
include iniparser/src/CMakeFiles/iniparser.dir/depend.make

# Include the progress variables for this target.
include iniparser/src/CMakeFiles/iniparser.dir/progress.make

# Include the compile flags for this target's objects.
include iniparser/src/CMakeFiles/iniparser.dir/flags.make

iniparser/src/CMakeFiles/iniparser.dir/iniparser.cpp.o: iniparser/src/CMakeFiles/iniparser.dir/flags.make
iniparser/src/CMakeFiles/iniparser.dir/iniparser.cpp.o: ../iniparser/src/iniparser.cpp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/home/jcgonzalez/JC1/fisica.git/libs2/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building CXX object iniparser/src/CMakeFiles/iniparser.dir/iniparser.cpp.o"
	cd /home/jcgonzalez/JC1/fisica.git/libs2/build/iniparser/src && /usr/bin/c++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/iniparser.dir/iniparser.cpp.o -c /home/jcgonzalez/JC1/fisica.git/libs2/iniparser/src/iniparser.cpp

iniparser/src/CMakeFiles/iniparser.dir/iniparser.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/iniparser.dir/iniparser.cpp.i"
	cd /home/jcgonzalez/JC1/fisica.git/libs2/build/iniparser/src && /usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /home/jcgonzalez/JC1/fisica.git/libs2/iniparser/src/iniparser.cpp > CMakeFiles/iniparser.dir/iniparser.cpp.i

iniparser/src/CMakeFiles/iniparser.dir/iniparser.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/iniparser.dir/iniparser.cpp.s"
	cd /home/jcgonzalez/JC1/fisica.git/libs2/build/iniparser/src && /usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /home/jcgonzalez/JC1/fisica.git/libs2/iniparser/src/iniparser.cpp -o CMakeFiles/iniparser.dir/iniparser.cpp.s

iniparser/src/CMakeFiles/iniparser.dir/iniparser.cpp.o.requires:

.PHONY : iniparser/src/CMakeFiles/iniparser.dir/iniparser.cpp.o.requires

iniparser/src/CMakeFiles/iniparser.dir/iniparser.cpp.o.provides: iniparser/src/CMakeFiles/iniparser.dir/iniparser.cpp.o.requires
	$(MAKE) -f iniparser/src/CMakeFiles/iniparser.dir/build.make iniparser/src/CMakeFiles/iniparser.dir/iniparser.cpp.o.provides.build
.PHONY : iniparser/src/CMakeFiles/iniparser.dir/iniparser.cpp.o.provides

iniparser/src/CMakeFiles/iniparser.dir/iniparser.cpp.o.provides.build: iniparser/src/CMakeFiles/iniparser.dir/iniparser.cpp.o


# Object files for target iniparser
iniparser_OBJECTS = \
"CMakeFiles/iniparser.dir/iniparser.cpp.o"

# External object files for target iniparser
iniparser_EXTERNAL_OBJECTS =

iniparser/src/libiniparser.so.0.1.0: iniparser/src/CMakeFiles/iniparser.dir/iniparser.cpp.o
iniparser/src/libiniparser.so.0.1.0: iniparser/src/CMakeFiles/iniparser.dir/build.make
iniparser/src/libiniparser.so.0.1.0: iniparser/src/CMakeFiles/iniparser.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/home/jcgonzalez/JC1/fisica.git/libs2/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking CXX shared library libiniparser.so"
	cd /home/jcgonzalez/JC1/fisica.git/libs2/build/iniparser/src && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/iniparser.dir/link.txt --verbose=$(VERBOSE)
	cd /home/jcgonzalez/JC1/fisica.git/libs2/build/iniparser/src && $(CMAKE_COMMAND) -E cmake_symlink_library libiniparser.so.0.1.0 libiniparser.so.0.1.0 libiniparser.so

iniparser/src/libiniparser.so: iniparser/src/libiniparser.so.0.1.0
	@$(CMAKE_COMMAND) -E touch_nocreate iniparser/src/libiniparser.so

# Rule to build all files generated by this target.
iniparser/src/CMakeFiles/iniparser.dir/build: iniparser/src/libiniparser.so

.PHONY : iniparser/src/CMakeFiles/iniparser.dir/build

iniparser/src/CMakeFiles/iniparser.dir/requires: iniparser/src/CMakeFiles/iniparser.dir/iniparser.cpp.o.requires

.PHONY : iniparser/src/CMakeFiles/iniparser.dir/requires

iniparser/src/CMakeFiles/iniparser.dir/clean:
	cd /home/jcgonzalez/JC1/fisica.git/libs2/build/iniparser/src && $(CMAKE_COMMAND) -P CMakeFiles/iniparser.dir/cmake_clean.cmake
.PHONY : iniparser/src/CMakeFiles/iniparser.dir/clean

iniparser/src/CMakeFiles/iniparser.dir/depend:
	cd /home/jcgonzalez/JC1/fisica.git/libs2/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/jcgonzalez/JC1/fisica.git/libs2 /home/jcgonzalez/JC1/fisica.git/libs2/iniparser/src /home/jcgonzalez/JC1/fisica.git/libs2/build /home/jcgonzalez/JC1/fisica.git/libs2/build/iniparser/src /home/jcgonzalez/JC1/fisica.git/libs2/build/iniparser/src/CMakeFiles/iniparser.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : iniparser/src/CMakeFiles/iniparser.dir/depend
