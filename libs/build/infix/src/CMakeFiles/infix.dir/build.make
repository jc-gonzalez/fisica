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
include infix/src/CMakeFiles/infix.dir/depend.make

# Include the progress variables for this target.
include infix/src/CMakeFiles/infix.dir/progress.make

# Include the compile flags for this target's objects.
include infix/src/CMakeFiles/infix.dir/flags.make

infix/src/CMakeFiles/infix.dir/infix.cpp.o: infix/src/CMakeFiles/infix.dir/flags.make
infix/src/CMakeFiles/infix.dir/infix.cpp.o: ../infix/src/infix.cpp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/home/jcgonzalez/JC1/fisica.git/libs2/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building CXX object infix/src/CMakeFiles/infix.dir/infix.cpp.o"
	cd /home/jcgonzalez/JC1/fisica.git/libs2/build/infix/src && /usr/bin/c++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/infix.dir/infix.cpp.o -c /home/jcgonzalez/JC1/fisica.git/libs2/infix/src/infix.cpp

infix/src/CMakeFiles/infix.dir/infix.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/infix.dir/infix.cpp.i"
	cd /home/jcgonzalez/JC1/fisica.git/libs2/build/infix/src && /usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /home/jcgonzalez/JC1/fisica.git/libs2/infix/src/infix.cpp > CMakeFiles/infix.dir/infix.cpp.i

infix/src/CMakeFiles/infix.dir/infix.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/infix.dir/infix.cpp.s"
	cd /home/jcgonzalez/JC1/fisica.git/libs2/build/infix/src && /usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /home/jcgonzalez/JC1/fisica.git/libs2/infix/src/infix.cpp -o CMakeFiles/infix.dir/infix.cpp.s

infix/src/CMakeFiles/infix.dir/infix.cpp.o.requires:

.PHONY : infix/src/CMakeFiles/infix.dir/infix.cpp.o.requires

infix/src/CMakeFiles/infix.dir/infix.cpp.o.provides: infix/src/CMakeFiles/infix.dir/infix.cpp.o.requires
	$(MAKE) -f infix/src/CMakeFiles/infix.dir/build.make infix/src/CMakeFiles/infix.dir/infix.cpp.o.provides.build
.PHONY : infix/src/CMakeFiles/infix.dir/infix.cpp.o.provides

infix/src/CMakeFiles/infix.dir/infix.cpp.o.provides.build: infix/src/CMakeFiles/infix.dir/infix.cpp.o


# Object files for target infix
infix_OBJECTS = \
"CMakeFiles/infix.dir/infix.cpp.o"

# External object files for target infix
infix_EXTERNAL_OBJECTS =

infix/src/libinfix.so.0.1.0: infix/src/CMakeFiles/infix.dir/infix.cpp.o
infix/src/libinfix.so.0.1.0: infix/src/CMakeFiles/infix.dir/build.make
infix/src/libinfix.so.0.1.0: infix/src/CMakeFiles/infix.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/home/jcgonzalez/JC1/fisica.git/libs2/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking CXX shared library libinfix.so"
	cd /home/jcgonzalez/JC1/fisica.git/libs2/build/infix/src && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/infix.dir/link.txt --verbose=$(VERBOSE)
	cd /home/jcgonzalez/JC1/fisica.git/libs2/build/infix/src && $(CMAKE_COMMAND) -E cmake_symlink_library libinfix.so.0.1.0 libinfix.so.0.1.0 libinfix.so

infix/src/libinfix.so: infix/src/libinfix.so.0.1.0
	@$(CMAKE_COMMAND) -E touch_nocreate infix/src/libinfix.so

# Rule to build all files generated by this target.
infix/src/CMakeFiles/infix.dir/build: infix/src/libinfix.so

.PHONY : infix/src/CMakeFiles/infix.dir/build

infix/src/CMakeFiles/infix.dir/requires: infix/src/CMakeFiles/infix.dir/infix.cpp.o.requires

.PHONY : infix/src/CMakeFiles/infix.dir/requires

infix/src/CMakeFiles/infix.dir/clean:
	cd /home/jcgonzalez/JC1/fisica.git/libs2/build/infix/src && $(CMAKE_COMMAND) -P CMakeFiles/infix.dir/cmake_clean.cmake
.PHONY : infix/src/CMakeFiles/infix.dir/clean

infix/src/CMakeFiles/infix.dir/depend:
	cd /home/jcgonzalez/JC1/fisica.git/libs2/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/jcgonzalez/JC1/fisica.git/libs2 /home/jcgonzalez/JC1/fisica.git/libs2/infix/src /home/jcgonzalez/JC1/fisica.git/libs2/build /home/jcgonzalez/JC1/fisica.git/libs2/build/infix/src /home/jcgonzalez/JC1/fisica.git/libs2/build/infix/src/CMakeFiles/infix.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : infix/src/CMakeFiles/infix.dir/depend
