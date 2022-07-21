cmake_minimum_required(VERSION 3.0)

project(otps2 LANGUAGES Fortran)

# Specify position-independent code since we will build a shared library.
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

include_directories(src/OTPS2)

add_library(otps2_ SHARED src/OTPS2/constit.h src/OTPS2/derived_types.h src/otps2_wrapper.f90)

# Find target Python interpreter.
set(Python_FIND_STRATEGY LOCATION)
set(Python3_FIND_STRATEGY LOCATION)
find_package(Python3 COMPONENTS Interpreter)

# Build wheel
add_custom_command(OUTPUT otps2_wheel.stamp
                   COMMAND ${CMAKE_COMMAND} -E copy_directory "${CMAKE_CURRENT_SOURCE_DIR}/otps2" otps2
                   COMMAND ${CMAKE_COMMAND} -E copy "$<TARGET_FILE:otps2_>" otps2/
                   COMMAND ${Python3_EXECUTABLE} "${CMAKE_CURRENT_SOURCE_DIR}/setup.py" bdist_wheel
                   VERBATIM)
add_custom_target(otps2_wheel ALL DEPENDS otps2_wheel.stamp)

# Let pip install the wheel
install(CODE "execute_process(COMMAND ${Python3_EXECUTABLE} -m pip install --no-index --find-links=${CMAKE_CURRENT_BINARY_DIR}/dist --user --force-reinstall --upgrade otps2 WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/dist)")