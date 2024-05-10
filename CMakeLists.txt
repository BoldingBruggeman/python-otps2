cmake_minimum_required(VERSION 3.15)

project(otps2 LANGUAGES Fortran)

# Specify position-independent code since we will build a shared library.
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

add_library(otps2_ SHARED src/OTPS2/constit.h src/OTPS2/derived_types.h src/otps2_wrapper.f90)
target_include_directories(otps2_ PRIVATE src/OTPS2)
if (MSVC)
  target_compile_options(otps2_ PRIVATE /libs:static)
endif()

# Find target Python interpreter.
find_package(Python3 COMPONENTS Interpreter)

# Build wheel
add_custom_command(OUTPUT otps2_wheel.stamp
                   COMMAND ${CMAKE_COMMAND} -E copy_directory "${CMAKE_CURRENT_SOURCE_DIR}/otps2" otps2
                   COMMAND ${CMAKE_COMMAND} -E copy "$<TARGET_FILE:otps2_>" otps2/
                   COMMAND ${Python3_EXECUTABLE} "${CMAKE_CURRENT_SOURCE_DIR}/setup.py" bdist_wheel
                   VERBATIM)
add_custom_target(otps2_wheel ALL DEPENDS otps2_wheel.stamp)

# Let pip install the wheel
install(CODE "execute_process(COMMAND ${Python3_EXECUTABLE} -m pip install --no-index --find-links=${CMAKE_CURRENT_BINARY_DIR}/dist --force-reinstall --upgrade otps2 WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/dist)")