cmake_minimum_required(VERSION 3.1)
project(demo_rad Fortran)

#-----------------------------------------------------------------------------
#
# Set where the CCPP Framework lives
#
#-----------------------------------------------------------------------------
SET(TOY_ROOT "${CMAKE_SOURCE_DIR}")
SET(TOY_SRC "${TOY_ROOT}/src")

SET(CCPP_ROOT "${TOY_ROOT}/ccpp_framework")
SET(CCPP_FRAMEWORK ${CCPP_ROOT}/scripts)

################################################################################
# Configure CCPP
# The chemistry suite schemes
SET(SCHEME_METADATA "${TOY_SRC}/musica_schemes.txt")
SET(HOST_METADATA "${TOY_SRC}/model.meta,${TOY_SRC}/demo_rad_data.meta")
SET(SUITE_FILES "${TOY_SRC}/toy_suite.xml")
# HOST is the name of the executable we will build.
SET(HOST "${CMAKE_PROJECT_NAME}")
# By default, no verbose output
SET(VERBOSITY 0 CACHE STRING "Verbosity level of output (default: 0)")
# By default, generated caps go in ccpp subdir
SET(CCPP_CAP_FILES "${CMAKE_BINARY_DIR}/ccpp" CACHE
  STRING "Location of CCPP-generated cap files")

SET(CCPP_FRAMEWORK ${CCPP_ROOT}/scripts)

# Run CCPP Framework
SET(CAPGEN_CMD "${CCPP_FRAMEWORK}/ccpp_capgen.py")
LIST(APPEND CAPGEN_CMD "--host-files")
LIST(APPEND CAPGEN_CMD "${HOST_METADATA}")
LIST(APPEND CAPGEN_CMD "--scheme-files")
LIST(APPEND CAPGEN_CMD "${SCHEME_METADATA}")
LIST(APPEND CAPGEN_CMD "--suites")
LIST(APPEND CAPGEN_CMD "${SUITE_FILES}")
LIST(APPEND CAPGEN_CMD "--output-root")
LIST(APPEND CAPGEN_CMD "${CCPP_CAP_FILES}")
LIST(APPEND CAPGEN_CMD "--host-name")
LIST(APPEND CAPGEN_CMD "${HOST}")
while (VERBOSITY GREATER 0)
  LIST(APPEND CAPGEN_CMD "--verbose")
  MATH(EXPR VERBOSITY "${VERBOSITY} - 1")
endwhile ()
string(REPLACE ";" " " CAPGEN_STRING "${CAPGEN_CMD}")
MESSAGE(STATUS "Running: ${CAPGEN_STRING}")
EXECUTE_PROCESS(COMMAND ${CAPGEN_CMD} WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  OUTPUT_VARIABLE CAPGEN_OUT ERROR_VARIABLE CAPGEN_OUT RESULT_VARIABLE RES)
MESSAGE(STATUS "${CAPGEN_OUT}")
if (RES EQUAL 0)
  MESSAGE(STATUS "CCPP cap generation completed")
else(RES EQUAL 0)
  MESSAGE(FATAL_ERROR "CCPP cap generation FAILED: result = ${RES}")
endif(RES EQUAL 0)

# Grab the CCPP generated files
set(DTABLE_CMD "${CCPP_FRAMEWORK}/ccpp_datafile.py")
list(APPEND DTABLE_CMD "${CCPP_CAP_FILES}/datatable.xml")
list(APPEND DTABLE_CMD "--ccpp-files")
list(APPEND DTABLE_CMD "--separator=\\;")
string(REPLACE ";" " " DTABLE_STRING "${DTABLE_CMD}")
MESSAGE(STATUS "Running: ${DTABLE_STRING}")
EXECUTE_PROCESS(COMMAND ${DTABLE_CMD} OUTPUT_VARIABLE CCPP_CAPS
                RESULT_VARIABLE RES
                OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_STRIP_TRAILING_WHITESPACE)
if (RES EQUAL 0)
  MESSAGE(STATUS "CCPP cap files retrieved")
else(RES EQUAL 0)
  MESSAGE(FATAL_ERROR "CCPP cap file retrieval FAILED: result = ${RES}")
endif(RES EQUAL 0)
FOREACH(FILE ${CCPP_CAPS})
  LIST(APPEND LIBRARY_LIST ${FILE})
ENDFOREACH(FILE)

################################################################################
# toy model with radiation
file(GLOB DEMO_SRCS src/demo_rad_data.F90 src/model.F90
                    src/musica_aerosol.F90
                    src/musica_aerosol_modal.F90
                    src/musica_aerosol_model.F90
                    src/musica_radiation.F90
                    src/musica_aerosol_sectional.F90
                    src/wavelength_grid.F90)

add_executable(demo ${DEMO_SRCS} ${LIBRARY_LIST})

set_target_properties(demo PROPERTIES OUTPUT_NAME "${HOST}")

################################################################################
