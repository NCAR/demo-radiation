cmake_minimum_required(VERSION 3.1)
project(demo-radiation Fortran)

################################################################################
# toy model with radiation
file(GLOB DEMO_SRCS src/aerosol.F90
                    src/aerosol_modal.F90
                    src/model.F90
                    src/radiation.F90
                    src/wavelength_grid.F90)

add_executable(demo ${DEMO_SRCS})

set_target_properties(demo PROPERTIES OUTPUT_NAME demo)

################################################################################
