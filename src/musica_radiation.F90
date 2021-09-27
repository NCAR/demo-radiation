! the radiation scheme
module musica_radiation

  use musica_aerosol,                  only : aerosol_t,                      &
                                              material_optics_grid_t,         &
                                              material_optics_sample_t
  use musica_wavelength_grid,          only : wavelength_grid_t

  implicit none
  private

  public :: musica_radiation_initialize
  public :: musica_radiation_run

  ! obviously not thread safe
  class( material_optics_grid_t ), pointer :: grid_optics
  class( material_optics_sample_t ), pointer :: optics_550nm
  real, allocatable :: optical_depths_on_grid( : )
  real :: scattering_optical_depth_550nm

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! this would be the CCPP initialize function for radiation
  !> \section arg_table_musica_radiation_initialize  Argument Table
  !! \htmlinclude musica_radiation_initialize.html
  subroutine musica_radiation_initialize(aerosol, errcode, errmsg)
    ! Dummy arguments
    class(aerosol_t),   intent(in)  :: aerosol
    integer,            intent(out) :: errcode
    character(len=512), intent(out) :: errmsg
    ! Local variable
    type( wavelength_grid_t ) :: grid

    ! create the grid (with more options for setting grid dimensions)
    grid = wavelength_grid_t( )

    allocate( optical_depths_on_grid( grid%number_of_bins( ) ) )

    ! get the optical depth calculators
    grid_optics => aerosol%get_optics( grid )
    optics_550nm => aerosol%get_optics( 550.0e-9 )
  end subroutine musica_radiation_initialize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! this would be the CCPP run function for radiation
  !> \section arg_table_musica_radiation_run  Argument Table
  !! \htmlinclude musica_radiation_run.html
  subroutine musica_radiation_run(aerosol, errcode, errmsg)
    class(aerosol_t),   intent(in)  :: aerosol
    integer,            intent(out) :: errcode
    character(len=512), intent(out) :: errmsg

    ! calculate the optical depths
    call grid_optics%optical_depth( aerosol, optical_depths_on_grid )
    call optics_550nm%scattering_optical_depth( aerosol,                      &
            scattering_optical_depth_550nm )

    write(6, *) "grid AOD", optical_depths_on_grid
    write(6, *) "scattering AOD at 550 nm", scattering_optical_depth_550nm
  end subroutine musica_radiation_run

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module musica_radiation
