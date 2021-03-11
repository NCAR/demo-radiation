! the radiation scheme
module musica_radiation

  use musica_aerosol,                  only : aerosol_t,                      &
                                              material_optics_grid_t,         &
                                              material_optics_sample_t

  implicit none
  private

  ! obviously not thread safe
  class(material_optics_grid_t), pointer :: grid_optics
  class(material_optics_sample_t), pointer :: optics_550nm
  real, allocatable(:) :: optical_depths_on_grid
  real :: scattering_optical_depth_550nm

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! this would be the CCPP initialize function for radiation
  subroutine radiation_initialize( aerosol )
    class(aerosol_t), intent(in) :: aerosol
    type(wavelength_grid_t) :: grid

    ! create the grid (with more options for setting grid dimensions)
    grid = wavelength_grid_t( )

    allocate( optical_depths_on_grid( grid%number_of_bins( ) ) )

    ! get the optical depth calculators
    grid_optics => aerosol%get_optics( grid )
    optics_550nm => aerosol%get_optics( 550.0e-9 )
  end subroutine radiation_initialize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! this would be the CCPP run function for radiation
  subroutine radiation_run( aerosol )
    class(aerosol_t), pointer :: aerosol

    ! calculate the optical depths
    call grid_optics%optical_depth( optical_depths_on_grid )
    call optics_550nm%scattering_optical_depth( scattering_optical_depth_550nm )

    write(*,*) "grid AOD", optical_depths_on_grid
    write(*,*) "scattering AOD at 550 nm", scattering_optical_depth_550nm
  end subroutine radiation_run

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module musica_radiation
