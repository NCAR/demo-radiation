! abstract aerosol as determined by the aerosol API effort
module musica_aerosol_modal

  implicit none
  private

  public :: aerosol_t

  ! an aerosol state and diagnostics
  type, abstract :: aerosol_t
  contains
    procedure, private :: get_optics_grid
    procedure, private :: get_optics_sample
    generic :: get_optics => get_optics_grid, get_optics_sample
  end type aerosol_modal_t

  ! calculator of aerosol optical properties for a particular wavelength grid
  type, abstract :: material_optics_grid_t
  contains
    procedure :: optical_depth => grid_optical_depth
    procedure :: scattering_optical_depth => grid_scattering_optical_depth
    procedure :: forward_scattering_optical_depth =>                          &
                     grid_forward_scattering_optical_depth
  end type material_optics_grid_t

  ! calculator of aerosol optical properties for a particular wavelength
  ! sample
  type, abstract :: material_optics_sample_t
  contains
    procedure :: optical_depth => sample_optical_depth
    procedure :: scattering_optical_depth => sample_scattering_optical_depth
    procedure :: forward_scattering_optical_depth =>                          &
                   sample_forward_scattering_optical_depth
  end type material_optics_sample_t

interface

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! creates an optics object for this aerosol for the specified wavelength
  ! grid
  function get_optics_grid( this, grid ) result( optics )
    use musica_wavelength_grid,               only : wavelength_grid_t
    import aerosol_t, material_optics_grid_t
    class( material_optics_grid_t ), pointer      :: optics
    class( aerosol_t ),              intent( in ) :: this
    class( wavelength_grid_t ),      intent( in ) :: grid
  end function get_optics_grid

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! creates an optics object for this aerosol for the specified wavelength
  ! sample
  function get_optics_sample( this, wavelength__m ) result( optics )
    import aerosol_t, material_optics_sample_t
    class( material_optics_sample_t ), pointer      :: optics
    class( aerosol_t ),                intent( in ) :: this
    real,                              intent( in ) :: wavelength__m
  end function get_optics_sample

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! calculates optical depth for the aerosol on the specified grid
  subroutine grid_optical_depth( this, aerosol, optical_depths )
    import aerosol_t, material_optics_grid_t
    class( material_optics_grid_t ), intent( in )  :: this
    class( aerosol_t ),              intent( in )  :: aerosol
    real( : ),                       intent( out ) :: optical_depths
  end subroutine grid_optical_depth

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! calculates scattering optical depth for the aerosol on the specified grid
  subroutine grid_scattering_optical_depth( this, aerosol, optical_depths )
    import aerosol_t, material_optics_grid_t
    class( material_optics_grid_t ), intent( in )  :: this
    class( aerosol_t ),              intent( in )  :: aerosol
    real( : ),                       intent( out ) :: optical_depths
  end subroutine grid_scattering_optical_depth

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! calculates forward scattering optical depth for the aerosol on the
  ! specified grid
  subroutine grid_forward_scattering_optical_depth( this, aerosol,            &
      optical_depths )
    import aerosol_t, material_optics_grid_t
    class( material_optics_grid_t ), intent( in )  :: this
    class( aerosol_t ),              intent( in )  :: aerosol
    real( : ),                       intent( out ) :: optical_depths
  end subroutine grid_forward_scattering_optical_depth

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! calculates optical depth for the aerosol for the specified wavelength
  ! sample
  subroutine sample_optical_depth( this, aerosol, optical_depth )
    import aerosol_t, material_optics_sample_t
    class( material_optics_sample_t ), intent( in )  :: this
    class( aerosol_t ),                intent( in )  :: aerosol
    real,                              intent( out ) :: optical_depth
  end subroutine sample_optical_depth

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! calculates scattering optical depth for the aerosol for the specified
  ! wavelength sample
  subroutine sample_scattering_optical_depth( this, aerosol, optical_depth )
    import aerosol_t, material_optics_sample_t
    class( material_optics_sample_t ), intent( in )  :: this
    class( aerosol_t ),                intent( in )  :: aerosol
    real,                              intent( out ) :: optical_depth
  end subroutine sample_scattering_optical_depth

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! calculates forward scattering optical depth for the aerosol for the
  ! specified wavelength sample
  subroutine sample_forward_scattering_optical_depth( this, aerosol,          &
      optical_depth )
    import aerosol_t, material_optics_sample_t
    class( material_optics_sample_t ), intent( in )  :: this
    class( aerosol_t ),                intent( in )  :: aerosol
    real,                              intent( out ) :: optical_depth
  end subroutine sample_forward_scattering_optical_depth

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end interface

end module musica_aerosol
