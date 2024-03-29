! a sectional aerosol module
module musica_aerosol_sectional

  use ccpp_kinds,                      only : kind_phys
  use musica_aerosol,                  only : aerosol_t,                      &
                                              material_optics_grid_t,         &
                                              material_optics_sample_t
  use musica_wavelength_grid,          only : wavelength_grid_t

  implicit none
  private

  public :: aerosol_sectional_t

  ! a sectional aerosol state and diagnostics
  type, extends( aerosol_t ) :: aerosol_sectional_t
    integer           :: number_of_sections_
    real(kind_phys), allocatable :: state_(:) ! stand-in for mass, number, etc.
  contains
    procedure, private :: get_optics_grid
    procedure, private :: get_optics_sample
  end type aerosol_sectional_t

  ! calculator of aerosol optical properties for a particular wavelength grid
  type, extends( material_optics_grid_t ) :: material_optics_grid_sectional_t
    ! any parameters needed to calculate optics at run-time
    integer :: my_parameter_
    type( wavelength_grid_t ) :: grid_
  contains
    procedure :: optical_depth => grid_optical_depth
    procedure :: scattering_optical_depth => grid_scattering_optical_depth
    procedure :: forward_scattering_optical_depth =>                          &
                     grid_forward_scattering_optical_depth
  end type material_optics_grid_sectional_t

  ! calculator of aerosol optical properties for a particular wavelength
  ! sample
  type, extends( material_optics_sample_t ) :: material_optics_sample_sectional_t
    ! any parameters needed to calculate optics at run-time
    integer :: my_parameter_
    real :: wavelength__m_
  contains
    procedure :: optical_depth => sample_optical_depth
    procedure :: scattering_optical_depth => sample_scattering_optical_depth
    procedure :: forward_scattering_optical_depth =>                          &
                   sample_forward_scattering_optical_depth
  end type material_optics_sample_sectional_t

  interface aerosol_sectional_t
    module procedure :: constructor
  end interface aerosol_sectional_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function constructor( ) result( aerosol )
    type( aerosol_sectional_t ) :: aerosol
    aerosol%number_of_sections_ = 5
    allocate( aerosol%state_( aerosol%number_of_sections_ ) )
    ! initialize other aerosol parameters
  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! creates an optics object for this aerosol for the specified wavelength grid
  function get_optics_grid( this, grid ) result( optics )
    class( material_optics_grid_t ), pointer      :: optics
    class( aerosol_sectional_t ),        intent( in ) :: this
    class( wavelength_grid_t ),      intent( in ) :: grid

    allocate( material_optics_grid_sectional_t :: optics )
    select type( optics )
    type is( material_optics_grid_sectional_t )
      ! read file data, calculate parameters needed to do optics calculations
      ! (do expensive, initialization-time calculations here)
      optics%my_parameter_ = this%number_of_sections_

      ! save the grid, or just use the grid dimensions to set optics
      ! parameters
      optics%grid_ = grid
    end select
  end function get_optics_grid

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! creates an optics object for this aerosol for the specified wavelength
  ! sample
  function get_optics_sample( this, wavelength__m ) result( optics )
    class( material_optics_sample_t ), pointer      :: optics
    class( aerosol_sectional_t ),          intent( in ) :: this
    real,                              intent( in ) :: wavelength__m

    allocate( material_optics_sample_sectional_t :: optics )
    select type( optics )
    type is( material_optics_sample_sectional_t )
      ! read file data, calculate parameters needed to do optics calculations
      ! (do expensive, initialization-time calculations here)
      optics%my_parameter_  = 32
      optics%wavelength__m_ = wavelength__m
    end select
  end function get_optics_sample

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! calculates optical depth for the aerosol on the specified grid
  subroutine grid_optical_depth( this, aerosol, optical_depths )
    class( material_optics_grid_sectional_t ), intent( in )  :: this
    class( aerosol_t ),                    intent( in )  :: aerosol
    real,                                  intent( out ) :: optical_depths( : )

    ! do the run-time calculations for aerosol optical depth using the
    ! parameters set during intialization
    optical_depths( : ) = this%my_parameter_
  end subroutine grid_optical_depth

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! calculates scattering optical depth for the aerosol on the specified grid
  subroutine grid_scattering_optical_depth( this, aerosol, optical_depths )
    class( material_optics_grid_sectional_t ), intent( in )  :: this
    class( aerosol_t ),                    intent( in )  :: aerosol
    real,                                  intent( out ) :: optical_depths( : )

    ! do the run-time calculations for aerosol optical depth using the
    ! parameters set during intialization
    optical_depths( : ) = this%my_parameter_ / 132.4
  end subroutine grid_scattering_optical_depth

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! calculates forward scattering optical depth for the aerosol on the
  ! specified grid
  subroutine grid_forward_scattering_optical_depth( this, aerosol,            &
      optical_depths )
    class( material_optics_grid_sectional_t ), intent( in )  :: this
    class( aerosol_t ),                    intent( in )  :: aerosol
    real,                                  intent( out ) :: optical_depths( : )

    ! do the run-time calculations for aerosol optical depth using the
    ! parameters set during intialization
    optical_depths( : ) = this%my_parameter_ * 12.45
  end subroutine grid_forward_scattering_optical_depth

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! calculates optical depth for the aerosol for the specified wavelength
  ! sample
  subroutine sample_optical_depth( this, aerosol, optical_depth )
    class( material_optics_sample_sectional_t ), intent( in )  :: this
    class( aerosol_t ),                      intent( in )  :: aerosol
    real,                                    intent( out ) :: optical_depth

    ! do the run-time calculations for aerosol optical depth using the
    ! parameters set during intialization
    optical_depth = this%my_parameter_ / this%wavelength__m_
  end subroutine sample_optical_depth

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! calculates scattering optical depth for the aerosol for the specified
  ! wavelength sample
  subroutine sample_scattering_optical_depth( this, aerosol, optical_depth )
    class( material_optics_sample_sectional_t ), intent( in )  :: this
    class( aerosol_t ),                      intent( in )  :: aerosol
    real,                                    intent( out ) :: optical_depth

    ! do the run-time calculations for aerosol optical depth using the
    ! parameters set during intialization
    optical_depth = this%my_parameter_ / 132.4 * this%wavelength__m_
  end subroutine sample_scattering_optical_depth

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! calculates forward scattering optical depth for the aerosol for the
  ! specified wavelength sample
  subroutine sample_forward_scattering_optical_depth( this, aerosol,          &
      optical_depth )
    class( material_optics_sample_sectional_t ), intent( in )  :: this
    class( aerosol_t ),                      intent( in )  :: aerosol
    real,                                    intent( out ) :: optical_depth

    ! do the run-time calculations for aerosol optical depth using the
    ! parameters set during intialization
    optical_depth = this%my_parameter_ * 12.45 / this%wavelength__m_
  end subroutine sample_forward_scattering_optical_depth

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module musica_aerosol_sectional
