! a wavelength grid
module musica_wavelength_grid

  implicit none
  private

  public :: wavelength_grid_t

  integer, parameter, public :: kUnspecifiedInterpolation = 0
  integer, parameter, public :: kSomeInterpolation        = 1
  integer, parameter, public :: kAnotherInterpolation     = 2

  type :: wavelength_grid_t
    private
    real, allocatable :: min_wavelength__m_( : )
    real, allocatable :: max_wavelength__m_( : )
    integer :: interpolation_type_ = kUnspecifiedInterpolation
  contains
    procedure :: number_of_bins
    ! add functions for accessing grid properties
  end type wavelength_grid_t

  interface wavelength_grid_t
    module procedure :: constructor
  end interface wavelength_grid_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  type( wavelength_grid_t ) function constructor( ) result( grid )
    allocate( grid%min_wavelength__m_( 3 ) )
    allocate( grid%max_wavelength__m_( 3 ) )
    grid%min_wavelength__m_( : ) = (/ 12.3, 43.2, 93.4 /)
    grid%max_wavelength__m_( : ) = (/ 22.3, 64.2, 99.4 /)
    grid%interpolation_type_ = kUnspecifiedInterpolation
  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  integer function number_of_bins( this )
    class( wavelength_grid_t ), intent( in ) ::  this
    number_of_bins = 3
  end function number_of_bins

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module musica_wavelength_grid
