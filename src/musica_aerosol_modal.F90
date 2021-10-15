! a modal aerosol module
module musica_aerosol_modal

  use ccpp_kinds,                      only : kind_phys
  use musica_aerosol,                  only : aerosol_t,                      &
                                              material_optics_grid_t,         &
                                              material_optics_sample_t
  use musica_wavelength_grid,          only : wavelength_grid_t

  implicit none
  private

  public :: aerosol_modal_t

  ! a modal aerosol state and diagnostics
  type, extends( aerosol_t ) :: aerosol_modal_t
    integer           :: number_of_modes_
    real(kind_phys), allocatable :: state_(:) ! stand-in for mass, number, etc.
  contains
    procedure, private :: get_optics_grid
    procedure, private :: get_optics_sample
  end type aerosol_modal_t

  ! calculator of aerosol optical properties for a particular wavelength grid
  type, extends( material_optics_grid_t ) :: material_optics_grid_modal_t
    ! any parameters needed to calculate optics at run-time
    integer :: my_parameter_
    type( wavelength_grid_t ) :: grid_
  contains
    procedure :: optical_depth => grid_optical_depth
    procedure :: scattering_optical_depth => grid_scattering_optical_depth
    procedure :: forward_scattering_optical_depth =>                          &
                     grid_forward_scattering_optical_depth
  end type material_optics_grid_modal_t

  ! calculator of aerosol optical properties for a particular wavelength
  ! sample
  type, extends( material_optics_sample_t ) :: material_optics_sample_modal_t
    ! any parameters needed to calculate optics at run-time
    integer :: my_parameter_
    real :: wavelength__m_
  contains
    procedure :: optical_depth => sample_optical_depth
    procedure :: scattering_optical_depth => sample_scattering_optical_depth
    procedure :: forward_scattering_optical_depth =>                          &
                   sample_forward_scattering_optical_depth
  end type material_optics_sample_modal_t

  interface aerosol_modal_t
    module procedure :: constructor
  end interface aerosol_modal_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function constructor( ) result( aerosol )
    type( aerosol_modal_t ) :: aerosol
    aerosol%number_of_modes_ = 3
    allocate( aerosol%state_( aerosol%number_of_modes_ ) )
    ! initialize other aerosol parameters
  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! creates an optics object for this aerosol for the specified wavelength grid
  function get_optics_grid( this, grid ) result( optics )
    class( material_optics_grid_t ), pointer      :: optics
    class( aerosol_modal_t ),        intent( in ) :: this
    class( wavelength_grid_t ),      intent( in ) :: grid

    allocate( material_optics_grid_modal_t :: optics )
    select type( optics )
    type is( material_optics_grid_modal_t )
      ! read file data, calculate parameters needed to do optics calculations
      ! (do expensive, initialization-time calculations here)
      optics%my_parameter_ = this%number_of_modes_

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
    class( aerosol_modal_t ),          intent( in ) :: this
    real,                              intent( in ) :: wavelength__m

    allocate( material_optics_sample_modal_t :: optics )
    select type( optics )
    type is( material_optics_sample_modal_t )
      ! read file data, calculate parameters needed to do optics calculations
      ! (do expensive, initialization-time calculations here)
      optics%my_parameter_  = 12
      optics%wavelength__m_ = wavelength__m
    end select
  end function get_optics_sample

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! calculates optical depth for the aerosol on the specified grid
  subroutine grid_optical_depth( this, aerosol, optical_depths )
    class( material_optics_grid_modal_t ), intent( in )  :: this
    class( aerosol_t ),                    intent( in )  :: aerosol
    real,                                  intent( out ) :: optical_depths( : )

    ! do the run-time calculations for aerosol optical depth using the
    ! parameters set during intialization
    optical_depths( : ) = this%my_parameter_
  end subroutine grid_optical_depth

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! calculates scattering optical depth for the aerosol on the specified grid
  subroutine grid_scattering_optical_depth( this, aerosol, optical_depths )
    class( material_optics_grid_modal_t ), intent( in )  :: this
    class( aerosol_t ),                    intent( in )  :: aerosol
    real,                                  intent( out ) :: optical_depths( : )

    ! do the run-time calculations for aerosol optical depth using the
    ! parameters set during intialization
    optical_depths( : ) = this%my_parameter_ / 32.3
  end subroutine grid_scattering_optical_depth

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! calculates forward scattering optical depth for the aerosol on the
  ! specified grid
  subroutine grid_forward_scattering_optical_depth( this, aerosol,            &
      optical_depths )
    class( material_optics_grid_modal_t ), intent( in )  :: this
    class( aerosol_t ),                    intent( in )  :: aerosol
    real,                                  intent( out ) :: optical_depths( : )

    ! do the run-time calculations for aerosol optical depth using the
    ! parameters set during intialization
    optical_depths( : ) = this%my_parameter_ * 51.2
  end subroutine grid_forward_scattering_optical_depth

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! calculates optical depth for the aerosol for the specified wavelength
  ! sample
  subroutine sample_optical_depth( this, aerosol, optical_depth )
    class( material_optics_sample_modal_t ), intent( in )  :: this
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
    class( material_optics_sample_modal_t ), intent( in )  :: this
    class( aerosol_t ),                      intent( in )  :: aerosol
    real,                                    intent( out ) :: optical_depth

    ! do the run-time calculations for aerosol optical depth using the
    ! parameters set during intialization
    optical_depth = this%my_parameter_ / 32.3 * this%wavelength__m_
  end subroutine sample_scattering_optical_depth

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! calculates forward scattering optical depth for the aerosol for the
  ! specified wavelength sample
  subroutine sample_forward_scattering_optical_depth( this, aerosol,          &
      optical_depth )
    class( material_optics_sample_modal_t ), intent( in )  :: this
    class( aerosol_t ),                      intent( in )  :: aerosol
    real,                                    intent( out ) :: optical_depth

    ! do the run-time calculations for aerosol optical depth using the
    ! parameters set during intialization
    optical_depth = this%my_parameter_ * 51.2 / this%wavelength__m_
  end subroutine sample_forward_scattering_optical_depth

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> \section arg_table_aerosol_modal_init  Argument Table
  !! \htmlinclude aerosol_modal_init.html
  subroutine aerosol_modal_init(aerosol, errcode, errmsg)
    type(aerosol_modal_t), intent(out) :: aerosol
    integer,               intent(out) :: errcode
    character(len=512),    intent(out) :: errmsg

    errcode = 0
    errmsg = ''

    aerosol = aerosol_modal_t()

  end subroutine aerosol_modal_init

  !> \section arg_table_aerosol_modal_run  Argument Table
  !! \htmlinclude aerosol_modal_run.html
  subroutine aerosol_modal_run(aerosol, errcode, errmsg)
    type(aerosol_modal_t), intent(in)  :: aerosol
    integer,               intent(out) :: errcode
    character(len=512),    intent(out) :: errmsg

    errcode = 0
    errmsg = ''

    if (.not. allocated(aerosol%state_)) then
       errcode = 1
       errmsg = 'ERROR: aerosol not allocated'
    end if

  end subroutine aerosol_modal_run

end module musica_aerosol_modal
