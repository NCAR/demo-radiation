module demo_model_data

  implicit none
  private

  public :: set_aerosol_model_name

  !> \section arg_table_demo_model_data  Argument Table
  !! \htmlinclude arg_table_demo_model_data
  character(len=:), allocatable, public, protected :: aerosol_model_name

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine set_aerosol_model_name( aerosol_model )

    character(len=*), intent(in) :: aerosol_model

    if( allocated( aerosol_model_name ) ) then
      write(*,*) "trying to reset aerosol model"
      stop 3
    end if

    aerosol_model_name = trim( aerosol_model )

  end subroutine set_aerosol_model_name

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module demo_model_data
