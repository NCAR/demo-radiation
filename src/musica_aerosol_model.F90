! Factory "scheme" to run an aerosol model
module musica_aerosol_model

  use ccpp_kinds,                      only : kind_phys
  use musica_aerosol,                  only : aerosol_t

  implicit none
  private

  public :: aerosol_model_init
  public :: aerosol_model_run

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> \section arg_table_aerosol_model_init  Argument Table
  !! \htmlinclude aerosol_model_init.html
  subroutine aerosol_model_init(model_name, aerosol, errcode, errmsg)
    use musica_aerosol_modal,     only : aerosol_modal_t
    use musica_aerosol_sectional, only : aerosol_sectional_t

    character(len=*),              intent(in)  :: model_name
    class(aerosol_t), allocatable, intent(out) :: aerosol
    integer,                       intent(out) :: errcode
    character(len=512),            intent(out) :: errmsg

    errcode = 0
    errmsg = ''

    select case(trim(model_name))
    case('modal_aerosol')
       allocate( aerosol_modal_t :: aerosol)
       aerosol = aerosol_modal_t()
    case('sectional_aerosol')
       allocate( aerosol_sectional_t :: aerosol)
       aerosol = aerosol_sectional_t()
    case default
       errcode = 1
       errmsg = "Unknown aerosol model, '"//trim(model_name)//"'"
    end select

  end subroutine aerosol_model_init

  !> \section arg_table_aerosol_model_run  Argument Table
  !! \htmlinclude aerosol_model_run.html
  subroutine aerosol_model_run(aerosol, errcode, errmsg)

    class(aerosol_t),   intent(inout) :: aerosol
    integer,            intent(out)   :: errcode
    character(len=512), intent(out)   :: errmsg

    errcode = 0
    errmsg = ''

    ! Check allocation
    if (.not. aerosol%initialized) then
       errcode = 1
       errmsg = 'ERROR: aerosol not allocated'
    end if

    ! run aerosol model
    call aerosol%aerosol_run(errcode, errmsg)

  end subroutine aerosol_model_run

end module musica_aerosol_model
