!> builder of \c aerosol_t extending objects
module musica_aerosol_factory

  implicit none
  private

  public :: musica_aerosol_factory_init
  public :: musica_aerosol_factory_run
  public :: musica_aerosol_factory_final

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> \section arg_table_musica_aerosol_factory_init  Argument Table
  !! \htmlinclude musica_aerosol_factory_init.html
  subroutine musica_aerosol_factory_init( type_name, aerosol, errcode, errmsg )

    use musica_aerosol,                only : aerosol_t
    use musica_aerosol_modal,          only : aerosol_modal_t
    use musica_aerosol_sectional,      only : aerosol_sectional_t

    character(len=*),                intent(in)  :: type_name
    class(aerosol_t),   allocatable, intent(out) :: aerosol
    integer,                         intent(out) :: errcode
    character(len=512),              intent(out) :: errmsg

    errcode = 0
    errmsg = ''

    if( type_name .eq. "modal" ) then
      aerosol = aerosol_modal_t( )
    else if( type_name .eq. "sectional" ) then
      aerosol = aerosol_sectional_t( )
    else
      errcode = 1
      errmsg  = "unsupported aerosol model '"//type_name//"'"
    end if

  end subroutine musica_aerosol_factory_init

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> \section arg_table_musica_aerosol_factory_run  Argument Table
  !! \htmlinclude musica_aerosol_factory_run.html
  subroutine musica_aerosol_factory_run( aerosol, errcode, errmsg )

    use musica_aerosol,                only : aerosol_t

    class(aerosol_t),   intent(inout) :: aerosol
    integer,            intent(out)   :: errcode
    character(len=512), intent(out)   :: errmsg

    errcode = 0
    errmsg  = ''

  end subroutine musica_aerosol_factory_run

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> \section arg_table_musica_aerosol_factory_final  Argument Table
  !! \htmlinclude musica_aerosol_factory_final.html
  subroutine musica_aerosol_factory_final( aerosol, errcode, errmsg )

    use musica_aerosol,                only : aerosol_t

    class(aerosol_t),   allocatable, intent(inout) :: aerosol
    integer,                         intent(out)   :: errcode
    character(len=512),              intent(out)   :: errmsg

    errcode = 0
    errmsg  = ''

    if( allocated( aerosol ) ) deallocate( aerosol )

  end subroutine musica_aerosol_factory_final

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module musica_aerosol_factory
