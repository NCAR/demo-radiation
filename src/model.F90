! the host model
program model

  use musica_aerosol,                  only : aerosol_t
  use musica_aerosol_modal,            only : aerosol_modal_t
  use musica_radiation,                only : radiation_initialize, radiation_run

  implicit none

  class(aerosol_t), pointer :: aerosol
  integer :: i_time

  ! with CCPP, the aerosol_t object would probably be registered by an aerosol scheme
  ! and exist as a standard-named variable?
  aerosol => aerosol_modal_t()

  ! this would be through the CCPP initialization of radiation
  call radiation_initialize( aerosol )

  ! time loop
  do i_time = 1, 5

    ! this would be through the CCPP run call to radiation
    ! radiation would ask for the aerosol_t object as an argument to its run function
    call radiation_run( aerosol )

  end do

end program model
