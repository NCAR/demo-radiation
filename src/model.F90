module demo_rad_mod

   implicit none
   private

   public :: demo_rad

CONTAINS

   !> \section arg_table_demo_rad  Argument Table
   !! \htmlinclude arg_table_demo_rad.html
   subroutine demo_rad()
      use demo_rad_ccpp_cap, only: demo_rad_ccpp_physics_initialize
      use demo_rad_ccpp_cap, only: demo_rad_ccpp_physics_timestep_initial
      use demo_rad_ccpp_cap, only: demo_rad_ccpp_physics_run
      use demo_rad_ccpp_cap, only: demo_rad_ccpp_physics_timestep_final
      use demo_rad_ccpp_cap, only: demo_rad_ccpp_physics_finalize

      integer                       :: i_time
      integer                       :: errcode
      integer                       :: col_start = 1
      integer                       :: col_end = 1
      character(len=512)            :: errmsg
      character(len=256)            :: arg_val
      character(len=:), allocatable :: aerosol_model_name

      ! get the aerosol model name
      if( command_argument_count( ) .ne. 1 ) then
        write(*,*) "Usage: ./demo aerosol_model_name"
        stop 3
      endif
      call get_command_argument( 1, arg_val )
      aerosol_model_name = trim( arg_val )

      ! Use the suite information to setup the run
      call demo_rad_ccpp_physics_initialize('toy_suite', aerosol_model_name,  &
           errmsg, errcode)
      if (errcode /= 0) then
         write(6, *) trim(errmsg)
         write(6, *) 'An error occurred in ccpp_physics_init, Exiting...'
         stop
      end if

      do i_time = 1, 5
         ! Initialize the timestep
         call demo_rad_ccpp_physics_timestep_initial('toy_suite',             &
              aerosol_model_name, errmsg, errcode)
         if (errcode /= 0) then
            write(6, *) trim(errmsg)
            write(6, *) 'An error occurred in ccpp_physics_timestep_init, ",  &
                 "Exiting...'
            stop
         end if

         call demo_rad_ccpp_physics_run('toy_suite', 'chemistry', col_start,  &
              col_end, aerosol_model_name, errmsg, errcode)
         if (errcode /= 0) then
            write(6, *) trim(errmsg)
            write(6, *) 'An error occurred in ccpp_physics_run, Exiting...'
            stop
         end if

         call demo_rad_ccpp_physics_timestep_final('toy_suite',               &
              aerosol_model_name, errmsg, errcode)
         if (errcode /= 0) then
            write(6, *) trim(errmsg)
            write(6, *) 'An error occurred in ccpp_physics_timestep_final, ", &
                 "Exiting...'
            stop
         end if

      end do

      call demo_rad_ccpp_physics_finalize('toy_suite', aerosol_model_name,    &
           errmsg, errcode)
      if (errcode /= 0) then
         write(6, *) trim(errmsg)
         write(6,'(a)') 'An error occurred in ccpp_physics_final, Exiting...'
         stop
      end if

   end subroutine demo_rad

end module demo_rad_mod

! the host model
program model
   use demo_rad_mod, only: demo_rad
   implicit none

   call demo_rad()

end program model
