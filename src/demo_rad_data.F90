module demo_rad_data

   implicit none
   private

   public :: demo_rad_data_init

   !> \section arg_table_demo_rad_data  Argument Table
   !! \htmlinclude arg_table_demo_rad_data.html
   character(len=128), public, protected :: aerosol_model_name = 'unset'

contains

   subroutine demo_rad_data_init(aerosol_model)
      character(len=*), intent(in) :: aerosol_model
      aerosol_model_name = trim(aerosol_model)
   end subroutine demo_rad_data_init

end module demo_rad_data
