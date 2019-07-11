module otps2
   use iso_c_binding, only: c_double, c_double_complex, c_int, c_char, C_NULL_CHAR, c_f_pointer, c_loc, c_ptr

   implicit none

   include 'constit.h'   

   contains

   subroutine predict_tide(ncon, c_id, z1Re, z1Im, lat, ntime, time_mjd, zpred) bind(c)
!DIR$ ATTRIBUTES DLLEXPORT :: predict_tide
      integer(c_int), value, intent(in) :: ncon
      character(kind=c_char),intent(in), target :: c_id(*)
      complex(c_double_complex), intent(in), target :: z1Re(*)
      complex(c_double_complex), intent(in), target :: z1Im(*)
      real(c_double), value, intent(in) :: lat
      integer(c_int), value, intent(in) :: ntime
      real(c_double), intent(in), target :: time_mjd(*)
      real(c_double), intent(inout), target :: zpred(*)

      character(len=4),pointer   :: c_id_(:)
      real(c_double), pointer :: z1Re_(:), z1Im_(:)
      complex :: z1(ncon)
      real(c_double), pointer :: time_mjd_(:)
      real(c_double), pointer :: zpred_(:)
      real :: zpred_tmp(ntime)
      logical :: interp
      integer :: ind(ncon)
      integer :: i

      call c_f_pointer(c_loc(c_id), c_id_, (/ncon/))
      call c_f_pointer(c_loc(z1Re), z1Re_, (/ncon/))
      call c_f_pointer(c_loc(z1Im), z1Im_, (/ncon/))
      call c_f_pointer(c_loc(time_mjd), time_mjd_, (/ntime/))
      call c_f_pointer(c_loc(zpred), zpred_, (/ntime/))

      !do i=1,ncon
      !   write (*,*) c_id_(i), z1Re_(i), z1Im_(i)
      !end do
      call def_con_ind(c_id_,ncon,constid,ncmx,ind)
      interp = .true.
      z1(:) = cmplx(z1Re_, z1Im_)
      call ptide(z1, c_id_, ncon, ind, real(lat), time_mjd_, ntime, interp, zpred_tmp)
      zpred_(:) = zpred_tmp
   end subroutine
end module