module otps2
   use iso_c_binding, only: c_double, c_int, c_char, C_NULL_CHAR, c_f_pointer, c_loc, c_ptr

   implicit none

   include 'constit.h'
   include 'derived_types.h'

contains
   include 'subs.f90'

   subroutine predict_tide(ncon, c_id, z1Re, z1Im, lat, ntime, time_mjd, zpred) bind(c)
!DIR$ ATTRIBUTES DLLEXPORT :: predict_tide
      integer(c_int),         intent(in), value  :: ncon
      character(kind=c_char), intent(in), target :: c_id(*)
      real(c_double),         intent(in)         :: z1Re(ncon)
      real(c_double),         intent(in)         :: z1Im(ncon)
      real(c_double),         intent(in), value  :: lat
      integer(c_int),         intent(in), value  :: ntime
      real(c_double),         intent(in)         :: time_mjd(ntime)
      real(c_double),         intent(inout)      :: zpred(ntime)

      character(len=4),pointer   :: c_id_(:)
      complex :: z1(ncon)
      real :: zpred_tmp(ntime)
      logical :: interp
      integer :: ind(ncon)
      integer :: i

      call c_f_pointer(c_loc(c_id), c_id_, (/ncon/))

      !do i=1,ncon
      !   write (*,*) c_id(i), z1Re(i), z1Im(i)
      !end do
      call def_con_ind(c_id_, ncon, constid, ncmx, ind)
      interp = .true.
      z1(:) = cmplx(z1Re, z1Im)
      call ptide(z1, c_id_, ncon, ind, real(lat), time_mjd, ntime, interp, zpred_tmp)
      zpred(:) = zpred_tmp
   end subroutine

   subroutine predict_tide_2d(n, ncon, c_id, z1Re, z1Im, lat, ntime, time_mjd, zpred) bind(c)
!DIR$ ATTRIBUTES DLLEXPORT :: predict_tide_2d
      integer(c_int),         intent(in), value  :: n
      integer(c_int),         intent(in), value  :: ncon
      character(kind=c_char), intent(in), target :: c_id(*)
      real(c_double),         intent(in)         :: z1Re(n, ncon)
      real(c_double),         intent(in)         :: z1Im(n, ncon)
      real(c_double),         intent(in)         :: lat(n)
      integer(c_int),         intent(in), value  :: ntime
      real(c_double),         intent(in)         :: time_mjd(ntime)
      real(c_double),         intent(inout)      :: zpred(n, ntime)

      character(len=4),pointer :: c_id_(:)
      complex :: z1(ncon)
      real :: zpred_tmp(ntime)
      logical :: interp
      integer :: ind(ncon)
      integer :: i

      call c_f_pointer(c_loc(c_id), c_id_, (/ncon/))
      call def_con_ind(c_id_, ncon, constid, ncmx, ind)
      interp = .true.
      do i=1,n
         z1(:) = cmplx(z1Re(i,:), z1Im(i,:))
         call ptide(z1, c_id_, ncon, ind, real(lat(i)), time_mjd, ntime, interp, zpred_tmp)
         zpred(i,:) = zpred_tmp
      end do
   end subroutine

end module