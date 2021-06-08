module otps2
   use iso_c_binding, only: c_double, c_int, c_char, C_NULL_CHAR, c_f_pointer, c_loc, c_ptr

   implicit none

   include 'constit.h'
   include 'derived_types.h'

contains
   include 'subs.f90'

   subroutine predict_tide(ncon, c_id, z1Re, z1Im, lat, ntime, time_mjd, zpred) bind(c)
!DIR$ ATTRIBUTES DLLEXPORT :: predict_tide
      integer(c_int),         intent(in),    value  :: ncon
      character(kind=c_char), intent(in),    target :: c_id(*)
      real(c_double),         intent(in),    target :: z1Re(*)
      real(c_double),         intent(in),    target :: z1Im(*)
      real(c_double),         intent(in),    value  :: lat
      integer(c_int),         intent(in),    value  :: ntime
      real(c_double),         intent(in),    target :: time_mjd(*)
      real(c_double),         intent(inout), target :: zpred(*)

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
      call def_con_ind(c_id_, ncon, constid, ncmx, ind)
      interp = .true.
      z1(:) = cmplx(z1Re_, z1Im_)
      call ptide(z1, c_id_, ncon, ind, real(lat), time_mjd_, ntime, interp, zpred_tmp)
      zpred_(:) = zpred_tmp
   end subroutine

   subroutine predict_tide_2d(n, ncon, c_id, z1Re, z1Im, lat, ntime, time_mjd, zpred) bind(c)
!DIR$ ATTRIBUTES DLLEXPORT :: predict_tide_2d
      integer(c_int),         intent(in),    value  :: n
      integer(c_int),         intent(in),    value  :: ncon
      character(kind=c_char), intent(in),    target :: c_id(*)
      real(c_double),         intent(in),    target :: z1Re(*)
      real(c_double),         intent(in),    target :: z1Im(*)
      real(c_double),         intent(in),    target :: lat(*)
      integer(c_int),         intent(in),    value  :: ntime
      real(c_double),         intent(in),    target :: time_mjd(*)
      real(c_double),         intent(inout), target :: zpred(*)

      character(len=4),pointer   :: c_id_(:)
      real(c_double), pointer :: z1Re_(:, :), z1Im_(:, :), lat_(:)
      complex :: z1(n, ncon)
      real(c_double), pointer :: time_mjd_(:)
      real(c_double), pointer :: zpred_(:, :)
      real :: zpred_tmp(n, ntime)
      logical :: interp
      integer :: ind(ncon)
      integer :: i

      call c_f_pointer(c_loc(c_id), c_id_, (/ncon/))
      call c_f_pointer(c_loc(z1Re), z1Re_, (/n, ncon/))
      call c_f_pointer(c_loc(z1Im), z1Im_, (/n, ncon/))
      call c_f_pointer(c_loc(lat), lat_, (/n/))
      call c_f_pointer(c_loc(time_mjd), time_mjd_, (/ntime/))
      call c_f_pointer(c_loc(zpred), zpred_, (/n, ntime/))

      !do i=1,ncon
      !   write (*,*) c_id_(i), z1Re_(i), z1Im_(i)
      !end do
      call def_con_ind(c_id_, ncon, constid, ncmx, ind)
      interp = .true.
      z1(:,:) = cmplx(z1Re_, z1Im_)
      do i=1,n
         call ptide(z1(i,:), c_id_, ncon, ind, real(lat(i)), time_mjd_, ntime, interp, zpred_tmp(i,:))
      end do
      zpred_(:,:) = zpred_tmp
   end subroutine

end module