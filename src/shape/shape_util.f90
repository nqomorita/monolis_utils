module mod_monolis_shape_util
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_std_algebra
  use mod_monolis_shape_c3d8
  implicit none

contains

  subroutine monolis_C3D8_get_local_position(coord, pos, x, ths, ths_up, is_converge)
    implicit none
    integer(kint) :: i, j
    real(kdouble) :: coord(3,8), pos(3), x(3)
    real(kdouble) :: jacobi(3,3), inJacob(3,3), det, ths, ths_up
    real(kdouble) :: norm, func(8,3), n(8), fr(3), dx(3)
    logical :: is_converge, is_fail

    is_converge = .false.
    x = 0.0d0
    fr = 0.0d0
    do i = 1, 10
      call monolis_C3D8_shapefunc(x, n)
      fr = matmul(coord, n) - pos
      norm = dsqrt(fr(1)*fr(1) + fr(2)*fr(2) + fr(3)*fr(3))
      if(norm < ths)then
        is_converge = .true.
        exit
      endif

      call monolis_C3D8_shapefunc_deriv(x, func)
      jacobi = matmul(coord, func)
      call monolis_get_inverse_matrix_R_3d(jacobi, inJacob, det, is_fail)
      if(is_fail) exit

      dx = - Matmul(inJacob, fr)
      do j = 1, 3
        x(j) = x(j) + dx(j)
      enddo

      norm = dsqrt(x(1)*x(1) + x(2)*x(2) + x(3)*x(3))
      if(ths_up < norm)then
        exit
      endif
    enddo
  end subroutine monolis_C3D8_get_local_position
end module mod_monolis_shape_util
