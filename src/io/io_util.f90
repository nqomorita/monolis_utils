!> IO util モジュール
module mod_monolis_io_util
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_alloc
  implicit none

contains

  !> @ingroup io
  !> elem データが 0 オリジン（C）、1 オリジン（fortran）か判定
  subroutine monolis_check_fortran_1_origin_elem(elem, is_1_origin)
    implicit none
    !> [in] elem データ
    integer(kint), intent(in) :: elem(:,:)
    !> [out] 1 オリジンであれば真
    logical, intent(out) :: is_1_origin
    is_1_origin = .true.
    if(minval(elem) == 0) is_1_origin = .false.
  end subroutine monolis_check_fortran_1_origin_elem

  !> @ingroup io
  !> graph データの item 配列が 0 オリジン（C）、1 オリジン（fortran）か判定
  subroutine monolis_check_fortran_1_origin_graph(item, is_1_origin)
    implicit none
    !> [in] graph データの item 配列
    integer(kint), intent(in) :: item(:)
    !> [out] 1 オリジンであれば真
    logical, intent(out) :: is_1_origin
    is_1_origin = .true.
    if(minval(item) == 0) is_1_origin = .false.
  end subroutine monolis_check_fortran_1_origin_graph
end module mod_monolis_io_util
