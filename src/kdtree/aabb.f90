!> バウンディングボックスモジュール
module mod_monolis_utils_aabb
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_error

  implicit none

contains

  !> @ingroup aabb
  !> 座標の集合からバウンディングボックスを作成
  subroutine monolis_get_aabb_from_coodinates(coord, BB)
    implicit none
    !> [in] 座標の集合（[1,:] に x 座標、[2,:] に y 座標、[3,:] に z 座標を格納）
    real(kdouble), intent(in) :: coord(:,:)
    !> [out] バウンディングボックス（x_min, x_max, y_min, y_max, z_min, z_max の順に格納）
    real(kdouble), intent(out) :: BB(6)

    BB(1) = minval(coord(1,:))
    BB(2) = maxval(coord(1,:))
    BB(3) = minval(coord(2,:))
    BB(4) = maxval(coord(2,:))
    BB(5) = minval(coord(3,:))
    BB(6) = maxval(coord(3,:))
  end subroutine monolis_get_aabb_from_coodinates

  !> @ingroup aabb
  !> バウンディングボックスの集合からバウンディングボックスを作成
  subroutine monolis_get_aabb_from_BB(BB_in, BB)
    implicit none
    !> [in] バウンディングボックスの集合（x_min, x_max, y_min, y_max, z_min, z_max の順に格納）
    real(kdouble), intent(in) :: BB_in(:,:)
    !> [out] バウンディングボックス（x_min, x_max, y_min, y_max, z_min, z_max の順に格納）
    real(kdouble), intent(out) :: BB(6)

    BB(1) = minval(BB_in(1,:))
    BB(2) = maxval(BB_in(2,:))
    BB(3) = minval(BB_in(3,:))
    BB(4) = maxval(BB_in(4,:))
    BB(5) = minval(BB_in(5,:))
    BB(6) = maxval(BB_in(6,:))
  end subroutine monolis_get_aabb_from_BB

  !> @ingroup aabb
  !> 座標がバウンディングボックスに含まれているか判定
  subroutine monolis_check_inside_in_aabb(coord, BB, ths, is_inside)
    implicit none
    !> [in] 座標
    real(kdouble), intent(in) :: coord(3)
    !> [in] バウンディングボックス（x_min, x_max, y_min, y_max, z_min, z_max の順に格納）
    real(kdouble), intent(in) :: BB(6)
    !> [in] 内包判定の閾値
    real(kdouble), intent(in) :: ths
    !> [out] 内包判定フラグ（内側に含まれていれば .true. を返す）
    logical, intent(out) :: is_inside

    is_inside = .false.

    if(BB(1) - ths <= coord(1) .and. coord(1) <= BB(2) + ths .and. &
       BB(3) - ths <= coord(2) .and. coord(2) <= BB(4) + ths .and. &
       BB(5) - ths <= coord(3) .and. coord(3) <= BB(6) + ths)then
      is_inside = .true.
    endif
  end subroutine monolis_check_inside_in_aabb
end module mod_monolis_utils_aabb
