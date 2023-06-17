!> IO モジュール
!# monolis_input_graph(fname, n_vertex, vertex_id, index, item)
!# monolis_output_graph(fname, n_vertex, vertex_id, index, item)
!# monolis_input_node(fname, n_node, node)
!# monolis_output_node(fname, n_node, node)
!# monolis_input_elem(fname, n_elem, n_base, elem)
!# monolis_output_elem(fname, n_elem, n_base, elem)
!# monolis_input_internal_vertex_number(fname, n_internal)
!# monolis_output_internal_vertex_number(fname, n_internal)
!# monolis_input_global_id(fname, n_vertex, vertex_id)
!# monolis_output_global_id(fname, n_vertex, vertex_id)
!# monolis_input_bc_R(fname, n_bc, n_dof, i_bc, r_bc)
!# monolis_output_bc_R(fname, n_bc, n_dof, i_bc, r_bc)
!# monolis_input_bc_C(fname, n_bc, n_dof, i_bc, r_bc)
!# monolis_output_bc_C(fname, n_bc, n_dof, i_bc, r_bc)
!# monolis_input_distval_I(fname, label, n_node, n_dof, val)
!# monolis_output_distval_I(fname, label, n_node, n_dof, val)
!# monolis_input_distval_R(fname, label, n_node, n_dof, val)
!# monolis_output_distval_R(fname, label, n_node, n_dof, val)
!# monolis_input_distval_C(fname, label, n_node, n_dof, val)
!# monolis_output_distval_C(fname, label, n_node, n_dof, val)
!# monolis_input_file_error_check(ierr)
module mod_monolis_io
  use mod_monolis_utils_define_prm
  use mod_monolis_utils_alloc
  implicit none

contains

  !> @ingroup io
  !> graph フォーマットの入力
  !> @note IO では graph フォーマットを入出力するが、
  !> プログラム内部では CSR 圧縮形式によってグラフを保持する。
  subroutine monolis_input_graph(fname, n_vertex, vertex_id, index, item)
    implicit none
    !> [in] 入力ファイル名
    character(*), intent(in) :: fname
    !> [out] 全計算点数
    integer(kint), intent(out) :: n_vertex
    !> [out] 計算点 id
    integer(kint), allocatable, intent(out) :: vertex_id(:)
    !> [out] グラフの CSR 圧縮形式の index 配列
    integer(kint), allocatable, intent(out) :: index(:)
    !> [out] グラフの CSR 圧縮形式の index 配列
    integer(kint), allocatable, intent(out) :: item(:)
    integer(kint) :: i, in, j, tmp, nz, ierr

    nz = 0
    open(20, file = fname, status = "old", iostat = ierr)
      call monolis_input_file_error_check(ierr)
      read(20,*) n_vertex
      do i = 1, n_vertex
        read(20,*) tmp, in
        nz = nz + in
      enddo
    close(20)

    call monolis_alloc_I_1d(vertex_id, n_vertex)
    call monolis_alloc_I_1d(index, n_vertex+1)
    call monolis_alloc_I_1d(item, nz)

    nz = 0
    open(20, file = fname, status = "old", iostat = ierr)
      read(20,*) n_vertex
      do i = 1, n_vertex
        read(20,*) vertex_id(i), in, (item(nz+j), j = 1, in)
        index(i+1) = index(i) + in
        nz = nz + in
      enddo
    close(20)
  end subroutine monolis_input_graph

  !> @ingroup io
  !> graph フォーマットの出力
  subroutine monolis_output_graph(fname, n_vertex, vertex_id, index, item)
    implicit none
    !> [in] 出力ファイル名
    character(*), intent(in) :: fname
    !> [in] 全計算点数
    integer(kint), intent(in) :: n_vertex
    !> [in] 計算点 id
    integer(kint), intent(in) :: vertex_id(:)
    !> [in] グラフの CSR 圧縮形式の index 配列
    integer(kint), intent(in) :: index(:)
    !> [in] グラフの CSR 圧縮形式の index 配列
    integer(kint), intent(in) :: item(:)
    integer(kint) :: i, in, j, jS, jE

    open(20, file = trim(fname), status = "replace")
      write(20,"(i0)") n_vertex
      do i = 1, n_vertex
        jS = index(i) + 1
        jE = index(i+1)
        in = jE - jS + 1
        write(20,"(i0,x,i0,$)") vertex_id(i), in
        do j = jS, jE
          write(20,"(x,i0,$)") item(j)
        enddo
        write(20,*)""
      enddo
    close(20)
  end subroutine monolis_output_graph

  !> @ingroup io
  !> node フォーマットの入力
  subroutine monolis_input_node(fname, n_node, node)
    implicit none
    !> [in] 出力ファイル名
    character(*), intent(in) :: fname
    !> [out] 節点数
    integer(kint), intent(out) :: n_node
    !> [out] 節点座標
    real(kdouble), allocatable, intent(out) :: node(:,:)
    integer(kint) :: i

    open(20, file = trim(fname), status = "old")
      read(20,*) n_node
      call monolis_alloc_R_2d(node, 3, n_node)
      do i = 1, n_node
        read(20,*) node(1,i), node(2,i), node(3,i)
      enddo
    close(20)
  end subroutine monolis_input_node

  !> @ingroup io
  !> node フォーマットの出力
  subroutine monolis_output_node(fname, n_node, node)
    implicit none
    !> [in] 出力ファイル名
    character(*), intent(in) :: fname
    !> [in] 節点数
    integer(kint), intent(in) :: n_node
    !> [in] 節点座標
    real(kdouble), intent(in) :: node(:,:)
    integer(kint) :: i

    open(20, file = trim(fname), status = "replace")
      write(20,"(i0)")n_node
      do i = 1, n_node
        write(20,"(1p3e22.14)") node(1,i), node(2,i), node(3,i)
      enddo
    close(20)
  end subroutine monolis_output_node

  !> @ingroup io
  !> elem フォーマットの入力
  subroutine monolis_input_elem(fname, n_elem, n_base, elem)
    implicit none
    !> [in] 出力ファイル名
    character(*), intent(in) :: fname
    !> [out] 要素数
    integer(kint), intent(out) :: n_elem
    !> [out] 要素を構成する形状関数の数
    integer(kint), intent(out) :: n_base
    !> [out] 要素コネクティビティ
    integer(kint), allocatable, intent(out) :: elem(:,:)
    integer(kint) :: i, j

    open(20, file = trim(fname), status = "old")
      read(20,*) n_elem, n_base
      call monolis_alloc_I_2d(elem, n_base, n_elem)
      do i = 1, n_elem
        read(20,*) (elem(j,i), j = 1, n_base)
      enddo
    close(20)
  end subroutine monolis_input_elem

  !> @ingroup io
  !> elem フォーマットの出力
  subroutine monolis_output_elem(fname, n_elem, n_base, elem)
    implicit none
    !> [in] 出力ファイル名
    character(*), intent(in) :: fname
    !> [in] 要素数
    integer(kint), intent(in) :: n_elem
    !> [in] 要素を構成する形状関数の数
    integer(kint), intent(in) :: n_base
    !> [in] 要素コネクティビティ
    integer(kint), intent(in) :: elem(:,:)
    integer(kint) :: i, j

    open(20, file = trim(fname), status = "replace")
      write(20,"(i0,x,i0)")n_elem, n_base
      do i = 1, n_elem
        do j = 1, n_base
          write(20,"(x,i0,$)") elem(j,i)
        enddo
        write(20,*)""
      enddo
    close(20)
  end subroutine monolis_output_elem

  !> @ingroup io
  !> monolis 内部計算点自由度数の入力
  subroutine monolis_input_internal_vertex_number(fname, n_internal)
    implicit none
    !> [in] 出力ファイル名
    character(*), intent(in) :: fname
    !> [out] 内部自由度の数
    integer(kint), intent(out) :: n_internal
    integer(kint) :: i
    character(monolis_charlen) :: label

    open(20, file = trim(fname), status = "old")
      read(20,*) label, i
      read(20,*) n_internal
    close(20)
  end subroutine monolis_input_internal_vertex_number

  !> @ingroup io
  !> monolis 内部計算点自由度数の出力
  subroutine monolis_output_internal_vertex_number(fname, n_internal)
    implicit none
    !> [in] 出力ファイル名
    character(*), intent(in) :: fname
    !> [in] 内部自由度の数
    integer(kint), intent(in) :: n_internal

    open(20, file = trim(fname), status = "replace")
      write(20,"(a)") "#n_internal 1"
      write(20,"(i0)") n_internal
    close(20)
  end subroutine monolis_output_internal_vertex_number

  !> @ingroup io
  !> monolis グローバル id の入力
  subroutine monolis_input_global_id(fname, n_vertex, vertex_id)
    implicit none
    !> [in] 出力ファイル名
    character(*), intent(in) :: fname
    !> [out] 分割領域における全計算点数
    integer(kint), intent(out) :: n_vertex
    !> [out] 計算点 id
    integer(kint), allocatable, intent(out) :: vertex_id(:)
    integer(kint) :: i, n_dof
    character(monolis_charlen) :: label

    open(20, file = trim(fname), status = "old")
      read(20,*) label
      read(20,*) n_vertex, n_dof

      call monolis_alloc_I_1d(vertex_id, n_vertex)

      do i = 1, n_vertex
        read(20,*) vertex_id(i)
      enddo
    close(20)
  end subroutine monolis_input_global_id

  !> @ingroup io
  !> monolis  グローバル id の出力
  subroutine monolis_output_global_id(fname, n_vertex, vertex_id)
    implicit none
    !> [in] 出力ファイル名
    character(*), intent(in) :: fname
    !> [in] 全計算点数
    integer(kint), intent(in) :: n_vertex
    !> [in] 計算点 id
    integer(kint), intent(in) :: vertex_id(:)
    integer(kint) :: i

    open(20, file = trim(fname), status = "replace")
      write(20,"(a)") "#id"
      write(20,"(i0,x,i0)") n_vertex, 1

      do i = 1, n_vertex
        write(20,"(i0)") vertex_id(i)
      enddo
    close(20)
  end subroutine monolis_output_global_id

  !> @ingroup io
  !> bc フォーマットの入力
  subroutine monolis_input_bc_R(fname, n_bc, n_dof, i_bc, r_bc)
    implicit none
    !> [in] 出力ファイル名
    character(*), intent(in) :: fname
    !> [out] 境界条件の数
    integer(kint),intent(out) :: n_bc
    !> [out] 計算点が持つ自由度
    integer(kint), intent(out) :: n_dof
    !> [out] 境界条件の付与番号と付与自由度
    integer(kint), allocatable, intent(out) :: i_bc(:,:)
    !> [out] 境界条件の値
    real(kdouble), allocatable, intent(out) :: r_bc(:)
    integer(kint) :: i

    open(20, file = trim(fname), status = "old")
      read(20,*) n_bc, n_dof

      call monolis_alloc_I_2d(i_bc, 2, n_bc)
      call monolis_alloc_R_1d(r_bc, n_bc)

      do i = 1, n_bc
        read(20,*) i_bc(1,i), i_bc(2,i), r_bc(i)
      enddo
    close(20)
  end subroutine monolis_input_bc_R

  !> @ingroup io
  !> bc フォーマットの出力
  subroutine monolis_output_bc_R(fname, n_bc, n_dof, i_bc, r_bc)
    implicit none
    !> [in] 出力ファイル名
    character(*), intent(in) :: fname
    !> [in] 境界条件の数
    integer(kint), intent(in) :: n_bc
    !> [in] 自由度数
    integer(kint), intent(in) :: n_dof
    !> [in] 境界条件の付与番号と付与自由度
    integer(kint), intent(in) :: i_bc(:,:)
    !> [in] 境界条件の値
    real(kdouble), intent(in) :: r_bc(:)
    integer(kint) :: i

    open(20, file = trim(fname), status = "replace")
      write(20,"(i0,x,i0)") n_bc, n_dof
      do i = 1, n_bc
        write(20,"(i0,x,i0,x,1pe22.14)") i_bc(1,i), i_bc(2,i), r_bc(i)
      enddo
    close(20)
  end subroutine monolis_output_bc_R

  !> @ingroup io
  !> bc フォーマットの入力
  subroutine monolis_input_bc_C(fname, n_bc, n_dof, i_bc, c_bc)
    implicit none
    !> [in] 出力ファイル名
    character(*), intent(in) :: fname
    !> [out] 境界条件の数
    integer(kint), intent(out) :: n_bc
    !> [out] 自由度数
    integer(kint), intent(out) :: n_dof
    !> [out] 境界条件の付与番号と付与自由度
    integer(kint), allocatable, intent(out) :: i_bc(:,:)
    !> [out] 境界条件の値
    complex(kdouble), allocatable, intent(out) :: c_bc(:)
    integer(kint) :: i, j
    real(kdouble) :: tmp(2)

    open(20, file = trim(fname), status = "old")
      read(20,*) n_bc, n_dof

      call monolis_alloc_I_2d(i_bc, 2, n_bc)
      call monolis_alloc_C_1d(c_bc, n_bc)

      do i = 1, n_bc
        read(20,*) i_bc(1,i), i_bc(2,i), tmp(1), tmp(2)
        c_bc(i) = cmplx(tmp(1), tmp(2))
      enddo
    close(20)
  end subroutine monolis_input_bc_C

  !> @ingroup io
  !> bc フォーマットの出力
  subroutine monolis_output_bc_C(fname, n_bc, n_dof, i_bc, c_bc)
    implicit none
    !> [in] 出力ファイル名
    character(*), intent(in) :: fname
    !> [in] 境界条件の数
    integer(kint), intent(in) :: n_bc
    !> [in] 自由度数
    integer(kint), intent(in) :: n_dof
    !> [in] 境界条件の付与番号と付与自由度
    integer(kint), intent(in) :: i_bc(:,:)
    !> [in] 境界条件の値
    complex(kdouble), intent(in) :: c_bc(:)
    integer(kint) :: i

    open(20, file = trim(fname), status = "replace")
      write(20,"(i0,x,i0)") n_bc, n_dof
      do i = 1, n_bc
        write(20,"(i0,x,i0,x,1p2e22.14)") i_bc(1,i), i_bc(2,i), c_bc(i)
      enddo
    close(20)
  end subroutine monolis_output_bc_C

  !> @ingroup io
  !> distval フォーマットの入力（整数型）
  subroutine monolis_input_distval_I(fname, label, n_node, n_dof, val)
    implicit none
    !> [in] 出力ファイル名
    character(*), intent(in) :: fname
    !> [out] ラベル名
    character(monolis_charlen), intent(out) :: label
    !> [out] 節点数
    integer(kint), intent(out) :: n_node
    !> [out] 計算点が持つ自由度
    integer(kint), intent(out) :: n_dof
    !> [out] データ
    integer(kint), allocatable, intent(out) :: val(:,:)
    integer(kint) :: i, j

    open(20, file = trim(fname), status = "old")
      read(20,*) label
      read(20,*) n_node, n_dof

      call monolis_alloc_I_2d(val, n_dof, n_node)

      do i = 1, n_node
        read(20,*) (val(j,i), j = 1, n_dof)
      enddo
    close(20)
  end subroutine monolis_input_distval_I

  !> @ingroup io
  !> distval フォーマットの出力（整数型）
  subroutine monolis_output_distval_I(fname, label, n_node, n_dof, val)
    implicit none
    !> [in] 出力ファイル名
    character(*), intent(in) :: fname
    !> [in] ラベル名
    character(*), intent(in) :: label
    !> [in] 節点数
    integer(kint), intent(in) :: n_node
    !> [in] 計算点が持つ自由度
    integer(kint), intent(in) :: n_dof
    !> [in] データ
    integer(kint), intent(in) :: val(:,:)
    integer(kint) :: i, j

    open(20, file = trim(fname), status = "replace")
      write(20,"(a)") trim(label)
      write(20,"(i0,x,i0)") n_node, n_dof

      do i = 1, n_node
        do j = 1, n_dof
          write(20,"(x,i0,x,$)") val(j,i)
        enddo
        write(20,*)""
      enddo
    close(20)
  end subroutine monolis_output_distval_I

  !> @ingroup io
  !> distval フォーマットの入力（浮動小数点数型）
  subroutine monolis_input_distval_R(fname, label, n_node, n_dof, val)
    implicit none
    !> [in] 出力ファイル名
    character(*), intent(in) :: fname
    !> [out] ラベル名
    character(monolis_charlen), intent(out) :: label
    !> [out] 節点数
    integer(kint), intent(out) :: n_node
    !> [out] 計算点が持つ自由度
    integer(kint), intent(out) :: n_dof
    !> [out] データ
    real(kdouble), allocatable, intent(out) :: val(:,:)
    integer(kint) :: i, j

    open(20, file = trim(fname), status = "old")
      read(20,*) label
      read(20,*) n_node, n_dof

      call monolis_alloc_R_2d(val, n_dof, n_node)

      do i = 1, n_node
        read(20,*) (val(j,i), j = 1, n_dof)
      enddo
    close(20)
  end subroutine monolis_input_distval_R

  !> @ingroup io
  !> distval フォーマットの出力（浮動小数点数型）
  subroutine monolis_output_distval_R(fname, label, n_node, n_dof, val)
    implicit none
    !> [in] 出力ファイル名
    character(*), intent(in) :: fname
    !> [in] ラベル名
    character(*), intent(in) :: label
    !> [in] 節点数
    integer(kint), intent(in) :: n_node
    !> [in] 計算点が持つ自由度
    integer(kint), intent(in) :: n_dof
    !> [in] データ
    real(kdouble), intent(in) :: val(:,:)
    integer(kint) :: i, j

    open(20, file = trim(fname), status = "replace")
      write(20,"(a)") trim(label)
      write(20,"(i0,x,i0)") n_node, n_dof

      do i = 1, n_node
        do j = 1, n_dof
          write(20,"(1pe22.14,x,$)") val(j,i)
        enddo
        write(20,*)""
      enddo
    close(20)
  end subroutine monolis_output_distval_R

  !> @ingroup io
  !> distval フォーマットの入力（複素数型）
  subroutine monolis_input_distval_C(fname, label, n_node, n_dof, val)
    implicit none
    !> [in] 出力ファイル名
    character(*), intent(in) :: fname
    !> [out] ラベル名
    character(monolis_charlen), intent(out) :: label
    !> [out] 節点数
    integer(kint), intent(out) :: n_node
    !> [out] 計算点が持つ自由度
    integer(kint), intent(out) :: n_dof
    !> [out] データ
    complex(kdouble), allocatable, intent(out) :: val(:,:)
    real(kdouble), allocatable :: tmp(:)
    integer(kint) :: i, j

    open(20, file = trim(fname), status = "old")
      read(20,*) label
      read(20,*) n_node, n_dof

      call monolis_alloc_C_2d(val, n_dof, n_node)
      call monolis_alloc_R_1d(tmp, 2*n_dof)

      do i = 1, n_node
        read(20,*) (tmp(j), j = 1, 2*n_dof)
        do j = 1, n_dof
          val(j,i) = cmplx(tmp(2*j-1), tmp(2*j))
        enddo
      enddo
    close(20)
  end subroutine monolis_input_distval_C

  !> @ingroup io
  !> distval フォーマットの出力（複素数型）
  subroutine monolis_output_distval_C(fname, label, n_node, n_dof, val)
    implicit none
    !> [in] 出力ファイル名
    character(*), intent(in) :: fname
    !> [in] ラベル名
    character(*), intent(in) :: label
    !> [in] 節点数
    integer(kint), intent(in) :: n_node
    !> [in] 計算点が持つ自由度
    integer(kint), intent(in) :: n_dof
    !> [in] データ
    complex(kdouble), intent(in) :: val(:,:)
    integer(kint) :: i, j

    open(20, file = trim(fname), status = "replace")
      write(20,"(a)") trim(label)
      write(20,"(i0,x,i0)") n_node, n_dof

      do i = 1, n_node
        do j = 1, n_dof
          write(20,"(1pe22.14,x,1pe22.14,x,$)") real(val(j,i)), imag(val(j,i))
        enddo
        write(20,*)""
      enddo
    close(20)
  end subroutine monolis_output_distval_C

  !> @ingroup dev_io
  !> Fortran open 文のエラー処理
  subroutine monolis_input_file_error_check(ierr)
    implicit none
    !> [in] エラーステータス
    integer(kint), intent(in) :: ierr

    if(ierr /= 0)then
      !call monolis_error_string("file open")
      !call monolis_error_stop()
    endif
  end subroutine monolis_input_file_error_check
end module mod_monolis_io
