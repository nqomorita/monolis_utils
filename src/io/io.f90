!> IO モジュール
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
    character(monolis_charlen) :: fname
    !> [out] グラフノード数
    integer(kint) :: n_vertex
    !> [out] グラフノード id
    integer(kint), allocatable :: vertex_id(:)
    !> [out] グラフの CSR 圧縮形式の index 配列
    integer(kint), allocatable :: index(:)
    !> [out] グラフの CSR 圧縮形式の index 配列
    integer(kint), allocatable :: item(:)
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

    call monolis_alloc_int_1d(vertex_id, n_vertex)
    call monolis_alloc_int_1d(index, n_vertex+1)
    call monolis_alloc_int_1d(item, nz)

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
    character(monolis_charlen) :: fname
    !> [in] グラフノード数
    integer(kint) :: n_vertex
    !> [in] グラフノード id
    integer(kint), allocatable :: vertex_id(:)
    !> [in] グラフの CSR 圧縮形式の index 配列
    integer(kint), allocatable :: index(:)
    !> [in] グラフの CSR 圧縮形式の index 配列
    integer(kint), allocatable :: item(:)
    integer(kint) :: i, in, j, jS, jE

    open(20, file = trim(fname), status = "replace")
      write(20,"(i0)") n_vertex
      do i = 1, n_vertex
        jS = index(i) + 1
        jE = index(i+1)
        in = jE - jS + 1
        write(20,"(i0,x,i0,$)") i, in
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
    character(monolis_charlen) :: fname
    !> [out] 節点数
    integer(kint) :: n_node
    !> [out] 節点座標
    integer(kint), allocatable :: node(:,:)
    integer(kint) :: i

    open(20, file = trim(fname), status = "old")
      read(20,*) n_node
      call monolis_alloc_int_2d(node, 3, n_node)
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
    character(monolis_charlen) :: fname
    !> [in] 節点数
    integer(kint) :: n_node
    !> [in] 節点座標
    integer(kint) :: node(:,:)
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
    character(monolis_charlen) :: fname
    !> [out] 要素数
    integer(kint) :: n_elem
    !> [out] 基底の数
    integer(kint) :: n_base
    !> [out] 要素コネクティビティ
    integer(kint), allocatable :: elem(:,:)
    integer(kint) :: i, j

    open(20, file = trim(fname), status = "old")
      read(20,*) n_elem, n_base
      call monolis_alloc_int_2d(elem, n_base, n_elem)
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
    character(monolis_charlen) :: fname
    !> [in] 要素数
    integer(kint) :: n_elem
    !> [in] 基底の数
    integer(kint) :: n_base
    !> [in] 要素コネクティビティ
    integer(kint) :: elem(:,:)
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
  !> monolis 内部節点自由度数の入力
  subroutine monolis_input_internal_vertex_number(fname, n_internal_vertex)
    implicit none
    !> [in] 出力ファイル名
    character(monolis_charlen) :: fname
    !> [out] 内部自由度の数
    integer(kint) :: n_internal_vertex
    character(monolis_charlen) :: label

    open(20, file = trim(fname), status = "old")
      read(20,*) label
      read(20,*) n_internal_vertex
    close(20)
  end subroutine monolis_input_internal_vertex_number

  !> @ingroup io
  !> monolis 内部節点自由度数の出力
  subroutine monolis_output_internal_vertex_number(fname, n_internal_vertex)
    implicit none
    !> [in] 出力ファイル名
    character(monolis_charlen) :: fname
    !> [in] 内部自由度の数
    integer(kint) :: n_internal_vertex

    open(20, file = trim(fname), status = "replace")
      write(20,"(a)") "#n_internal_vertex"
      write(20,"(i0)") n_internal_vertex
    close(20)
  end subroutine monolis_output_internal_vertex_number

  !> @ingroup io
  !> bc フォーマットの入力
  subroutine monolis_input_bc(fname, n_bc, n_dof, i_bc, r_bc)
    implicit none
    !> [in] 出力ファイル名
    character(monolis_charlen) :: fname
    !> [out] 境界条件の数
    integer(kint) :: n_bc
    !> [out] 自由度数
    integer(kint) :: n_dof
    !> [out] 境界条件の付与番号と付与自由度
    integer(kint), allocatable :: i_bc(:,:)
    !> [out] 境界条件の値
    real(kdouble), allocatable :: r_bc(:)
    integer(kint) :: i

    open(20, file = trim(fname), status = "old")
      read(20,*) n_bc, n_dof

      call monolis_alloc_I_2d(i_bc, 2, n_bc)
      call monolis_alloc_R_1d(r_bc, n_bc)

      do i = 1, n_bc
        read(20,*) i_bc(1,i), i_bc(2,i), r_bc(i)
      enddo
    close(20)
  end subroutine monolis_input_bc

  !> @ingroup io
  !> bc フォーマットの出力
  subroutine monolis_output_bc(fname, n_bc, n_dof, i_bc, r_bc)
    implicit none
    !> [in] 出力ファイル名
    character(monolis_charlen) :: fname
    !> [in] 境界条件の数
    integer(kint) :: n_bc
    !> [in] 自由度数
    integer(kint) :: n_dof
    !> [in] 境界条件の付与番号と付与自由度
    integer(kint) :: i_bc(:,:)
    !> [in] 境界条件の値
    real(kdouble) :: r_bc(:)
    integer(kint) :: i

    open(20, file = trim(fname), status = "replace")
      write(20,"(i0,x,i0)") n_bc, n_dof
      do i = 1, n_bc
        write(20,"(i0,x,i0,x,1pe22.14)") i_bc(1,i), i_bc(2,i), r_bc(i)
      enddo
    close(20)
  end subroutine monolis_output_bc

  !> @ingroup dev_io
  !> Fortran open 文のエラー処理
  subroutine monolis_input_file_error_check(ierr)
    implicit none
    !> [in] エラーステータス
    integer(kint) :: ierr

    if(ierr /= 0)then
      !call monolis_error_string("file open")
      !call monolis_error_stop()
    endif
  end subroutine monolis_input_file_error_check
end module mod_monolis_io
