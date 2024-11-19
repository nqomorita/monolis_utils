!> MPI モジュール
module mod_monolis_mpi_sendrecv
  use mod_monolis_utils_define_prm
  use mod_monolis_mpi_util
  use mod_monolis_mpi
  implicit none

contains

  !> @ingroup mpi
  !> 通信テーブルを用いた send recv 関数（浮動小数点型、可変ブロックサイズ）
  subroutine monolis_SendRecv_V_R(send_n_neib, send_neib_pe, recv_n_neib, recv_neib_pe, &
    & send_index, send_item, recv_index, recv_item, &
    & val_in, val_out, n_dof_index_in, n_dof_index_out, comm)
    implicit none
    !> [in] send する隣接領域数
    integer(kint), intent(in) :: send_n_neib
    !> [in] send する隣接領域 id
    integer(kint), intent(in) :: send_neib_pe(:)
    !> [in] recv する隣接領域数
    integer(kint), intent(in) :: recv_n_neib
    !> [in] recv する隣接領域 id
    integer(kint), intent(in) :: recv_neib_pe(:)
    !> [in] send の index 配列
    integer(kint), intent(in) :: send_index(:)
    !> [in] send の item 配列（送信する節点番号データ）
    integer(kint), intent(in) :: send_item (:)
    !> [in] recv の index 配列
    integer(kint), intent(in) :: recv_index(:)
    !> [in] recv の item 配列（受信する節点番号データ）
    integer(kint), intent(in) :: recv_item (:)
    !> [in,out] 送信データ配列
    real(kdouble), intent(inout) :: val_in(:)
    !> [in,out] 受信データ配列
    real(kdouble), intent(inout) :: val_out(:)
    !> [in] 計算点が持つ自由度の index 配列
    integer(kint), intent(in) :: n_dof_index_in(:)
    !> [in] 計算点が持つ自由度の index 配列
    integer(kint), intent(in) :: n_dof_index_out(:)
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint) :: i, iS, iE, in, j, jn, jS, jE, k, kn, ierr, ns, nr, nd
    integer(kint) :: sta1(monolis_mpi_status_size, send_n_neib)
    integer(kint) :: sta2(monolis_mpi_status_size, recv_n_neib)
    integer(kint) :: req1(send_n_neib)
    integer(kint) :: req2(recv_n_neib)
    integer(kint) :: ws_index(send_n_neib + 1)
    integer(kint) :: wr_index(recv_n_neib + 1)
    real(kdouble), allocatable :: ws(:)
    real(kdouble), allocatable :: wr(:)

#ifndef NO_MPI
    ws_index = 0
    ns = 0
    do i = 1, send_n_neib
      jS = send_index(i) + 1
      jE = send_index(i + 1)
      do j = jS, jE
        in = send_item(j)
        if(in == -1) cycle
        nd = n_dof_index_in(in + 1) - n_dof_index_in(in)
        ns = ns + nd
      enddo
      ws_index(i + 1) = ns
    enddo

    wr_index = 0
    nr = 0
    do i = 1, recv_n_neib
      jS = recv_index(i) + 1
      jE = recv_index(i + 1)
      do j = jS, jE
        in = recv_item(j)
        if(in == -1) cycle
        nd = n_dof_index_out(in + 1) - n_dof_index_out(in)
        nr = nr + nd
      enddo
      wr_index(i + 1) = nr
    enddo

    call monolis_alloc_R_1d(ws, ns)
    call monolis_alloc_R_1d(wr, nr)

    kn = 0
    do i = 1, send_n_neib
      iS = send_index(i)
      iE = send_index(i + 1)
      in = iE - iS
      if(in == 0) cycle
      l1:do j = iS + 1, iE
        jn = send_item(j)
        if(jn == -1) cycle l1
        jS = n_dof_index_in(jn)
        jE = n_dof_index_in(jn + 1)
        nd = jE - jS
        do k = 1, nd
          kn = kn + 1
          ws(kn) = val_in(jS + k)
        enddo
      enddo l1
      iS = ws_index(i) + 1
      iE = ws_index(i + 1)
      call monolis_Isend_R(iE-iS+1, ws(iS:iE), send_neib_pe(i), comm, req1(i))
    enddo

    do i = 1, recv_n_neib
      iS = wr_index(i) + 1
      iE = wr_index(i + 1)
      in = iE - iS
      if(in == 0) cycle
      call monolis_Irecv_R(iE-iS+1, wr(iS:iE), recv_neib_pe(i), comm, req2(i))
    enddo

    call MPI_waitall(recv_n_neib, req2, sta2, ierr)

    kn = 0
    do i = 1, recv_index(recv_n_neib + 1)
      in = recv_item(i)
      if(in == -1) cycle
      jS = n_dof_index_out(in)
      jE = n_dof_index_out(in + 1)
      nd = jE - jS
      do j = 1, nd
        kn = kn + 1
        val_out(jS + j) = wr(kn)
      enddo
    enddo

    call MPI_waitall(send_n_neib, req1, sta1, ierr)
#endif
  end subroutine monolis_SendRecv_V_R

  subroutine monolis_SendRecv_V_R1(com, send_n_dof, recv_n_dof, send_id, recv_id)
    implicit none
    type(monolis_COM), intent(inout) :: com
    integer(kint) :: i
    real(kdouble) :: send_id(:)
    real(kdouble) :: recv_id(:)
    integer(kint) :: send_n_dof(:)
    integer(kint) :: recv_n_dof(:)
    integer(kint), allocatable :: send_index(:)
    integer(kint), allocatable :: send_item(:)
    integer(kint), allocatable :: send_n_dof_index(:)
    integer(kint), allocatable :: recv_index(:)
    integer(kint), allocatable :: recv_item(:)
    integer(kint), allocatable :: recv_n_dof_index(:)

    call monolis_alloc_I_1d(send_index, com%send_n_neib + 1)
    call monolis_alloc_I_1d(send_item,  com%send_n_neib)
    call monolis_alloc_I_1d(send_n_dof_index, com%send_n_neib + 1)
    call monolis_alloc_I_1d(recv_index, com%recv_n_neib + 1)
    call monolis_alloc_I_1d(recv_item,  com%recv_n_neib)
    call monolis_alloc_I_1d(recv_n_dof_index, com%recv_n_neib + 1)

    do i = 1, com%send_n_neib
      send_index(i + 1) = i
    enddo

    do i = 1, com%send_n_neib
      send_item(i) = i
    enddo

    do i = 1, com%recv_n_neib
      recv_index(i + 1) = i
    enddo

    do i = 1, com%recv_n_neib
      recv_item(i) = i
    enddo

    call monolis_get_ndof_index_from_ndof_list(com%send_n_neib, send_n_dof, send_n_dof_index)
    call monolis_get_ndof_index_from_ndof_list(com%recv_n_neib, recv_n_dof, recv_n_dof_index)

    call monolis_SendRecv_V_R(com%send_n_neib, com%send_neib_pe, &
       & com%recv_n_neib, com%recv_neib_pe, &
       & send_index, send_item, recv_index, recv_item, &
       & send_id, recv_id, send_n_dof_index, recv_n_dof_index, com%comm)
  end subroutine monolis_SendRecv_V_R1

  !> @ingroup mpi
  !> 通信テーブルを用いた send recv 関数（浮動小数点型）
  subroutine monolis_SendRecv_R(send_n_neib, send_neib_pe, recv_n_neib, recv_neib_pe, &
    & send_index, send_item, recv_index, recv_item, &
    & val_in, val_out, ndof, comm)
    implicit none
    !> [in] send する隣接領域数
    integer(kint), intent(in) :: send_n_neib
    !> [in] send する隣接領域 id
    integer(kint), intent(in) :: send_neib_pe(:)
    !> [in] recv する隣接領域数
    integer(kint), intent(in) :: recv_n_neib
    !> [in] recv する隣接領域 id
    integer(kint), intent(in) :: recv_neib_pe(:)
    !> [in] send の index 配列
    integer(kint), intent(in) :: send_index(:)
    !> [in] send の item 配列（送信する節点番号データ）
    integer(kint), intent(in) :: send_item (:)
    !> [in] recv の index 配列
    integer(kint), intent(in) :: recv_index(:)
    !> [in] recv の item 配列（受信する節点番号データ）
    integer(kint), intent(in) :: recv_item (:)
    !> [in,out] 送信データ配列
    real(kdouble), intent(inout) :: val_in(:)
    !> [in,out] 受信データ配列
    real(kdouble), intent(inout) :: val_out(:)
    !> [in] 計算点が持つ自由度
    integer(kint), intent(in) :: ndof
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint) :: i, iS, in, j, k, ierr, ns, nr
    integer(kint) :: sta1(monolis_mpi_status_size, send_n_neib)
    integer(kint) :: sta2(monolis_mpi_status_size, recv_n_neib)
    integer(kint) :: req1(send_n_neib)
    integer(kint) :: req2(recv_n_neib)
    real(kdouble), allocatable :: ws(:)
    real(kdouble), allocatable :: wr(:)

#ifndef NO_MPI
    ns = send_index(send_n_neib + 1)
    nr = recv_index(recv_n_neib + 1)

    call monolis_alloc_R_1d(ws, ndof*ns)
    call monolis_alloc_R_1d(wr, ndof*nr)

    do i = 1, send_n_neib
      iS = send_index(i)
      in = send_index(i + 1) - iS
      if(in == 0) cycle
      l1:do j = iS + 1, iS + in
        if(send_item(j) == -1) cycle l1
        do k = 1, ndof
          ws(ndof*(j - 1) + k) = val_in(ndof*(send_item(j) - 1) + k)
        enddo
      enddo l1
      call monolis_Isend_R(ndof*in, ws(ndof*iS + 1:ndof*iS + ndof*in), send_neib_pe(i), comm, req1(i))
    enddo

    do i = 1, recv_n_neib
      iS = recv_index(i)
      in = recv_index(i + 1) - iS
      if(in == 0) cycle
      call monolis_Irecv_R(ndof*in, wr(ndof*iS + 1:ndof*iS + ndof*in), recv_neib_pe(i), comm, req2(i))
    enddo

    call MPI_waitall(recv_n_neib, req2, sta2, ierr)

    do i = 1, nr
      in = recv_item(i)
      if(in == -1) cycle
      do j = 1, ndof
        val_out(ndof*(in - 1) + j) = wr(ndof*(i - 1) + j)
      enddo
    enddo

    call MPI_waitall(send_n_neib, req1, sta1, ierr)
#endif
  end subroutine monolis_SendRecv_R

  !> @ingroup mpi
  !> 通信テーブルを用いた send recv reverse 関数（浮動小数点型）
  subroutine monolis_SendRecv_reverse_R(send_n_neib, send_neib_pe, recv_n_neib, recv_neib_pe, &
    & send_index, send_item, recv_index, recv_item, &
    & val_in, val_out, ndof, comm)
    implicit none
    !> [in] send する隣接領域数
    integer(kint), intent(in) :: send_n_neib
    !> [in] send する隣接領域 id
    integer(kint), intent(in) :: send_neib_pe(:)
    !> [in] recv する隣接領域数
    integer(kint), intent(in) :: recv_n_neib
    !> [in] recv する隣接領域 id
    integer(kint), intent(in) :: recv_neib_pe(:)
    !> [in] send の index 配列
    integer(kint), intent(in) :: send_index(:)
    !> [in] send の item 配列（送信する節点番号データ）
    integer(kint), intent(in) :: send_item (:)
    !> [in] recv の index 配列
    integer(kint), intent(in) :: recv_index(:)
    !> [in] recv の item 配列（受信する節点番号データ）
    integer(kint), intent(in) :: recv_item (:)
    !> [in,out] 送信データ配列
    real(kdouble), intent(inout) :: val_in(:)
    !> [in,out] 受信データ配列
    real(kdouble), intent(inout) :: val_out(:)
    !> [in] 計算点が持つ自由度
    integer(kint), intent(in) :: ndof
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint) :: i, iS, in, j, k, ierr, ns, nr
    integer(kint) :: sta1(monolis_mpi_status_size, send_n_neib)
    integer(kint) :: sta2(monolis_mpi_status_size, recv_n_neib)
    integer(kint) :: req1(send_n_neib)
    integer(kint) :: req2(recv_n_neib)
    real(kdouble), allocatable :: ws(:)
    real(kdouble), allocatable :: wr(:)

#ifndef NO_MPI
    ns = send_index(send_n_neib + 1)
    nr = recv_index(recv_n_neib + 1)

    call monolis_alloc_R_1d(ws, ndof*ns)
    call monolis_alloc_R_1d(wr, ndof*nr)

    do i = 1, recv_n_neib
      iS = recv_index(i)
      in = recv_index(i + 1) - iS
      if(in == 0) cycle
      l1:do j = iS + 1, iS + in
        if(recv_item(j) == -1) cycle l1
        do k = 1, ndof
          wr(ndof*(j - 1) + k) = val_in(ndof*(recv_item(j) - 1) + k)
        enddo
      enddo l1
      call monolis_Isend_R(ndof*in, wr(ndof*iS + 1:ndof*iS + ndof*in), recv_neib_pe(i), comm, req1(i))
    enddo

    do i = 1, send_n_neib
      iS = send_index(i)
      in = send_index(i + 1) - iS
      if(in == 0) cycle
      call monolis_Irecv_R(ndof*in, ws(ndof*iS + 1:ndof*iS + ndof*in), send_neib_pe(i), comm, req2(i))
    enddo

    call MPI_waitall(recv_n_neib, req2, sta2, ierr)

    val_out = 0.0d0

    do i = 1, ns
      in = send_item(i)
      if(in == -1) cycle
      do j = 1, ndof
        val_out(ndof*(in - 1) + j) = val_out(ndof*(in - 1) + j) + ws(ndof*(i - 1) + j)
      enddo
    enddo

    call MPI_waitall(send_n_neib, req1, sta1, ierr)
#endif
  end subroutine monolis_SendRecv_reverse_R

  !> @ingroup mpi
  !> 通信テーブルを用いた send recv 関数（整数型、可変ブロックサイズ）
  subroutine monolis_SendRecv_V_I(send_n_neib, send_neib_pe, recv_n_neib, recv_neib_pe, &
    & send_index, send_item, recv_index, recv_item, &
    & val_in, val_out, n_dof_index_in, n_dof_index_out, comm)
    implicit none
    !> [in] send する隣接領域数
    integer(kint), intent(in) :: send_n_neib
    !> [in] send する隣接領域 id
    integer(kint), intent(in) :: send_neib_pe(:)
    !> [in] recv する隣接領域数
    integer(kint), intent(in) :: recv_n_neib
    !> [in] recv する隣接領域 id
    integer(kint), intent(in) :: recv_neib_pe(:)
    !> [in] send の index 配列
    integer(kint), intent(in) :: send_index(:)
    !> [in] send の item 配列（送信する節点番号データ）
    integer(kint), intent(in) :: send_item (:)
    !> [in] recv の index 配列
    integer(kint), intent(in) :: recv_index(:)
    !> [in] recv の item 配列（受信する節点番号データ）
    integer(kint), intent(in) :: recv_item (:)
    !> [in,out] 送信データ配列
    integer(kint), intent(inout) :: val_in(:)
    !> [in,out] 受信データ配列
    integer(kint), intent(inout) :: val_out(:)
    !> [in] 計算点が持つ自由度の index 配列
    integer(kint), intent(in) :: n_dof_index_in(:)
    !> [in] 計算点が持つ自由度の index 配列
    integer(kint), intent(in) :: n_dof_index_out(:)
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint) :: i, iS, iE, in, j, jn, jS, jE, k, kn, ierr, ns, nr, nd
    integer(kint) :: sta1(monolis_mpi_status_size, send_n_neib)
    integer(kint) :: sta2(monolis_mpi_status_size, recv_n_neib)
    integer(kint) :: req1(send_n_neib)
    integer(kint) :: req2(recv_n_neib)
    integer(kint) :: ws_index(send_n_neib + 1)
    integer(kint) :: wr_index(recv_n_neib + 1)
    integer(kint), allocatable :: ws(:)
    integer(kint), allocatable :: wr(:)

#ifndef NO_MPI
    ws_index = 0
    ns = 0
    do i = 1, send_n_neib
      jS = send_index(i) + 1
      jE = send_index(i + 1)
      do j = jS, jE
        in = send_item(j)
        if(in == -1) cycle
        nd = n_dof_index_in(in + 1) - n_dof_index_in(in)
        ns = ns + nd
      enddo
      ws_index(i + 1) = ns
    enddo

    wr_index = 0
    nr = 0
    do i = 1, recv_n_neib
      jS = recv_index(i) + 1
      jE = recv_index(i + 1)
      do j = jS, jE
        in = recv_item(j)
        if(in == -1) cycle
        nd = n_dof_index_out(in + 1) - n_dof_index_out(in)
        nr = nr + nd
      enddo
      wr_index(i + 1) = nr
    enddo

    call monolis_alloc_I_1d(ws, ns)
    call monolis_alloc_I_1d(wr, nr)

    kn = 0
    do i = 1, send_n_neib
      iS = send_index(i)
      iE = send_index(i + 1)
      in = iE - iS
      if(in == 0) cycle
      l1:do j = iS + 1, iE
        jn = send_item(j)
        if(jn == -1) cycle l1
        jS = n_dof_index_in(jn)
        jE = n_dof_index_in(jn + 1)
        nd = jE - jS
        do k = 1, nd
          kn = kn + 1
          ws(kn) = val_in(jS + k)
        enddo
      enddo l1
      iS = ws_index(i) + 1
      iE = ws_index(i + 1)
      call monolis_Isend_I(iE-iS+1, ws(iS:iE), send_neib_pe(i), comm, req1(i))
    enddo

    do i = 1, recv_n_neib
      iS = wr_index(i) + 1
      iE = wr_index(i + 1)
      in = iE - iS
      if(in == 0) cycle
      call monolis_Irecv_I(iE-iS+1, wr(iS:iE), recv_neib_pe(i), comm, req2(i))
    enddo

    call MPI_waitall(recv_n_neib, req2, sta2, ierr)

    kn = 0
    do i = 1, recv_index(recv_n_neib + 1)
      in = recv_item(i)
      if(in == -1) cycle
      jS = n_dof_index_out(in)
      jE = n_dof_index_out(in + 1)
      nd = jE - jS
      do j = 1, nd
        kn = kn + 1
        val_out(jS + j) = wr(kn)
      enddo
    enddo

    call MPI_waitall(send_n_neib, req1, sta1, ierr)
#endif
  end subroutine monolis_SendRecv_V_I

  subroutine monolis_SendRecv_V_I1(com, send_n_dof, recv_n_dof, send_id, recv_id)
    implicit none
    type(monolis_COM), intent(inout) :: com
    integer(kint) :: i
    integer(kint) :: send_id(:)
    integer(kint) :: recv_id(:)
    integer(kint) :: send_n_dof(:)
    integer(kint) :: recv_n_dof(:)
    integer(kint), allocatable :: send_index(:)
    integer(kint), allocatable :: send_item(:)
    integer(kint), allocatable :: send_n_dof_index(:)
    integer(kint), allocatable :: recv_index(:)
    integer(kint), allocatable :: recv_item(:)
    integer(kint), allocatable :: recv_n_dof_index(:)

    call monolis_alloc_I_1d(send_index, com%send_n_neib + 1)
    call monolis_alloc_I_1d(send_item,  com%send_n_neib)
    call monolis_alloc_I_1d(send_n_dof_index, com%send_n_neib + 1)
    call monolis_alloc_I_1d(recv_index, com%recv_n_neib + 1)
    call monolis_alloc_I_1d(recv_item,  com%recv_n_neib)
    call monolis_alloc_I_1d(recv_n_dof_index,  com%recv_n_neib + 1)

    do i = 1, com%send_n_neib
      send_index(i + 1) = i
    enddo

    do i = 1, com%send_n_neib
      send_item(i) = i
    enddo

    do i = 1, com%recv_n_neib
      recv_index(i + 1) = i
    enddo

    do i = 1, com%recv_n_neib
      recv_item(i) = i
    enddo

    call monolis_get_ndof_index_from_ndof_list(com%send_n_neib, send_n_dof, send_n_dof_index)
    call monolis_get_ndof_index_from_ndof_list(com%recv_n_neib, recv_n_dof, recv_n_dof_index)

    call monolis_SendRecv_V_I(com%send_n_neib, com%send_neib_pe, &
       & com%recv_n_neib, com%recv_neib_pe, &
       & send_index, send_item, recv_index, recv_item, &
       & send_id, recv_id, send_n_dof_index, recv_n_dof_index, com%comm)
  end subroutine monolis_SendRecv_V_I1

  !> @ingroup mpi
  !> 通信テーブルを用いた send recv 関数（整数型）
  subroutine monolis_SendRecv_I(send_n_neib, send_neib_pe, recv_n_neib, recv_neib_pe, &
    & send_index, send_item, recv_index, recv_item, &
    & val_in, val_out, ndof, comm)
    implicit none
    !> [in] send する隣接領域数
    integer(kint), intent(in) :: send_n_neib
    !> [in] send する隣接領域 id
    integer(kint), intent(in) :: send_neib_pe(:)
    !> [in] recv する隣接領域数
    integer(kint), intent(in) :: recv_n_neib
    !> [in] recv する隣接領域 id
    integer(kint), intent(in) :: recv_neib_pe(:)
    !> [in] send の index 配列
    integer(kint), intent(in) :: send_index(:)
    !> [in] send の item 配列（送信する節点番号データ）
    integer(kint), intent(in) :: send_item (:)
    !> [in] recv の index 配列
    integer(kint), intent(in) :: recv_index(:)
    !> [in] recv の item 配列（受信する節点番号データ）
    integer(kint), intent(in) :: recv_item (:)
    !> [in,out] 送信データ配列
    integer(kint), intent(inout) :: val_in(:)
    !> [in,out] 受信データ配列
    integer(kint), intent(inout) :: val_out(:)
    !> [in] 計算点が持つ自由度
    integer(kint), intent(in) :: ndof
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint) :: i, iS, in, j, k, ierr, ns, nr
    integer(kint) :: sta1(monolis_mpi_status_size, send_n_neib)
    integer(kint) :: sta2(monolis_mpi_status_size, recv_n_neib)
    integer(kint) :: req1(send_n_neib)
    integer(kint) :: req2(recv_n_neib)
    integer(kint), allocatable :: ws(:)
    integer(kint), allocatable :: wr(:)

#ifndef NO_MPI
    ns = send_index(send_n_neib + 1)
    nr = recv_index(recv_n_neib + 1)

    call monolis_alloc_I_1d(ws, ndof*ns)
    call monolis_alloc_I_1d(wr, ndof*nr)

    do i = 1, send_n_neib
      iS = send_index(i)
      in = send_index(i + 1) - iS
      if(in == 0) cycle
      l1:do j = iS + 1, iS + in
        if(send_item(j) == -1) cycle l1
        do k = 1, ndof
          ws(ndof*(j - 1) + k) = val_in(ndof*(send_item(j) - 1) + k)
        enddo
      enddo l1
      call monolis_Isend_I(ndof*in, ws(ndof*iS + 1:ndof*iS + ndof*in), send_neib_pe(i), comm, req1(i))
    enddo

    do i = 1, recv_n_neib
      iS = recv_index(i)
      in = recv_index(i + 1) - iS
      if(in == 0) cycle
      call monolis_Irecv_I(ndof*in, wr(ndof*iS + 1:ndof*iS + ndof*in), recv_neib_pe(i), comm, req2(i))
    enddo

    call MPI_waitall(recv_n_neib, req2, sta2, ierr)

    do i = 1, nr
      in = recv_item(i)
      if(in == -1) cycle
      do j = 1, ndof
        val_out(ndof*(in - 1) + j) = wr(ndof*(i - 1) + j)
      enddo
    enddo

    call MPI_waitall(send_n_neib, req1, sta1, ierr)
#endif
  end subroutine monolis_SendRecv_I

  !> @ingroup mpi
  !> 通信テーブルを用いた send recv 関数（整数型、可変ブロックサイズ）
  subroutine monolis_SendRecv_V_C(send_n_neib, send_neib_pe, recv_n_neib, recv_neib_pe, &
    & send_index, send_item, recv_index, recv_item, &
    & val_in, val_out, n_dof_index_in, n_dof_index_out, comm)
    implicit none
    !> [in] send する隣接領域数
    integer(kint), intent(in) :: send_n_neib
    !> [in] send する隣接領域 id
    integer(kint), intent(in) :: send_neib_pe(:)
    !> [in] recv する隣接領域数
    integer(kint), intent(in) :: recv_n_neib
    !> [in] recv する隣接領域 id
    integer(kint), intent(in) :: recv_neib_pe(:)
    !> [in] send の index 配列
    integer(kint), intent(in) :: send_index(:)
    !> [in] send の item 配列（送信する節点番号データ）
    integer(kint), intent(in) :: send_item (:)
    !> [in] recv の index 配列
    integer(kint), intent(in) :: recv_index(:)
    !> [in] recv の item 配列（受信する節点番号データ）
    integer(kint), intent(in) :: recv_item (:)
    !> [in,out] 送信データ配列
    complex(kdouble), intent(inout) :: val_in(:)
    !> [in,out] 受信データ配列
    complex(kdouble), intent(inout) :: val_out(:)
    !> [in] 計算点が持つ自由度の index 配列
    integer(kint), intent(in) :: n_dof_index_in(:)
    !> [in] 計算点が持つ自由度の index 配列
    integer(kint), intent(in) :: n_dof_index_out(:)
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint) :: i, iS, iE, in, j, jn, jS, jE, k, kn, ierr, ns, nr, nd
    integer(kint) :: sta1(monolis_mpi_status_size, send_n_neib)
    integer(kint) :: sta2(monolis_mpi_status_size, recv_n_neib)
    integer(kint) :: req1(send_n_neib)
    integer(kint) :: req2(recv_n_neib)
    integer(kint) :: ws_index(send_n_neib + 1)
    integer(kint) :: wr_index(recv_n_neib + 1)
    complex(kdouble), allocatable :: ws(:)
    complex(kdouble), allocatable :: wr(:)

#ifndef NO_MPI
    ws_index = 0
    ns = 0
    do i = 1, send_n_neib
      jS = send_index(i) + 1
      jE = send_index(i + 1)
      do j = jS, jE
        in = send_item(j)
        if(in == -1) cycle
        nd = n_dof_index_in(in + 1) - n_dof_index_in(in)
        ns = ns + nd
      enddo
      ws_index(i + 1) = ns
    enddo

    wr_index = 0
    nr = 0
    do i = 1, recv_n_neib
      jS = recv_index(i) + 1
      jE = recv_index(i + 1)
      do j = jS, jE
        in = recv_item(j)
        if(in == -1) cycle
        nd = n_dof_index_out(in + 1) - n_dof_index_out(in)
        nr = nr + nd
      enddo
      wr_index(i + 1) = nr
    enddo

    call monolis_alloc_C_1d(ws, ns)
    call monolis_alloc_C_1d(wr, nr)

    kn = 0
    do i = 1, send_n_neib
      iS = send_index(i)
      iE = send_index(i + 1)
      in = iE - iS
      if(in == 0) cycle
      l1:do j = iS + 1, iE
        jn = send_item(j)
        if(jn == -1) cycle l1
        jS = n_dof_index_in(jn)
        jE = n_dof_index_in(jn + 1)
        nd = jE - jS
        do k = 1, nd
          kn = kn + 1
          ws(kn) = val_in(jS + k)
        enddo
      enddo l1
      iS = ws_index(i) + 1
      iE = ws_index(i + 1)
      call monolis_Isend_C(iE-iS+1, ws(iS:iE), send_neib_pe(i), comm, req1(i))
    enddo

    do i = 1, recv_n_neib
      iS = wr_index(i) + 1
      iE = wr_index(i + 1)
      in = iE - iS
      if(in == 0) cycle
      call monolis_Irecv_C(iE-iS+1, wr(iS:iE), recv_neib_pe(i), comm, req2(i))
    enddo

    call MPI_waitall(recv_n_neib, req2, sta2, ierr)

    kn = 0
    do i = 1, recv_index(recv_n_neib + 1)
      in = recv_item(i)
      if(in == -1) cycle
      jS = n_dof_index_out(in)
      jE = n_dof_index_out(in + 1)
      nd = jE - jS
      do j = 1, nd
        kn = kn + 1
        val_out(jS + j) = wr(kn)
      enddo
    enddo

    call MPI_waitall(send_n_neib, req1, sta1, ierr)
#endif
  end subroutine monolis_SendRecv_V_C

  !> @ingroup mpi
  !> 通信テーブルを用いた send recv 関数（複素数型）
  subroutine monolis_SendRecv_C(send_n_neib, send_neib_pe, recv_n_neib, recv_neib_pe, &
    & send_index, send_item, recv_index, recv_item, &
    & val_in, val_out, ndof, comm)
    implicit none
    !> [in] send する隣接領域数
    integer(kint), intent(in) :: send_n_neib
    !> [in] send する隣接領域 id
    integer(kint), intent(in) :: send_neib_pe(:)
    !> [in] recv する隣接領域数
    integer(kint), intent(in) :: recv_n_neib
    !> [in] recv する隣接領域 id
    integer(kint), intent(in) :: recv_neib_pe(:)
    !> [in] send の index 配列
    integer(kint), intent(in) :: send_index(:)
    !> [in] send の item 配列（送信する節点番号データ）
    integer(kint), intent(in) :: send_item (:)
    !> [in] recv の index 配列
    integer(kint), intent(in) :: recv_index(:)
    !> [in] recv の item 配列（受信する節点番号データ）
    integer(kint), intent(in) :: recv_item (:)
    !> [in,out] 送信データ配列
    complex(kdouble), intent(inout) :: val_in(:)
    !> [in,out] 受信データ配列
    complex(kdouble), intent(inout) :: val_out(:)
    !> [in] 計算点が持つ自由度
    integer(kint), intent(in) :: ndof
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint) :: i, iS, in, j, k, ierr, ns, nr
    integer(kint) :: sta1(monolis_mpi_status_size, send_n_neib)
    integer(kint) :: sta2(monolis_mpi_status_size, recv_n_neib)
    integer(kint) :: req1(send_n_neib)
    integer(kint) :: req2(recv_n_neib)
    complex(kdouble), allocatable :: ws(:)
    complex(kdouble), allocatable :: wr(:)

#ifndef NO_MPI
    ns = send_index(send_n_neib + 1)
    nr = recv_index(recv_n_neib + 1)

    call monolis_alloc_C_1d(ws, ndof*ns)
    call monolis_alloc_C_1d(wr, ndof*nr)

    do i = 1, send_n_neib
      iS = send_index(i)
      in = send_index(i + 1) - iS
      if(in == 0) cycle
      l1:do j = iS + 1, iS + in
        if(send_item(j) == -1) cycle l1
        do k = 1, ndof
          ws(ndof*(j - 1) + k) = val_in(ndof*(send_item(j) - 1) + k)
        enddo
      enddo l1
      call monolis_Isend_C(ndof*in, ws(ndof*iS + 1:ndof*iS + ndof*in), send_neib_pe(i), comm, req1(i))
    enddo

    do i = 1, recv_n_neib
      iS = recv_index(i)
      in = recv_index(i + 1) - iS
      if(in == 0) cycle
      call monolis_Irecv_C(ndof*in, wr(ndof*iS + 1:ndof*iS + ndof*in), recv_neib_pe(i), comm, req2(i))
    enddo

    call MPI_waitall(recv_n_neib, req2, sta2, ierr)

    do i = 1, nr
      in = recv_item(i)
      if(in == -1) cycle
      do j = 1, ndof
        val_out(ndof*(in - 1) + j) = wr(ndof*(i - 1) + j)
      enddo
    enddo

    call MPI_waitall(send_n_neib, req1, sta1, ierr)
#endif
  end subroutine monolis_SendRecv_C

  !# monoCOM 構造体を引数にとる関数群（隣接領域情報が必要）

  !> @ingroup mpi
  !> ベクトルのアップデート関数（実数型、可変ブロックサイズ）
  subroutine monolis_mpi_update_V_R(monoCOM, n, n_dof_list, X, tcomm)
    implicit none
    !> [in] COM 構造体
    type(monolis_com), intent(in) :: monoCOM
    !> [in] 全計算点数
    integer(kint), intent(in) :: n
    !> [in] 計算点が持つ自由度
    integer(kint), intent(in) :: n_dof_list(:)
    !> [in,out] 入出力ベクトル
    real(kdouble), intent(inout) :: X(:)
    !> [in,out] 通信時間
    real(kdouble), optional, intent(inout) :: tcomm
    real(kdouble) :: t1, t2
    integer(kint), allocatable :: index_temp(:)

    if(monoCOM%send_n_neib == 0 .and. monoCOM%recv_n_neib == 0) return

    t1 = monolis_get_time()
    call monolis_alloc_I_1d(index_temp, n + 1)
    call monolis_get_ndof_index_from_ndof_list(n, n_dof_list, index_temp)
    call monolis_mpi_update_V_R_main(monoCOM, index_temp, X, tcomm)
    t2 = monolis_get_time()

    if(present(tcomm))then
      tcomm = tcomm + t2 - t1
    endif
  end subroutine monolis_mpi_update_V_R

  !> @ingroup dev_mpi
  !> ベクトルのアップデート関数（実数型、可変ブロックサイズ）
  subroutine monolis_mpi_update_V_R_main(monoCOM, n_dof_index, X, tcomm)
    implicit none
    !> [in] COM 構造体
    type(monolis_com), intent(in) :: monoCOM
    !> [in] 計算点が持つ自由度
    integer(kint), intent(in) :: n_dof_index(:)
    !> [in,out] 入出力ベクトル
    real(kdouble), intent(inout) :: X(:)
    !> [in,out] 通信時間
    real(kdouble), optional, intent(inout) :: tcomm
    real(kdouble) :: t1, t2

    if(monoCOM%send_n_neib == 0 .and. monoCOM%recv_n_neib == 0) return

    t1 = monolis_get_time()
    call monolis_SendRecv_V_R(monoCOM%send_n_neib, monoCOM%send_neib_pe, &
       & monoCOM%recv_n_neib, monoCOM%recv_neib_pe, &
       & monoCOM%send_index, monoCOM%send_item, &
       & monoCOM%recv_index, monoCOM%recv_item, &
       & X, X, n_dof_index, n_dof_index, monoCOM%comm)
    t2 = monolis_get_time()

    if(present(tcomm))then
      tcomm = tcomm + t2 - t1
    endif
  end subroutine monolis_mpi_update_V_R_main

  !> @ingroup mpi
  !> ベクトルのアップデート関数（実数型）
  subroutine monolis_mpi_update_R(monoCOM, ndof, X, tcomm)
    implicit none
    !> [in] COM 構造体
    type(monolis_com), intent(in) :: monoCOM
    !> [in] 計算点が持つ自由度
    integer(kint), intent(in) :: ndof
    !> [in,out] 入出力ベクトル
    real(kdouble), intent(inout) :: X(:)
    !> [in,out] 通信時間
    real(kdouble), optional, intent(inout) :: tcomm
    real(kdouble) :: t1, t2

    if(monoCOM%send_n_neib == 0 .and. monoCOM%recv_n_neib == 0) return

    t1 = monolis_get_time()
    call monolis_SendRecv_R(monoCOM%send_n_neib, monoCOM%send_neib_pe, &
       & monoCOM%recv_n_neib, monoCOM%recv_neib_pe, &
       & monoCOM%send_index, monoCOM%send_item, &
       & monoCOM%recv_index, monoCOM%recv_item, &
       & X, X, ndof, monoCOM%comm)
    t2 = monolis_get_time()

    if(present(tcomm))then
      tcomm = tcomm + t2 - t1
    endif
  end subroutine monolis_mpi_update_R

  !> @ingroup mpi
  !> ベクトルのアップデート関数（実数型）
  subroutine monolis_mpi_update_reverse_R(monoCOM, ndof, X, tcomm)
    implicit none
    !> [in] COM 構造体
    type(monolis_com), intent(in) :: monoCOM
    !> [in] 計算点が持つ自由度
    integer(kint), intent(in) :: ndof
    !> [in,out] 入出力ベクトル
    real(kdouble), intent(inout) :: X(:)
    !> [in,out] 通信時間
    real(kdouble), optional, intent(inout) :: tcomm
    real(kdouble) :: t1, t2

    if(monoCOM%send_n_neib == 0 .and. monoCOM%recv_n_neib == 0) return
    if(monoCOM%comm_size == 1) return

    t1 = monolis_get_time()
    call monolis_SendRecv_reverse_R(monoCOM%send_n_neib, monoCOM%send_neib_pe, &
       & monoCOM%recv_n_neib, monoCOM%recv_neib_pe, &
       & monoCOM%send_index, monoCOM%send_item, &
       & monoCOM%recv_index, monoCOM%recv_item, &
       & X, X, ndof, monoCOM%comm)
    t2 = monolis_get_time()

    if(present(tcomm))then
      tcomm = tcomm + t2 - t1
    endif
  end subroutine monolis_mpi_update_reverse_R

  !> @ingroup mpi
  !> ベクトルのアップデート関数（整数型）
  subroutine monolis_mpi_update_V_I(monoCOM, n, n_dof_index, X, tcomm)
    implicit none
    !> [in] COM 構造体
    type(monolis_com), intent(in) :: monoCOM
    !> [in] 全計算点数
    integer(kint), intent(in) :: n
    !> [in] 計算点が持つ自由度
    integer(kint), intent(in) :: n_dof_index(:)
    !> [in,out] 入出力ベクトル
    integer(kint), intent(inout) :: X(:)
    !> [in,out] 通信時間
    real(kdouble), optional, intent(inout) :: tcomm
    real(kdouble) :: t1, t2
    integer(kint), allocatable :: index_temp(:)

    if(monoCOM%send_n_neib == 0 .and. monoCOM%recv_n_neib == 0) return

    t1 = monolis_get_time()
    call monolis_alloc_I_1d(index_temp, n + 1)
    call monolis_get_ndof_index_from_ndof_list(n, n_dof_index, index_temp)
    call monolis_mpi_update_V_I_main(monoCOM, index_temp, X, tcomm)
    t2 = monolis_get_time()

    if(present(tcomm))then
      tcomm = tcomm + t2 - t1
    endif
  end subroutine monolis_mpi_update_V_I

  !> @ingroup dev_mpi
  !> ベクトルのアップデート関数（整数型）
  subroutine monolis_mpi_update_V_I_main(monoCOM, n_dof_index, X, tcomm)
    implicit none
    !> [in] COM 構造体
    type(monolis_com), intent(in) :: monoCOM
    !> [in] 計算点が持つ自由度
    integer(kint), intent(in) :: n_dof_index(:)
    !> [in,out] 入出力ベクトル
    integer(kint), intent(inout) :: X(:)
    !> [in,out] 通信時間
    real(kdouble), optional, intent(inout) :: tcomm
    real(kdouble) :: t1, t2

    if(monoCOM%send_n_neib == 0 .and. monoCOM%recv_n_neib == 0) return

    t1 = monolis_get_time()
    call monolis_SendRecv_V_I(monoCOM%send_n_neib, monoCOM%send_neib_pe, &
       & monoCOM%recv_n_neib, monoCOM%recv_neib_pe, &
       & monoCOM%send_index, monoCOM%send_item, &
       & monoCOM%recv_index, monoCOM%recv_item, &
       & X, X, n_dof_index, n_dof_index, monoCOM%comm)
    t2 = monolis_get_time()

    if(present(tcomm))then
      tcomm = tcomm + t2 - t1
    endif
  end subroutine monolis_mpi_update_V_I_main

  !> @ingroup mpi
  !> ベクトルのアップデート関数（整数型）
  subroutine monolis_mpi_update_I(monoCOM, ndof, X, tcomm)
    implicit none
    !> [in] COM 構造体
    type(monolis_com), intent(in) :: monoCOM
    !> [in] 計算点が持つ自由度
    integer(kint), intent(in) :: ndof
    !> [in,out] 入出力ベクトル
    integer(kint), intent(inout) :: X(:)
    !> [in,out] 通信時間
    real(kdouble), optional, intent(inout) :: tcomm
    real(kdouble) :: t1, t2

    if(monoCOM%send_n_neib == 0 .and. monoCOM%recv_n_neib == 0) return

    t1 = monolis_get_time()
    call monolis_SendRecv_I(monoCOM%send_n_neib, monoCOM%send_neib_pe, &
       & monoCOM%recv_n_neib, monoCOM%recv_neib_pe, &
       & monoCOM%send_index, monoCOM%send_item, &
       & monoCOM%recv_index, monoCOM%recv_item, &
       & X, X, ndof, monoCOM%comm)
    t2 = monolis_get_time()

    if(present(tcomm))then
      tcomm = tcomm + t2 - t1
    endif
  end subroutine monolis_mpi_update_I

  !> @ingroup mpi
  !> ベクトルのアップデート関数（複素数型）
  subroutine monolis_mpi_update_V_C(monoCOM, n, n_dof_list, X, tcomm)
    implicit none
    !> [in] COM 構造体
    type(monolis_com), intent(in) :: monoCOM
    !> [in] 全計算点数
    integer(kint), intent(in) :: n
    !> [in] 計算点が持つ自由度
    integer(kint), intent(in) :: n_dof_list(:)
    !> [in,out] 入出力ベクトル
    complex(kdouble), intent(inout) :: X(:)
    !> [in,out] 通信時間
    real(kdouble), optional, intent(inout) :: tcomm
    real(kdouble) :: t1, t2
    integer(kint), allocatable :: index_temp(:)

    if(monoCOM%send_n_neib == 0 .and. monoCOM%recv_n_neib == 0) return

    t1 = monolis_get_time()
    call monolis_alloc_I_1d(index_temp, n + 1)
    call monolis_get_ndof_index_from_ndof_list(n, n_dof_list, index_temp)
    call monolis_mpi_update_V_C_main(monoCOM, index_temp, X, tcomm)
    t2 = monolis_get_time()

    if(present(tcomm))then
      tcomm = tcomm + t2 - t1
    endif
  end subroutine monolis_mpi_update_V_C

  !> @ingroup dev_mpi
  !> ベクトルのアップデート関数（複素数型）
  subroutine monolis_mpi_update_V_C_main(monoCOM, n_dof_index, X, tcomm)
    implicit none
    !> [in] COM 構造体
    type(monolis_com), intent(in) :: monoCOM
    !> [in] 計算点が持つ自由度
    integer(kint), intent(in) :: n_dof_index(:)
    !> [in,out] 入出力ベクトル
    complex(kdouble), intent(inout) :: X(:)
    !> [in,out] 通信時間
    real(kdouble), optional, intent(inout) :: tcomm
    real(kdouble) :: t1, t2

    if(monoCOM%send_n_neib == 0 .and. monoCOM%recv_n_neib == 0) return

    t1 = monolis_get_time()
    call monolis_SendRecv_V_C(monoCOM%send_n_neib, monoCOM%send_neib_pe, &
       & monoCOM%recv_n_neib, monoCOM%recv_neib_pe, &
       & monoCOM%send_index, monoCOM%send_item, &
       & monoCOM%recv_index, monoCOM%recv_item, &
       & X, X, n_dof_index, n_dof_index, monoCOM%comm)
    t2 = monolis_get_time()

    if(present(tcomm))then
      tcomm = tcomm + t2 - t1
    endif
  end subroutine monolis_mpi_update_V_C_main

  !> @ingroup mpi
  !> ベクトルのアップデート関数（複素数型）
  subroutine monolis_mpi_update_C(monoCOM, ndof, X, tcomm)
    implicit none
    !> [in] COM 構造体
    type(monolis_com), intent(in) :: monoCOM
    !> [in] 計算点が持つ自由度
    integer(kint), intent(in) :: ndof
    !> [in,out] 入出力ベクトル
    complex(kdouble), intent(inout) :: X(:)
    !> [in,out] 通信時間
    real(kdouble), optional, intent(inout) :: tcomm
    real(kdouble) :: t1, t2

    if(monoCOM%send_n_neib == 0 .and. monoCOM%recv_n_neib == 0) return

    t1 = monolis_get_time()
    call monolis_SendRecv_C(monoCOM%send_n_neib, monoCOM%send_neib_pe, &
       & monoCOM%recv_n_neib, monoCOM%recv_neib_pe, &
       & monoCOM%send_index, monoCOM%send_item, &
       & monoCOM%recv_index, monoCOM%recv_item, &
       & X, X, ndof, monoCOM%comm)
    t2 = monolis_get_time()

    if(present(tcomm))then
      tcomm = tcomm + t2 - t1
    endif
  end subroutine monolis_mpi_update_C

  !> @ingroup mpi
  !> 隣接領域の任意本数ベクトルの取得関数（実数型）
  !> @detail 分割領域ごとに任意本数のベクトルが定義されている。
  subroutine monolis_mpi_get_neib_vector_R(monoCOM, n_vec, ndof, my_vec, neib_vec, tcomm)
    implicit none
    !> [in] COM 構造体
    type(monolis_com), intent(in) :: monoCOM
    !> [in] 自領域が持つベクトル数
    integer(kint), intent(in) :: n_vec
    !> [in] 計算点が持つ自由度
    integer(kint), intent(in) :: ndof
    !> [in] 自領域のベクトル
    real(kdouble), intent(in) :: my_vec(:,:)
    !> [out] 自領域を含む隣接領域のベクトル
    real(kdouble), intent(out) :: neib_vec(:,:)
    !> [in,out] 通信時間
    real(kdouble), optional, intent(inout) :: tcomm
    integer(kint) :: i, j, k, in, jn, kS, kE, kn, m
    integer(kint) :: comm_size, my_rank, n_send, n_recv
    real(kdouble) :: t1, t2
    integer(kint), allocatable :: n_neib_send(:)
    integer(kint), allocatable :: send_index(:), send_item(:), recv_index(:), recv_item(:)
    real(kdouble), allocatable :: X(:), Y(:)

    if(monoCOM%send_n_neib == 0 .and. monoCOM%recv_n_neib == 0)then
      neib_vec = my_vec
      return
    endif

    !# 送信ベクトル数の共有
    my_rank = monolis_mpi_get_local_my_rank(monoCOM%comm)
    comm_size = monolis_mpi_get_local_comm_size(monoCOM%comm)
    call monolis_alloc_I_1d(n_neib_send, comm_size)
    n_neib_send = n_vec
    call monolis_alltoall_I1(comm_size, n_neib_send, monoCOM%comm)

    !# 通信テーブルの計算（send）
    call monolis_alloc_I_1d(send_index, monoCOM%send_n_neib + 1)
    call monolis_alloc_I_1d(recv_index, monoCOM%recv_n_neib + 1)

    do i = 1, monoCOM%send_n_neib
      send_index(i + 1) = n_vec*monoCOM%send_index(i + 1)
    enddo

    n_send = monoCOM%send_index(monoCOM%send_n_neib + 1)
    call monolis_alloc_R_1d(X, n_vec*ndof*n_send)
    call monolis_alloc_I_1d(send_item, n_vec*n_send)
    call monolis_get_sequence_array_I(send_item, n_vec*n_send, 1, 1)

    !# 通信テーブルの計算（recv）
    n_recv = 0
    do i = 1, monoCOM%recv_n_neib
      in = monoCOM%recv_neib_pe(i)
      jn = monoCOM%recv_index(i + 1) - monoCOM%recv_index(i)
      n_recv = n_recv + jn*n_neib_send(in + 1)
      recv_index(i + 1) = recv_index(i) + jn*n_neib_send(in + 1)
    enddo

    call monolis_alloc_R_1d(Y, ndof*n_recv)
    call monolis_alloc_I_1d(recv_item, n_recv)
    call monolis_get_sequence_array_I(recv_item, n_recv, 1, 1)

    !# 送信ベクトル作成
    in = 0
    do k = 1, monoCOM%send_n_neib
      kS = monoCOM%send_index(k) + 1
      kE = monoCOM%send_index(k + 1)
      do i = 1, n_vec
        do j = kS, kE
          in = in + 1
          jn = monoCOM%send_item(j)
          X(ndof*(in-1)+1:ndof*in) = my_vec(ndof*(jn-1)+1:ndof*jn,i)
        enddo
      enddo
    enddo

    !# ベクトルのアップデート
    t1 = monolis_get_time()
    call monolis_SendRecv_R(monoCOM%send_n_neib, monoCOM%send_neib_pe, &
       & monoCOM%recv_n_neib, monoCOM%recv_neib_pe, &
       & send_index, send_item, &
       & recv_index, recv_item, &
       & X, Y, ndof, monoCOM%comm)
    t2 = monolis_get_time()

    !# 更新されたベクトルの格納（自領域）
    neib_vec = 0.0d0
    do i = 1, n_vec
      do j = 1, monoCOM%n_internal_vertex*ndof
        neib_vec(j,i) = my_vec(j,i)
      enddo
    enddo

    !# 受信要素の 0 初期化
    do i = 1, n_vec
      do j = 1, monoCOM%recv_index(monoCOM%recv_n_neib + 1)
        in = monoCOM%recv_item(j)
        neib_vec(ndof*(in - 1) + 1:ndof*in,i) = 0.0d0
      enddo
    enddo

    !# 更新されたベクトルの格納（隣接領域）
    m = 0
    jn = 0
    do i = 1, monoCOM%recv_n_neib
      in = monoCOM%recv_neib_pe(i)
      do j = 1, n_neib_send(in + 1)
        jn = jn + 1
        kS = monoCOM%recv_index(i) + 1
        kE = monoCOM%recv_index(i + 1)
        do k = kS, kE
          kn = monoCOM%recv_item(k)
          m = m + 1
          neib_vec(ndof*(kn-1)+1:ndof*kn, n_vec+jn) = Y(ndof*(m-1)+1:ndof*m)
        enddo
      enddo
    enddo

    if(present(tcomm))then
      tcomm = tcomm + t2 - t1
    endif
  end subroutine monolis_mpi_get_neib_vector_R

  subroutine monolis_get_ndof_index_from_ndof_list(n, ndof_list, ndof_index)
   implicit none
    !> [in] 全計算点数
    integer(kint), intent(in) :: n
    !> [in] 計算点が持つ自由度
    integer(kint), intent(in) :: ndof_list(:)
    !> [in,out] 入出力ベクトル
    integer(kint), intent(out) :: ndof_index(:)
    integer(kint) :: i
    ndof_index = 0
    do i = 1, n
      ndof_index(i + 1) = ndof_index(i) + ndof_list(i)
    enddo
  end subroutine monolis_get_ndof_index_from_ndof_list
end module mod_monolis_mpi_sendrecv
