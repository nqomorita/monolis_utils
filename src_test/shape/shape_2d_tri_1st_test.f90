module mod_monolis_shape_2d_tri_1st_test
  use mod_monolis_utils
  use mod_monolis_utils_std_test
  use mod_monolis_def_shape
  use mod_monolis_shape_2d_tri_1st
  implicit none

contains

  subroutine monolis_shape_2d_tri_1st_test()
    implicit none
    integer(kint) :: i, j, edge_id, edge_type
    integer(kint), allocatable :: edge_nodes(:)
    real(kdouble) :: r(2), pos(2), local(2), parent(2)
    real(kdouble) :: func(3), deriv(3,2), dndx(3,2), det
    real(kdouble) :: node(2,3)
    real(kdouble) :: sub_coord_edge(1)
    logical :: is_inside, is_on_boundary, is_all_pass
    character(len=64) :: ctext
    
    call monolis_std_log_string("monolis_shape_2d_tri_1st_test")
    is_all_pass = .true.
    
    !-----------------------------------------------------------------------
    ! テスト1: 形状関数の検証
    ! 説明: 各節点位置で、対応する形状関数のみが1.0となり、他の関数は0となることを確認する
    !-----------------------------------------------------------------------
    do i = 1, 3
      call monolis_shape_2d_tri_1st_node_point(i, r) ! 節点iの座標を取得
      call monolis_shape_2d_tri_1st_shapefunc(r, func) ! その位置での形状関数の値を計算
      
      do j = 1, 3
        write(ctext, '("monolis_shape_2d_tri_1st_test node ", i1, " func ", i1)') i, j
        if(i == j) then
          ! i番目の節点ではi番目の形状関数のみ1.0、他は0.0になるべき
          call monolis_test_check_eq_R1(trim(ctext), func(j), 1.0d0)
        else
          call monolis_test_check_eq_R1(trim(ctext), func(j), 0.0d0)
        endif
      enddo
    enddo
    
    !-----------------------------------------------------------------------
    ! テスト2: 形状関数の導関数の検証
    ! 説明: 三角形要素の形状関数の導関数が理論値と一致することを確認する
    ! 1次要素の場合、導関数は位置によらず一定値となる
    !-----------------------------------------------------------------------
    local = (/0.33333d0, 0.33333d0/) ! 三角形の重心点
    call monolis_shape_2d_tri_1st_shapefunc_deriv(local, deriv)
    
    ! x方向の導関数をチェック
    call monolis_test_check_eq_R1("monolis_shape_2d_tri_1st_test deriv x1", deriv(1,1), -1.0d0) ! dN1/dx = -1.0
    call monolis_test_check_eq_R1("monolis_shape_2d_tri_1st_test deriv x2", deriv(2,1),  1.0d0) ! dN2/dx = 1.0
    call monolis_test_check_eq_R1("monolis_shape_2d_tri_1st_test deriv x3", deriv(3,1),  0.0d0) ! dN3/dx = 0.0
    
    ! y方向の導関数をチェック
    call monolis_test_check_eq_R1("monolis_shape_2d_tri_1st_test deriv y1", deriv(1,2), -1.0d0) ! dN1/dy = -1.0
    call monolis_test_check_eq_R1("monolis_shape_2d_tri_1st_test deriv y2", deriv(2,2),  0.0d0) ! dN2/dy = 0.0
    call monolis_test_check_eq_R1("monolis_shape_2d_tri_1st_test deriv y3", deriv(3,2),  1.0d0) ! dN3/dy = 1.0
    
    !-----------------------------------------------------------------------
    ! テスト3: 定義域内判定のテスト
    ! 説明: 点が三角形の内部にあるか外部にあるかを正しく判定できることを確認する
    !-----------------------------------------------------------------------
    local = (/0.33333d0, 0.33333d0/) ! 三角形内部の点(重心)
    call monolis_shape_2d_tri_1st_is_inside_domain(local, is_inside)
    call monolis_test_check_eq_L1("monolis_shape_2d_tri_1st_test is_inside 1", is_inside, .true.)
    
    local = (/1.1d0, 0.1d0/) ! 三角形外部の点
    call monolis_shape_2d_tri_1st_is_inside_domain(local, is_inside)
    call monolis_test_check_eq_L1("monolis_shape_2d_tri_1st_test is_inside 2", is_inside, .false.)
    
    !-----------------------------------------------------------------------
    ! テスト4: グローバル座標変換のテスト
    ! 説明: 三角形の局所座標(r,s)から実空間のグローバル座標(x,y)への変換を確認する
    !-----------------------------------------------------------------------
    node(1,1) = 0.0d0; node(2,1) = 0.0d0 ! 節点座標を定義
    node(1,2) = 1.0d0; node(2,2) = 0.0d0
    node(1,3) = 0.0d0; node(2,3) = 1.0d0
    
    local = (/0.33333d0, 0.33333d0/) ! 重心の局所座標
    call monolis_shape_2d_tri_1st_get_global_position(node, local, pos)
    
    ! 標準三角形では重心は(1/3, 1/3)となるはず
    call monolis_test_check_eq_R1("monolis_shape_2d_tri_1st_test global_pos 1", pos(1), 0.33333d0)
    call monolis_test_check_eq_R1("monolis_shape_2d_tri_1st_test global_pos 2", pos(2), 0.33333d0)
    
    !-----------------------------------------------------------------------
    ! テスト5: グローバル導関数のテスト
    ! 説明: 形状関数のグローバル座標に対する導関数を計算する
    !-----------------------------------------------------------------------
    call monolis_shape_2d_tri_1st_get_global_deriv(node, local, dndx, det)
    call monolis_test_check_eq_R1("monolis_shape_2d_tri_1st_test global_deriv det", det, 1.0d0)
    
    !-----------------------------------------------------------------------
    ! テスト6: エッジ情報の検証
    ! 説明: 三角形の各エッジが線分1次要素として正しく定義されているか確認する
    !-----------------------------------------------------------------------
    do edge_id = 1, 3
      call monolis_shape_2d_tri_1st_get_edge_data(edge_id, edge_nodes, edge_type)
      
      ! エッジの要素タイプが線分1次要素であることを確認
      write(ctext, '("monolis_shape_2d_tri_1st_test edge_type ", i1)') edge_id
      call monolis_test_check_eq_I1(trim(ctext), edge_type, monolis_shape_1d_line_1st)
      
      ! エッジは2つの節点を持つことを確認
      write(ctext, '("monolis_shape_2d_tri_1st_test edge_nodes size ", i1)') edge_id
      call monolis_test_check_eq_I1(trim(ctext), size(edge_nodes), 2)
      
      deallocate(edge_nodes)
    enddo
    
    !-----------------------------------------------------------------------
    ! テスト7: 境界判定の検証
    ! 説明: 点が三角形の境界上にあるかどうかを正しく判定できることを確認する
    !-----------------------------------------------------------------------
    local = (/0.0d0, 0.0d0/) ! 頂点1（境界上）
    call monolis_shape_2d_tri_1st_is_on_boundary(local, is_on_boundary)
    call monolis_test_check_eq_L1("monolis_shape_2d_tri_1st_test is_on_boundary 1", is_on_boundary, .true.)
    
    local = (/0.33333d0, 0.33333d0/) ! 三角形内部（境界外）
    call monolis_shape_2d_tri_1st_is_on_boundary(local, is_on_boundary)
    call monolis_test_check_eq_L1("monolis_shape_2d_tri_1st_test is_on_boundary 2", is_on_boundary, .false.)
    
    !-----------------------------------------------------------------------
    ! テスト8: 局所座標マッピングの検証（エッジ）
    ! 説明: エッジ上の局所座標から親要素の局所座標へのマッピングを確認する
    !-----------------------------------------------------------------------
    sub_coord_edge = (/0.0d0/) ! エッジ上の局所座標
    call monolis_shape_2d_tri_1st_map_local_coord(1, 1, sub_coord_edge, parent) ! エッジ1
    
    ! エッジ1での中点は三角形内での座標(0.5,0.0)に対応するはず
    call monolis_test_check_eq_R1("monolis_shape_2d_tri_1st_test map_local_coord edge1 x", parent(1), 0.5d0)
    call monolis_test_check_eq_R1("monolis_shape_2d_tri_1st_test map_local_coord edge1 y", parent(2), 0.0d0)
    
    !-----------------------------------------------------------------------
    ! テスト9: 局所座標マッピングの検証（頂点）
    ! 説明: 頂点から親要素の局所座標へのマッピングを確認する
    !-----------------------------------------------------------------------
    call monolis_shape_2d_tri_1st_map_local_coord(0, 1, sub_coord_edge, parent) ! 頂点1
    
    ! 頂点1は三角形内での座標(0.0,0.0)に対応するはず
    call monolis_test_check_eq_R1("monolis_shape_2d_tri_1st_test map_local_coord vertex1 x", parent(1), 0.0d0)
    call monolis_test_check_eq_R1("monolis_shape_2d_tri_1st_test map_local_coord vertex1 y", parent(2), 0.0d0)
    
    ! すべてのテストが成功したか確認
    if(is_all_pass) then
      write(*,*) "monolis_shape_2d_tri_1st_test: PASS"
    else
      write(*,*) "monolis_shape_2d_tri_1st_test: FAIL"
    end if
  end subroutine monolis_shape_2d_tri_1st_test

end module mod_monolis_shape_2d_tri_1st_test
