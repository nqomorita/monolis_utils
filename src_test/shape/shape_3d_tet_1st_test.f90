module mod_monolis_shape_3d_tet_1st_test
  use mod_monolis_utils
  use mod_monolis_utils_std_test
  use mod_monolis_def_shape
  use mod_monolis_shape_3d_tet_1st
  implicit none

contains

  subroutine monolis_shape_3d_tet_1st_test()
    implicit none
    integer(kint) :: i, j, face_id, edge_id, face_type, edge_type
    integer(kint), allocatable :: edge_nodes(:), face_nodes(:)
    real(kdouble) :: r(3), pos(3), local(3), parent(3)
    real(kdouble) :: sub_coord_face(2), sub_coord_edge(1)
    real(kdouble) :: func(4), deriv(4,3), dndx(4,3), det
    real(kdouble) :: node(3,4)
    logical :: is_inside, is_on_boundary, is_all_pass
    character(len=64) :: ctext
    
    call monolis_std_log_string("monolis_shape_3d_tet_1st_test")
    is_all_pass = .true.
    
    !-----------------------------------------------------------------------
    ! テスト1: 形状関数の検証
    ! 説明: 各節点位置で、対応する形状関数のみが1.0となり、他の関数は0となることを確認する
    ! これにより、形状関数が各節点を正しく補間できることを検証する
    !-----------------------------------------------------------------------
    do i = 1, 4
      call monolis_shape_3d_tet_1st_node_point(i, r) ! 節点iの座標を取得
      call monolis_shape_3d_tet_1st_shapefunc(r, func) ! その位置での形状関数の値を計算
      
      do j = 1, 4
        write(ctext, '("monolis_shape_3d_tet_1st_test node ", i1, " func ", i1)') i, j
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
    ! 説明: 四面体要素の形状関数の導関数が理論値と一致することを確認する
    ! 1次要素の場合、導関数は位置によらず一定値となる
    !-----------------------------------------------------------------------
    local = (/0.25d0, 0.25d0, 0.25d0/) ! 四面体の重心点
    call monolis_shape_3d_tet_1st_shapefunc_deriv(local, deriv)
    
    ! x方向の導関数をチェック
    call monolis_test_check_eq_R1("monolis_shape_3d_tet_1st_test deriv x1", deriv(1,1), -1.0d0) ! dN1/dx = -1.0
    call monolis_test_check_eq_R1("monolis_shape_3d_tet_1st_test deriv x2", deriv(2,1),  1.0d0) ! dN2/dx = 1.0
    call monolis_test_check_eq_R1("monolis_shape_3d_tet_1st_test deriv x3", deriv(3,1),  0.0d0) ! dN3/dx = 0.0
    call monolis_test_check_eq_R1("monolis_shape_3d_tet_1st_test deriv x4", deriv(4,1),  0.0d0) ! dN4/dx = 0.0
    
    ! y方向の導関数をチェック
    call monolis_test_check_eq_R1("monolis_shape_3d_tet_1st_test deriv y1", deriv(1,2), -1.0d0) ! dN1/dy = -1.0
    call monolis_test_check_eq_R1("monolis_shape_3d_tet_1st_test deriv y2", deriv(2,2),  0.0d0) ! dN2/dy = 0.0
    call monolis_test_check_eq_R1("monolis_shape_3d_tet_1st_test deriv y3", deriv(3,2),  1.0d0) ! dN3/dy = 1.0
    call monolis_test_check_eq_R1("monolis_shape_3d_tet_1st_test deriv y4", deriv(4,2),  0.0d0) ! dN4/dy = 0.0
    
    ! z方向の導関数をチェック
    call monolis_test_check_eq_R1("monolis_shape_3d_tet_1st_test deriv z1", deriv(1,3), -1.0d0) ! dN1/dz = -1.0
    call monolis_test_check_eq_R1("monolis_shape_3d_tet_1st_test deriv z2", deriv(2,3),  0.0d0) ! dN2/dz = 0.0
    call monolis_test_check_eq_R1("monolis_shape_3d_tet_1st_test deriv z3", deriv(3,3),  0.0d0) ! dN3/dz = 0.0
    call monolis_test_check_eq_R1("monolis_shape_3d_tet_1st_test deriv z4", deriv(4,3),  1.0d0) ! dN4/dz = 1.0
    
    !-----------------------------------------------------------------------
    ! テスト3: 定義域内判定のテスト
    ! 説明: 点が四面体の内部にあるか外部にあるかを正しく判定できることを確認する
    ! N1,N2,N3,N4がすべて0〜1の範囲にある場合、点は四面体内部にあると判定される
    !-----------------------------------------------------------------------
    local = (/0.25d0, 0.25d0, 0.25d0/) ! 四面体内部の点(重心)
    call monolis_shape_3d_tet_1st_is_inside_domain(local, is_inside)
    call monolis_test_check_eq_L1("monolis_shape_3d_tet_1st_test is_inside 1", is_inside, .true.)
    
    local = (/1.1d0, 0.25d0, 0.25d0/) ! 四面体外部の点(x座標が定義域外)
    call monolis_shape_3d_tet_1st_is_inside_domain(local, is_inside)
    call monolis_test_check_eq_L1("monolis_shape_3d_tet_1st_test is_inside 2", is_inside, .false.)
    
    !-----------------------------------------------------------------------
    ! テスト4: グローバル座標変換のテスト
    ! 説明: 四面体の局所座標(r,s,t)から実空間のグローバル座標(x,y,z)への変換を確認する
    ! 標準四面体(0,0,0)-(1,0,0)-(0,1,0)-(0,0,1)に対して、局所座標(0.25,0.25,0.25)は
    ! グローバル座標(0.25,0.25,0.25)に対応するはず
    !-----------------------------------------------------------------------
    node(1,1) = 0.0d0; node(2,1) = 0.0d0; node(3,1) = 0.0d0 ! 節点座標を定義
    node(1,2) = 1.0d0; node(2,2) = 0.0d0; node(3,2) = 0.0d0
    node(1,3) = 0.0d0; node(2,3) = 1.0d0; node(3,3) = 0.0d0
    node(1,4) = 0.0d0; node(2,4) = 0.0d0; node(3,4) = 1.0d0
    
    local = (/0.25d0, 0.25d0, 0.25d0/) ! 重心の局所座標
    call monolis_shape_3d_tet_1st_get_global_position(node, local, pos)
    
    ! 標準四面体では局所座標とグローバル座標が対応する
    call monolis_test_check_eq_R1("monolis_shape_3d_tet_1st_test global_pos 1", pos(1), 0.25d0)
    call monolis_test_check_eq_R1("monolis_shape_3d_tet_1st_test global_pos 2", pos(2), 0.25d0)
    call monolis_test_check_eq_R1("monolis_shape_3d_tet_1st_test global_pos 3", pos(3), 0.25d0)
    
    !-----------------------------------------------------------------------
    ! テスト5: グローバル導関数のテスト
    ! 説明: 形状関数のグローバル座標に対する導関数を計算する
    ! ヤコビアン行列式は四面体の体積の6倍となる
    !-----------------------------------------------------------------------
    call monolis_shape_3d_tet_1st_get_global_deriv(node, local, dndx, det)
    call monolis_test_check_eq_R1("monolis_shape_3d_tet_1st_test global_deriv det", det, 1.0d0)
    
    !-----------------------------------------------------------------------
    ! テスト6: 面情報の検証
    ! 説明: 四面体の各面が三角形1次要素として正しく定義されているか確認する
    ! 四面体は4つの三角形面を持ち、各面は3つの節点で定義される
    !-----------------------------------------------------------------------
    do face_id = 1, 4
      call monolis_shape_3d_tet_1st_get_face_data(face_id, face_nodes, face_type)
      
      ! 面の要素タイプが三角形1次要素であることを確認
      write(ctext, '("monolis_shape_3d_tet_1st_test face_type ", i1)') face_id
      call monolis_test_check_eq_I1(trim(ctext), face_type, monolis_shape_2d_tri_1st)
      
      ! 面は3つの節点を持つことを確認
      write(ctext, '("monolis_shape_3d_tet_1st_test face_nodes size ", i1)') face_id
      call monolis_test_check_eq_I1(trim(ctext), size(face_nodes), 3)
      
      deallocate(face_nodes)
    enddo
    
    !-----------------------------------------------------------------------
    ! テスト7: エッジ情報の検証
    ! 説明: 四面体の各エッジが線分1次要素として正しく定義されているか確認する
    ! 四面体は6つのエッジを持ち、各エッジは2つの節点で定義される
    !-----------------------------------------------------------------------
    do edge_id = 1, 6
      call monolis_shape_3d_tet_1st_get_edge_data(edge_id, edge_nodes, edge_type)
      
      ! エッジの要素タイプが線分1次要素であることを確認
      write(ctext, '("monolis_shape_3d_tet_1st_test edge_type ", i1)') edge_id
      call monolis_test_check_eq_I1(trim(ctext), edge_type, monolis_shape_1d_line_1st)
      
      ! エッジは2つの節点を持つことを確認
      write(ctext, '("monolis_shape_3d_tet_1st_test edge_nodes size ", i1)') edge_id
      call monolis_test_check_eq_I1(trim(ctext), size(edge_nodes), 2)
      
      deallocate(edge_nodes)
    enddo
    
    !-----------------------------------------------------------------------
    ! テスト8: 境界判定の検証
    ! 説明: 点が四面体の境界上にあるかどうかを正しく判定できることを確認する
    ! 標準四面体の場合、任意の座標成分が0であれば境界上にあると判定される
    !-----------------------------------------------------------------------
    local = (/0.0d0, 0.0d0, 0.0d0/) ! 頂点1（境界上）
    call monolis_shape_3d_tet_1st_is_on_boundary(local, is_on_boundary)
    call monolis_test_check_eq_L1("monolis_shape_3d_tet_1st_test is_on_boundary 1", is_on_boundary, .true.)
    
    local = (/0.25d0, 0.25d0, 0.25d0/) ! 四面体内部（境界外）
    call monolis_shape_3d_tet_1st_is_on_boundary(local, is_on_boundary)
    call monolis_test_check_eq_L1("monolis_shape_3d_tet_1st_test is_on_boundary 2", is_on_boundary, .false.)
    
    !-----------------------------------------------------------------------
    ! テスト9: 局所座標マッピングの検証（面）
    ! 説明: 面上の局所座標から親要素(四面体)の局所座標へのマッピングを確認する
    ! 面の局所座標(u,v)を四面体の局所座標(r,s,t)に変換する機能のテスト
    !-----------------------------------------------------------------------
    sub_coord_face = (/0.5d0, 0.5d0/) ! 面上の局所座標
    call monolis_shape_3d_tet_1st_map_local_coord(2, 1, sub_coord_face, parent) ! 底面 z=0
    
    ! 底面での局所座標(0.5,0.5)は四面体内での座標(0.5,0.5,0.0)に対応するはず
    call monolis_test_check_eq_R1("monolis_shape_3d_tet_1st_test map_local_coord face1 x", parent(1), 0.5d0)
    call monolis_test_check_eq_R1("monolis_shape_3d_tet_1st_test map_local_coord face1 y", parent(2), 0.5d0)
    call monolis_test_check_eq_R1("monolis_shape_3d_tet_1st_test map_local_coord face1 z", parent(3), 0.0d0)
    
    !-----------------------------------------------------------------------
    ! テスト10: 局所座標マッピングの検証（エッジ）
    ! 説明: エッジ上の局所座標から親要素の局所座標へのマッピングを確認する
    ! エッジの局所座標uを四面体の局所座標(r,s,t)に変換する機能のテスト
    !-----------------------------------------------------------------------
    sub_coord_edge = (/0.5d0/) ! エッジ上の局所座標 (uが0〜1の範囲)
    call monolis_shape_3d_tet_1st_map_local_coord(1, 1, sub_coord_edge, parent) ! エッジ1-2
    
    ! エッジ1-2での局所座標0.5は四面体内での座標(0.5,0.0,0.0)に対応するはず
    call monolis_test_check_eq_R1("monolis_shape_3d_tet_1st_test map_local_coord edge1 x", parent(1), 0.5d0)
    call monolis_test_check_eq_R1("monolis_shape_3d_tet_1st_test map_local_coord edge1 y", parent(2), 0.0d0)
    call monolis_test_check_eq_R1("monolis_shape_3d_tet_1st_test map_local_coord edge1 z", parent(3), 0.0d0)
    
    !-----------------------------------------------------------------------
    ! テスト11: 局所座標マッピングの検証（頂点）
    ! 説明: 頂点から親要素の局所座標へのマッピングを確認する
    ! 頂点IDを四面体の局所座標(r,s,t)に変換する機能のテスト
    !-----------------------------------------------------------------------
    call monolis_shape_3d_tet_1st_map_local_coord(0, 1, sub_coord_edge, parent) ! 頂点1
    
    ! 頂点1は四面体内での座標(0.0,0.0,0.0)に対応するはず
    call monolis_test_check_eq_R1("monolis_shape_3d_tet_1st_test map_local_coord vertex1 x", parent(1), 0.0d0)
    call monolis_test_check_eq_R1("monolis_shape_3d_tet_1st_test map_local_coord vertex1 y", parent(2), 0.0d0)
    call monolis_test_check_eq_R1("monolis_shape_3d_tet_1st_test map_local_coord vertex1 z", parent(3), 0.0d0)
    
    !-----------------------------------------------------------------------
    ! テスト12: エッジと面の対応関係確認
    ! 説明: エッジ1が面1と面2に共有されていることを確認する
    ! 四面体のトポロジー構造（エッジと面の接続関係）の整合性を検証
    !-----------------------------------------------------------------------
    call monolis_shape_3d_tet_1st_get_edge_data(1, edge_nodes, edge_type)
    call monolis_shape_3d_tet_1st_get_face_data(1, face_nodes, face_type)
    
    ! エッジ1の両端点が面1の節点集合に含まれるかチェック
    call monolis_test_check_eq_L1("monolis_shape_3d_tet_1st_test edge1 in face1", &
               any(edge_nodes(1) == face_nodes) .and. any(edge_nodes(2) == face_nodes), .true.)
    
    deallocate(edge_nodes, face_nodes)
    
    call monolis_shape_3d_tet_1st_get_edge_data(1, edge_nodes, edge_type)
    call monolis_shape_3d_tet_1st_get_face_data(2, face_nodes, face_type)
    
    ! エッジ1の両端点が面2の節点集合にも含まれるかチェック
    call monolis_test_check_eq_L1("monolis_shape_3d_tet_1st_test edge1 in face2", &
               any(edge_nodes(1) == face_nodes) .and. any(edge_nodes(2) == face_nodes), .true.)
    
    deallocate(edge_nodes, face_nodes)
    
    ! すべてのテストが成功したか確認
    if(is_all_pass) then
      write(*,*) "monolis_shape_3d_tet_1st_test: PASS"
    else
      write(*,*) "monolis_shape_3d_tet_1st_test: FAIL"
    end if
  end subroutine monolis_shape_3d_tet_1st_test

end module mod_monolis_shape_3d_tet_1st_test
