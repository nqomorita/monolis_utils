/* alloc_c.h */
#ifndef MONOLIS_SHAPP_3D_TET_2ND_H
#define MONOLIS_SHAPP_3D_TET_2ND_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdbool.h>

/* 静的変数: 3D 四面体 2 次要素の面情報 */
static const int MONOLIS_SHAPE_3D_TET_2ND_SURF[4][6] = {
    { 2, 1, 0, 5, 4, 6 },
    { 0, 1, 3, 4, 8, 7 },
    { 1, 2, 3, 5, 9, 8 },
    { 2, 0, 3, 6, 7, 9 }
};

/* 静的変数: 3D 四面体 2 次要素の面の拘束値 */
static const double MONOLIS_SHAPE_3D_TET_2ND_SURF_CONSTRAINT_VALUE[4][7] = {
    {  0.0,  0.0, -1.0,  0.0,  0.0,  0.0,  0.0 },
    {  0.0, -1.0,  0.0,  0.0,  0.0,  0.0,  0.0 },
    {  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  1.0 },
    { -1.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0 }
};

/* 静的変数: 3D 四面体 2 次要素の面の拘束フラグ */
static const bool MONOLIS_SHAPE_3D_TET_2ND_SURF_CONSTRAINT_FLAG[4][7] = {
    { false, false, true,  false, false, false, false },
    { false, true,  false, false, false, false, false },
    { false, false, false, false, false, false, true  },
    { true,  false, false, false, false, false, false }
};


/* 静的変数: 3D 四面体 2 次要素のエッジ情報 */
static const int MONOLIS_SHAPE_3D_TET_2ND_EDGE[6][3] = {
    { 0, 4, 1 },
    { 1, 5, 2 },
    { 2, 6, 0 },
    { 0, 7, 3 },
    { 1, 8, 3 },
    { 2, 9, 3 }
};

/* 静的変数: 3D 四面体 2 次要素のエッジの拘束値 */
static const double MONOLIS_SHAPE_3D_TET_2ND_EDGE_CONSTRAINT_VALUE[6][7] = {
    {  0.0, -1.0, -1.0,  0.0,  0.0,  0.0,  0.0 },
    {  0.0,  0.0, -1.0,  1.0,  0.0,  0.0,  0.0 },
    { -1.0,  0.0, -1.0,  0.0,  0.0,  0.0,  0.0 },
    { -1.0, -1.0,  0.0,  0.0,  0.0,  0.0,  0.0 },
    {  0.0, -1.0,  0.0,  0.0,  0.0,  1.0,  0.0 },
    { -1.0,  0.0,  0.0,  0.0,  1.0,  0.0,  0.0 }
};

/* 静的変数: 3D 四面体 2 次要素のエッジの拘束フラグ */
static const bool MONOLIS_SHAPE_3D_TET_2ND_EDGE_CONSTRAINT_FLAG[6][7] = {
    { false, true,  true,  false, false, false, false },
    { false, false, true,  true,  false, false, false },
    { true,  false, true,  false, false, false, false },
    { true,  true,  false, false, false, false, false },
    { false, true,  false, false, false, true,  false },
    { true,  false, false, false, true,  false, false }
};

/**
 * @brief 3D 四面体 2 次要素の積分点数を取得
 * @details 積分点の総数を返す。
 * @return 積分点数
 * @ingroup shape_func
 */
int monolis_shape_3d_tet_2nd_num_integral_point();

/**
 * @brief 3D 四面体 2 次要素の積分点の重みを取得
 * @details 指定された積分点の重みを返す。
 * @param[in] i 積分点のインデックス
 * @return 積分点の重み
 * @ingroup shape_func
 */
double monolis_shape_3d_tet_2nd_weight(
    const int i);

/**
 * @brief 3D 四面体 2 次要素の積分点の座標を取得
 * @details 指定された積分点の座標を取得する。
 * @param[in] i 積分点のインデックス
 * @param[out] r 積分点の座標 (x, y, z)
 * @ingroup shape_func
 */
void monolis_shape_3d_tet_2nd_integral_point(
    const int i, 
    double*   r);

/**
 * @brief 3D 四面体 2 次要素の内部判定
 * @details 指定された局所座標が要素内にあるか判定する。
 * @param[in] local 局所座標 (x, y, z)
 * @param[out] is_inside 内部判定結果 (true: 内部, false: 外部)
 * @ingroup shape_func
 */
void monolis_shape_3d_tet_2nd_is_inside_domain(
    const double* local, 
    bool*         is_inside);

/**
 * @brief 3D 四面体 2 次要素の形状関数を計算
 * @details 指定された局所座標での形状関数の値を計算する。
 * @param[in] local 局所座標 (x, y, z)
 * @param[out] func 形状関数の値 (10 要素)
 * @ingroup shape_func
 */
void monolis_shape_3d_tet_2nd_shapefunc(
    const double* local, 
    double*       func);

/**
 * @brief 3D 四面体 2 次要素の形状関数の導関数を計算
 * @details 指定された局所座標での形状関数の偏微分を計算する。
 * @param[in] local 局所座標 (x, y, z)
 * @param[out] func 形状関数の偏微分値 (10 × 3 行列)
 * @ingroup shape_func
 */
void monolis_shape_3d_tet_2nd_shapefunc_deriv(
    const double* local, 
    double**      func);

#ifdef __cplusplus
}
#endif

#endif /* MONOLIS_SHAPP_3D_TET_2ND_H */
