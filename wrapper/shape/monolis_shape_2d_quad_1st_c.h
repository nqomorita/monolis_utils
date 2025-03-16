/* alloc_c.h */
#ifndef MONOLIS_ALLOC_C_H
#define MONOLIS_ALLOC_C_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdbool.h>

static const int monolis_shape_2d_quad_1st_edge[4][2] = {
    {1, 2},
    {2, 3},
    {3, 4},
    {4, 1}
};

static const double monolis_shape_2d_quad_1st_edge_constraint_value[4][3] = {
    { 0.0, -1.0,  0.0},
    { 1.0,  0.0,  0.0},
    { 0.0,  1.0,  0.0},
    {-1.0,  0.0,  0.0}
};

static const bool monolis_shape_2d_quad_1st_edge_constraint_flag[4][3] = {
    { false, true,  false },
    { true,  false, false },
    { false, true,  false },
    { true,  false, false }
};

/**
 * @brief 1 次元整数配列のメモリ確保
 * @details 初期値 0 でメモリ確保がなされる。
 * @param[inout] var メモリ確保する配列
 * @param[in] size 配列サイズ
 * @ingroup alloc
 */
int monolis_shape_2d_quad_1st_num_gauss_point();

double monolis_shape_2d_quad_1st_weight(
    int i);

void monolis_shape_2d_quad_1st_integral_point(
    int     i, 
    double* r);

void monolis_shape_2d_quad_1st_node_point(
    int     i, 
    double* r);

void monolis_shape_2d_quad_1st_is_inside_domain(
    const double* local, 
    bool*         is_inside);

void monolis_shape_2d_quad_1st_shapefunc(
    const double* local, 
    double*       func);

void monolis_shape_2d_quad_1st_shapefunc_deriv(
    const double* local, 
    double**      func);

#ifdef __cplusplus
}
#endif

#endif
