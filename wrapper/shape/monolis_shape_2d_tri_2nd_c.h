/* alloc_c.h */
#ifndef MONOLIS_ALLOC_C_H
#define MONOLIS_ALLOC_C_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdbool.h>

static const int monolis_shape_2d_tri_2nd_edge[3][3] = {
    {1, 4, 2},
    {2, 5, 3},
    {3, 6, 1}
};

static const double monolis_shape_2d_tri_2nd_edge_constraint_value[3][3] = {
    { 0.0, -1.0,  0.0},
    { 0.0,  0.0,  0.0},
    {-1.0,  0.0,  0.0}
};

static const bool monolis_shape_2d_tri_2nd_edge_constraint_flag[3][3] = {
    { false, true,  false },
    { false, false, true  },
    { true,  false, false }
};

int monolis_shape_2d_tri_2nd_num_gauss_point();

double monolis_shape_2d_tri_2nd_weight(
    int i);

void monolis_shape_2d_tri_2nd_integral_point(
    int     i, 
    double* r);

void monolis_shape_2d_tri_2nd_node_point(
    int     i, 
    double* r);

void monolis_shape_2d_tri_2nd_is_inside_domain(
    const double* local, 
    bool*         is_inside);

void monolis_shape_2d_tri_2nd_shapefunc(
    const double* local, 
    double*       func);

void monolis_shape_2d_tri_2nd_shapefunc_deriv(
    const double* local, 
    double**      func);

void monolis_shape_2d_tri_2nd_shapefunc_2nd_deriv(
    double func[6][2][2]);

#ifdef __cplusplus
}
#endif

#endif
