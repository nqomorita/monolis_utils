/* alloc_c.h */
#ifndef MONOLIS_SHAPP_3D_TET_1ST_H
#define MONOLIS_SHAPP_3D_TET_1ST_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdbool.h>

static const int monolis_shape_3d_tet_1st_surf[4][3] = {
    {3, 2, 1},
    {1, 2, 4},
    {2, 3, 4},
    {3, 1, 4}
};

static const double monolis_shape_3d_tet_1st_surf_constraint_value[4][7] = {
    { 0.0,  0.0, -1.0,  0.0,  0.0,  0.0,  0.0},
    { 0.0, -1.0,  0.0,  0.0,  0.0,  0.0,  0.0},
    { 0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  1.0},
    {-1.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0}
};

static const bool monolis_shape_3d_tet_1st_surf_constraint_flag[4][7] = {
    { false, false, true,  false, false, false, false },
    { false, true,  false, false, false, false, false },
    { false, false, false, false, false, false, true  },
    { true,  false, false, false, false, false, false }
};

static const int monolis_shape_3d_tet_1st_edge[6][2] = {
    {1, 2},
    {2, 3},
    {3, 1},
    {1, 4},
    {2, 4},
    {3, 4}
};

static const double monolis_shape_3d_tet_1st_edge_constraint_value[6][7] = {
    { 0.0, -1.0, -1.0,  0.0,  0.0,  0.0,  0.0},
    { 0.0,  0.0, -1.0,  1.0,  0.0,  0.0,  0.0},
    {-1.0,  0.0, -1.0,  0.0,  0.0,  0.0,  0.0},
    {-1.0, -1.0,  0.0,  0.0,  0.0,  0.0,  0.0},
    { 0.0, -1.0,  0.0,  0.0,  0.0,  1.0,  0.0},
    {-1.0,  0.0,  0.0,  0.0,  1.0,  0.0,  0.0}
};

static const bool monolis_shape_3d_tet_1st_edge_constraint_flag[6][7] = {
    { false, true,  true,  false, false, false, false },
    { false, false, true,  true,  false, false, false },
    { true,  false, true,  false, false, false, false },
    { true,  true,  false, false, false, false, false },
    { false, true,  false, false, false, true,  false },
    { true,  false, false, false, true,  false, false }
};

int monolis_shape_3d_tet_1st_num_integral_point();

double monolis_shape_3d_tet_1st_weight(
    const int i);

void monolis_shape_3d_tet_1st_integral_point(
    const int i, 
    double*   r);

void monolis_shape_3d_tet_1st_node_point(
    const int i, 
    double*   r);

void monolis_shape_3d_tet_1st_is_inside_domain(
    const double* local, 
    bool*         is_inside);

void monolis_shape_3d_tet_1st_shapefunc(
    const double* local, 
    double*       func);

void monolis_shape_3d_tet_1st_shapefunc_deriv(
    const double* local, 
    double**      func);

#ifdef __cplusplus
}
#endif

#endif
