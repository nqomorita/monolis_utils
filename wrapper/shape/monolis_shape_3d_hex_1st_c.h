/* alloc_c.h */
#ifndef MONOLIS_ALLOC_C_H
#define MONOLIS_ALLOC_C_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdbool.h>

static const int monolis_shape_3d_hex_1st_surf[6][4] = {
    {4, 3, 2, 1},
    {5, 6, 7, 8},
    {1, 2, 6, 5},
    {2, 3, 7, 6},
    {3, 4, 8, 7},
    {4, 1, 5, 8}
};

static const double monolis_shape_3d_hex_1st_surf_constraint_value[6][7] = {
    {0.0,  0.0, -1.0,  0.0,  0.0,  0.0,  0.0},
    {0.0,  0.0,  1.0,  0.0,  0.0,  0.0,  0.0},
    {0.0, -1.0,  0.0,  0.0,  0.0,  0.0,  0.0},
    {1.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0},
    {0.0,  1.0,  0.0,  0.0,  0.0,  0.0,  0.0},
    {-1.0, 0.0,  0.0,  0.0,  0.0,  0.0,  0.0}
};

static const bool monolis_shape_3d_hex_1st_surf_constraint_flag[12][7] = {
    { false, true,  true,  false, false, false, false },
    { true,  false, true,  false, false, false, false },
    { false, true,  true,  false, false, false, false },
    { true,  false, true,  false, false, false, false },
    { false, true,  true,  false, false, false, false },
    { true,  false, true,  false, false, false, false },
    { false, true,  true,  false, false, false, false },
    { true,  false, true,  false, false, false, false },
    { true,  true,  false, false, false, false, false },
    { true,  true,  false, false, false, false, false },
    { true,  true,  false, false, false, false, false },
    { true,  true,  false, false, false, false, false }
};

static const int monolis_shape_3d_hex_1st_edge[12][2] = {
    {1, 2},
    {2, 3},
    {3, 4},
    {4, 1},
    {5, 6},
    {6, 7},
    {7, 8},
    {8, 5},
    {1, 5},
    {2, 6},
    {3, 7},
    {4, 8}
};

static const double monolis_shape_3d_hex_1st_edge_constraint_value[12][7] = {
    {0.0, -1.0, -1.0,  0.0,  0.0,  0.0,  0.0},
    {1.0,  0.0, -1.0,  0.0,  0.0,  0.0,  0.0},
    {0.0,  1.0, -1.0,  0.0,  0.0,  0.0,  0.0},
    {-1.0, 0.0, -1.0,  0.0,  0.0,  0.0,  0.0},
    {0.0, -1.0,  1.0,  0.0,  0.0,  0.0,  0.0},
    {1.0,  0.0,  1.0,  0.0,  0.0,  0.0,  0.0},
    {0.0,  1.0,  1.0,  0.0,  0.0,  0.0,  0.0},
    {-1.0, 0.0,  1.0,  0.0,  0.0,  0.0,  0.0},
    {-1.0, -1.0, 0.0,   0.0,  0.0,  0.0,  0.0},
    {1.0,  -1.0, 0.0,   0.0,  0.0,  0.0,  0.0},
    {1.0,   1.0, 0.0,   0.0,  0.0,  0.0,  0.0},
    {-1.0,  1.0, 0.0,   0.0,  0.0,  0.0,  0.0}
};

static const bool monolis_shape_3d_hex_1st_edge_constraint_flag[12][7] = {
    { false, true,  true,  false, false, false, false },
    { true,  false, true,  false, false, false, false },
    { false, true,  true,  false, false, false, false },
    { true,  false, true,  false, false, false, false },
    { false, true,  true,  false, false, false, false },
    { true,  false, true,  false, false, false, false },
    { false, true,  true,  false, false, false, false },
    { true,  false, true,  false, false, false, false },
    { true,  true,  false, false, false, false, false },
    { true,  true,  false, false, false, false, false },
    { true,  true,  false, false, false, false, false },
    { true,  true,  false, false, false, false, false }
};

int monolis_shape_3d_hex_1st_num_gauss_point();

double monolis_shape_3d_hex_1st_weight(
    int i);

void monolis_shape_3d_hex_1st_integral_point(
    int     i, 
    double* r);

void monolis_shape_3d_hex_1st_node_point(
    int     i, 
    double* r);

void monolis_shape_3d_hex_1st_is_inside_domain(
    const double* local, 
    bool*         is_inside);

void monolis_shape_3d_hex_1st_shapefunc(
    const double* local, 
    double*       func);

void monolis_shape_3d_hex_1st_shapefunc_deriv(
    const double* local, 
    double**      func);

#ifdef __cplusplus
}
#endif

#endif
