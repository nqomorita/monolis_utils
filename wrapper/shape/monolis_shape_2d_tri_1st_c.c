#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "monolis_shape_2d_tri_1st_c.h"

static const double gsp[3][2] = {
    {0.166666666666667, 0.166666666666667},
    {0.666666666666667, 0.166666666666667},
    {0.166666666666667, 0.666666666666667}
};

static const double np[3][2] = {
    {0.0, 0.0},
    {1.0, 0.0},
    {0.0, 1.0}
};

int monolis_shape_2d_tri_1st_num_integral_point() {
    return 3;
}

double monolis_shape_2d_tri_1st_weight(
    const int i) 
{
    return 0.166666666666666;
}

void monolis_shape_2d_tri_1st_integral_point(
    const int i, 
    double*   r)
{
    r[0] = gsp[i][0];
    r[1] = gsp[i][1];
}

void monolis_shape_2d_tri_1st_node_point(
    const int i, 
    double*   r) {
    r[0] = np[i][0];
    r[1] = np[i][1];
}

void monolis_shape_2d_tri_1st_shapefunc(
    const double* local, 
    double*       func) 
{
    func[0] = 1.0 - local[0] - local[1];
    func[1] = local[0];
    func[2] = local[1];
}

void monolis_shape_2d_tri_1st_shapefunc_deriv(
    const double* local, 
    double**      func) 
{
    func[0][0] = -1.0;
    func[0][1] =  1.0;
    func[0][2] =  0.0;

    func[1][0] = -1.0;
    func[1][1] =  0.0;
    func[1][2] =  1.0;
}

void monolis_shape_2d_tri_1st_is_inside_domain(
    const double* local, 
    bool*         is_inside) 
{
    double func[3];

    *is_inside = false;
    monolis_shape_2d_tri_1st_shapefunc(local, func);

    if (0.0 <= func[0] && func[0] <= 1.0 &&
        0.0 <= func[1] && func[1] <= 1.0 &&
        0.0 <= func[2] && func[2] <= 1.0) {
        *is_inside = true;
    }
}
