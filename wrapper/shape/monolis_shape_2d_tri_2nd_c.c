#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "monolis_shape_2d_tri_1st_c.h"

static const double gsp[4][2] = {
    {0.166666666666667, 0.166666666666667},
    {0.666666666666667, 0.166666666666667},
    {0.166666666666667, 0.666666666666667},
    {0.333333333333333, 0.333333333333333}
};

static const double np[6][2] = {
    {0.0, 0.0},
    {1.0, 0.0},
    {1.0, 0.0},
    {0.5, 0.0},
    {0.5, 0.5},
    {0.0, 0.5}
};

int monolis_shape_2d_tri_2nd_num_integral_point() 
{
    return 4;
}

double monolis_shape_2d_tri_2nd_weight(
    const int i) 
{
    return 0.5;
}

void monolis_shape_2d_tri_2nd_integral_point(
    const int i, 
    double*   r) 
{
    r[0] = gsp[i][0];
    r[1] = gsp[i][1];
}

void monolis_shape_2d_tri_2nd_node_point(
    const int i, 
    double*   r) 
{
    r[0] = np[i][0];
    r[1] = np[i][1];
}

void monolis_shape_2d_tri_2nd_is_inside_domain(
    const double* local, 
    bool*         is_inside) 
{
    double func[3];

    *is_inside = false;
    monolis_shape_2d_tri_1st_shapefunc(local, func);
    if (func[0] >= 0.0 && func[0] <= 1.0 &&
        func[1] >= 0.0 && func[1] <= 1.0 &&
        func[2] >= 0.0 && func[2] <= 1.0)
    {
        *is_inside = true;
    }
}

void monolis_shape_2d_tri_2nd_shapefunc(
    const double* local, 
    double*       func) 
{
    double st = 1.0 - local[0] - local[1];
    double xi = local[0];
    double et = local[1];

    func[0] = st * (2.0 * st - 1.0);
    func[1] = xi * (2.0 * xi - 1.0);
    func[2] = et * (2.0 * et - 1.0);
    func[3] = 4.0 * xi * st;
    func[4] = 4.0 * xi * et;
    func[5] = 4.0 * et * st;
}

void monolis_shape_2d_tri_2nd_shapefunc_deriv(
    const double* local, 
    double**      func) 
{
    double st = 1.0 - local[0] - local[1];
    double xi = local[0];
    double et = local[1];

    func[0][0] = 1.0 - 4.0 * st;
    func[1][0] = 4.0 * xi - 1.0;
    func[2][0] = 0.0;
    func[3][0] = 4.0 * (st - xi);
    func[4][0] = 4.0 * et;
    func[5][0] = -4.0 * et;

    func[0][1] = 1.0 - 4.0 * st;
    func[1][1] = 0.0;
    func[2][1] = 4.0 * et - 1.0;
    func[3][1] = -4.0 * xi;
    func[4][1] = 4.0 * xi;
    func[5][1] = 4.0 * (st - et);
}

void monolis_shape_2d_tri_2nd_shapefunc_2nd_deriv(
    double*** func) 
{
    func[0][0][0] =  4.0;  func[0][0][1] =  4.0;
    func[1][0][0] =  4.0;  func[1][0][1] =  0.0;
    func[2][0][0] =  0.0;  func[2][0][1] =  0.0;
    func[3][0][0] = -8.0;  func[3][0][1] = -4.0;
    func[4][0][0] =  0.0;  func[4][0][1] =  4.0;
    func[5][0][0] =  0.0;  func[5][0][1] = -4.0;

    func[0][1][0] =  4.0;  func[0][1][1] =  4.0;
    func[1][1][0] =  0.0;  func[1][1][1] =  0.0;
    func[2][1][0] =  0.0;  func[2][1][1] =  4.0;
    func[3][1][0] = -4.0;  func[3][1][1] =  0.0;
    func[4][1][0] =  4.0;  func[4][1][1] =  0.0;
    func[5][1][0] = -4.0;  func[5][1][1] = -8.0;
}
