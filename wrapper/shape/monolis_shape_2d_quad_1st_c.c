#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

static const double gsp[4][2] = {
    {-0.577350269189626, -0.577350269189626},
    { 0.577350269189626, -0.577350269189626},
    {-0.577350269189626,  0.577350269189626},
    { 0.577350269189626,  0.577350269189626}
};

static const double np[4][2] = {
    {-1.0, -1.0},
    { 1.0, -1.0},
    { 1.0,  1.0},
    {-1.0,  1.0}
};

int monolis_shape_2d_quad_1st_num_integral_point() 
{
    return 4;
}

double monolis_shape_2d_quad_1st_weight(
    const int i) 
{
    return 1.0;
}

void monolis_shape_2d_quad_1st_integral_point(
    const int i, 
    double*   r) 
{
    r[0] = gsp[i][0];
    r[1] = gsp[i][1];
}

void monolis_shape_2d_quad_1st_node_point(
    const int i, 
    double*   r) 
{
    r[0] = np[i][0];
    r[1] = np[i][1];
}

void monolis_shape_2d_quad_1st_is_inside_domain(
    const double* local, 
    bool*         is_inside) 
{
    *is_inside = false;
    if (local[0] >= -1.0 && local[0] <= 1.0 &&
        local[1] >= -1.0 && local[1] <= 1.0) 
    {
        *is_inside = true;
    }
}

void monolis_shape_2d_quad_1st_shapefunc(
    const double* local, 
    double*       func) 
{
    func[0] = 0.25 * (1.0 - local[0]) * (1.0 - local[1]);
    func[1] = 0.25 * (1.0 + local[0]) * (1.0 - local[1]);
    func[2] = 0.25 * (1.0 + local[0]) * (1.0 + local[1]);
    func[3] = 0.25 * (1.0 - local[0]) * (1.0 + local[1]);
}

void monolis_shape_2d_quad_1st_shapefunc_deriv(
    const double* local, 
    double**      func) 
{
    func[0][0] = -0.25 * (1.0 - local[1]);
    func[1][0] =  0.25 * (1.0 - local[1]);
    func[2][0] =  0.25 * (1.0 + local[1]);
    func[3][0] = -0.25 * (1.0 + local[1]);

    func[0][1] = -0.25 * (1.0 - local[0]);
    func[1][1] = -0.25 * (1.0 + local[0]);
    func[2][1] =  0.25 * (1.0 + local[0]);
    func[3][1] =  0.25 * (1.0 - local[0]);
}
