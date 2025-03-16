#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

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

static const double weight[3] = {
    0.166666666666666, 0.166666666666666, 0.166666666666666
};

int monolis_shape_2d_tri_1st_num_gauss_point() {
    return 3;
}

double monolis_shape_2d_tri_1st_weight(
    int i) 
{
    return weight[i];
}

void monolis_shape_2d_tri_1st_integral_point(
    int     i, 
    double* r)
{
    r[0] = gsp[0][i];
    r[1] = gsp[1][i];
}

void monolis_shape_2d_tri_1st_node_point(
    int     i, 
    double* r) {
    r[0] = np[0][i];
    r[1] = np[1][i];
}

void monolis_shape_2d_tri_1st_shapefunc(
    double* local, 
    double* func) 
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
