#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "monolis_shape_3d_tet_1st_c.h"

static const double gsp[4][3] = {
    {0.138196601125011, 0.138196601125011, 0.138196601125011},
    {0.585410196624968, 0.138196601125011, 0.138196601125011},
    {0.138196601125011, 0.585410196624968, 0.138196601125011},
    {0.138196601125011, 0.138196601125011, 0.585410196624968}
};

int monolis_shape_3d_tet_2nd_num_integral_point()
{
    return 4;
}

double monolis_shape_3d_tet_2nd_weight(
    const int i)
{
    return 0.041666666666667;
}

void monolis_shape_3d_tet_2nd_integral_point(
    const int i, 
    double*   r)
{
    r[0] = gsp[i][0];
    r[1] = gsp[i][1];
    r[2] = gsp[i][2];
}

void monolis_shape_3d_tet_2nd_is_inside_domain(
    const double* local, 
    bool*         is_inside)
{
    double func[4];
    *is_inside = false;

    monolis_shape_3d_tet_1st_shapefunc(local, func);
    if (func[0] >= 0.0 && func[0] <= 1.0 &&
        func[1] >= 0.0 && func[1] <= 1.0 &&
        func[2] >= 0.0 && func[2] <= 1.0 &&
        func[3] >= 0.0 && func[3] <= 1.0){
        *is_inside = true;
    }
}

void monolis_shape_3d_tet_2nd_shapefunc(
    const double* local, 
    double*       func)
{
    double xi = local[0];
    double et = local[1];
    double st = local[2];
    double a  = 1.0 - xi - et - st;

    func[0] = (2.0 * a - 1.0) * a;
    func[1] = xi * (2.0 * xi - 1.0);
    func[2] = et * (2.0 * et - 1.0);
    func[3] = st * (2.0 * st - 1.0);
    func[4] = 4.0 * xi * a;
    func[5] = 4.0 * xi * et;
    func[6] = 4.0 * et * a;
    func[7] = 4.0 * st * a;
    func[8] = 4.0 * xi * st;
    func[9] = 4.0 * et * st;
}

void monolis_shape_3d_tet_2nd_shapefunc_deriv(
    const double* local, 
    double**      func)
{
    double xi = local[0];
    double et = local[1];
    double st = local[2];
    double a  = 1.0 - xi - et - st;

    func[0][0] = 1.0 - 4.0 * a;
    func[1][0] = 4.0 * xi - 1.0;
    func[2][0] = 0.0;
    func[3][0] = 0.0;
    func[4][0] = 4.0 * (1.0 - 2.0 * xi - et - st);
    func[5][0] = 4.0 * et;
    func[6][0] = -4.0 * et;
    func[7][0] = -4.0 * st;
    func[8][0] = 4.0 * st;
    func[9][0] = 0.0;

    func[0][1] = 1.0 - 4.0 * a;
    func[1][1] = 0.0;
    func[2][1] = 4.0 * et - 1.0;
    func[3][1] = 0.0;
    func[4][1] = -4.0 * xi;
    func[5][1] = 4.0 * xi;
    func[6][1] = 4.0 * (1.0 - xi - 2.0 * et - st);
    func[7][1] = -4.0 * st;
    func[8][1] = 0.0;
    func[9][1] = 4.0 * st;

    func[0][2] = 1.0 - 4.0 * a;
    func[1][2] = 0.0;
    func[2][2] = 0.0;
    func[3][2] = 4.0 * st - 1.0;
    func[4][2] = -4.0 * xi;
    func[5][2] = 0.0;
    func[6][2] = -4.0 * et;
    func[7][2] = 4.0 * (1.0 - xi - et - 2.0 * st);
    func[8][2] = 4.0 * xi;
    func[9][2] = 4.0 * et;
}
