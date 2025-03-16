#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

static const double gsp[3] = {
    0.25, 0.25, 0.25
};

static const double np[3][4] = {
    {0.0, 1.0, 0.0, 0.0}, 
    {0.0, 0.0, 1.0, 0.0}, 
    {0.0, 0.0, 0.0, 1.0}  
};

int monolis_shape_3d_tet_1st_num_gauss_point()
{
    return 1;
}

double monolis_shape_3d_tet_1st_weight(
    int i)
{
    return 0.166666666666666;
}

void monolis_shape_3d_tet_1st_integral_point(
    int     i, 
    double* r)
{
    r[0] = gsp[0];
    r[1] = gsp[1];
    r[2] = gsp[2];
}

void monolis_shape_3d_tet_1st_node_point(
    int     i, 
    double* r)
{
    r[0] = np[0][i];
    r[1] = np[1][i];
    r[2] = np[2][i];
}

void monolis_shape_3d_tet_1st_is_inside_domain(
    const double* local, 
    bool*         is_inside)
{
    double func[4];
    *is_inside = false;

    monolis_shape_3d_tet_1st_shapefunc(local, func);
    if (func[0] >= 0.0 && func[0] <= 1.0 &&
        func[1] >= 0.0 && func[1] <= 1.0 &&
        func[2] >= 0.0 && func[2] <= 1.0 &&
        func[3] >= 0.0 && func[3] <= 1.0)
    {
        *is_inside = true;
    }
}

void monolis_shape_3d_tet_1st_shapefunc(
    const double* local, 
    double*       func)
{
    func[0] = 1.0 - local[0] - local[1] - local[2];
    func[1] = local[0];
    func[2] = local[1];
    func[3] = local[2];
}

void monolis_shape_3d_tet_1st_shapefunc_deriv(
    const double* local, 
    double**      func)
{
    (void)local;  /* local は不要 */
    func[0][0] = -1.0;  func[0][1] = -1.0;  func[0][2] = -1.0;
    func[1][0] =  1.0;  func[1][1] =  0.0;  func[1][2] =  0.0;
    func[2][0] =  0.0;  func[2][1] =  1.0;  func[2][2] =  0.0;
    func[3][0] =  0.0;  func[3][1] =  0.0;  func[3][2] =  1.0;
}

