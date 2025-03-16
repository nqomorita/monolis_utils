#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "monolis_utils.h"

#define TOL 1.0e-12

void monolis_shape_3d_tet_1st_num_integral_point_c_test()
{
    monolis_std_log_string("monolis_shape_3d_tet_1st_num_integral_point");

    int n = monolis_shape_3d_tet_1st_num_integral_point();

    monolis_test_check_eq_I1("monolis_shape_3d_tet_1st_num_integral_point", n, 1);
}

void monolis_shape_3d_tet_1st_weight_c_test()
{
    monolis_std_log_string("monolis_shape_3d_tet_1st_weight");

    double w = monolis_shape_3d_tet_1st_weight(0);
    monolis_test_check_eq_R1("monolis_shape_3d_tet_1st_weight", w, 0.166666666666666);
}

void monolis_shape_3d_tet_1st_integral_point_c_test()
{
    monolis_std_log_string("monolis_shape_3d_tet_1st_integral_point");

    double expected[3] = {0.25, 0.25, 0.25};
    double r[3];

    monolis_shape_3d_tet_1st_integral_point(0, r);
    monolis_test_check_eq_R1("monolis_shape_3d_tet_1st_integral_point r0", r[0], expected[0]);
    monolis_test_check_eq_R1("monolis_shape_3d_tet_1st_integral_point r1", r[1], expected[1]);
    monolis_test_check_eq_R1("monolis_shape_3d_tet_1st_integral_point r2", r[2], expected[2]);
}

void monolis_shape_3d_tet_1st_node_point_c_test()
{
    monolis_std_log_string("monolis_shape_3d_tet_1st_node_point");

    double expected[4][3] = {
        {0.0, 0.0, 0.0},
        {1.0, 0.0, 0.0},
        {0.0, 1.0, 0.0},
        {0.0, 0.0, 1.0}
    };

    double r[3];

    for (int i = 0; i < 4; i++) {
        monolis_shape_3d_tet_1st_node_point(i, r);
        monolis_test_check_eq_R1("monolis_shape_3d_tet_1st_node_point r0", r[0], expected[i][0]);
        monolis_test_check_eq_R1("monolis_shape_3d_tet_1st_node_point r1", r[1], expected[i][1]);
        monolis_test_check_eq_R1("monolis_shape_3d_tet_1st_node_point r2", r[2], expected[i][2]);
    }
}

void monolis_shape_3d_tet_1st_is_inside_domain_c_test()
{
    monolis_std_log_string("monolis_shape_3d_tet_1st_is_inside_domain");

    double inside_points[][3] = {
        {0.3, 0.3, 0.3}, {0.0, 0.0, 0.0}, {0.5, 0.0, 0.0}, {0.1, 0.1, 0.1}
    };
    double outside_points[][3] = {
        {-0.1, 0.1, 0.1}, {1.1, 0.0, 0.0}, {0.5, 0.5, 0.5}, {0.0, 1.0, 1.0}
    };

    bool is_inside;

    for (int i = 0; i < 4; i++) {
        monolis_shape_3d_tet_1st_is_inside_domain(inside_points[i], &is_inside);
        monolis_test_check_eq_I1("monolis_shape_3d_tet_1st_is_inside_domain inside", is_inside, 1);
    }

    for (int i = 0; i < 4; i++) {
        monolis_shape_3d_tet_1st_is_inside_domain(outside_points[i], &is_inside);
        monolis_test_check_eq_I1("monolis_shape_3d_tet_1st_is_inside_domain outside", is_inside, 0);
    }
}

void monolis_shape_3d_tet_1st_shapefunc_c_test()
{
    monolis_std_log_string("monolis_shape_3d_tet_1st_shapefunc");

    double test_point[3] = {0.3, 0.3, 0.3};
    double func[4];
    double expected[4] = {0.1, 0.3, 0.3, 0.3};

    monolis_shape_3d_tet_1st_shapefunc(test_point, func);

    for (int i = 0; i < 4; i++) {
        monolis_test_check_eq_R1("monolis_shape_3d_tet_1st_shapefunc", func[i], expected[i]);
    }
}

void monolis_shape_3d_tet_1st_shapefunc_deriv_c_test()
{
    monolis_std_log_string("monolis_shape_3d_tet_1st_shapefunc_deriv");

    double test_point[3] = {0.0, 0.0, 0.0};
    double* func[4];
    for (int i = 0; i < 4; i++) {
        func[i] = (double*)malloc(3 * sizeof(double));
    }

    monolis_shape_3d_tet_1st_shapefunc_deriv(test_point, func);

    double expected[4][3] = {
        {-1.0, -1.0, -1.0},
        { 1.0,  0.0,  0.0},
        { 0.0,  1.0,  0.0},
        { 0.0,  0.0,  1.0}
    };

    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 3; j++) {
            monolis_test_check_eq_R1("monolis_shape_3d_tet_1st_shapefunc_deriv", func[i][j], expected[i][j]);
        }
    }

    for (int i = 0; i < 4; i++) {
        free(func[i]);
    }
}

void monolis_shape_3d_tet_1st_test()
{
    monolis_shape_3d_tet_1st_num_integral_point_c_test();
    monolis_shape_3d_tet_1st_weight_c_test();
    monolis_shape_3d_tet_1st_integral_point_c_test();
    monolis_shape_3d_tet_1st_node_point_c_test();
    monolis_shape_3d_tet_1st_is_inside_domain_c_test();
    monolis_shape_3d_tet_1st_shapefunc_c_test();
    monolis_shape_3d_tet_1st_shapefunc_deriv_c_test();
}
