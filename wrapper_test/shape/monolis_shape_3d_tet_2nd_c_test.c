#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "monolis_utils.h"

#define TOL 1.0e-12

void monolis_shape_3d_tet_2nd_num_integral_point_c_test()
{
    monolis_std_log_string("monolis_shape_3d_tet_2nd_num_integral_point");

    int n = monolis_shape_3d_tet_2nd_num_integral_point();

    monolis_test_check_eq_I1("monolis_shape_3d_tet_2nd_num_integral_point", n, 4);
}

void monolis_shape_3d_tet_2nd_weight_c_test()
{
    monolis_std_log_string("monolis_shape_3d_tet_2nd_weight");

    double expected = 0.041666666666667;

    for (int i = 0; i < 4; i++) {
        double w = monolis_shape_3d_tet_2nd_weight(i);
        monolis_test_check_eq_R1("monolis_shape_3d_tet_2nd_weight", w, expected);
    }
}

void monolis_shape_3d_tet_2nd_integral_point_c_test()
{
    monolis_std_log_string("monolis_shape_3d_tet_2nd_integral_point");

    double expected[4][3] = {
        {0.138196601125011, 0.138196601125011, 0.138196601125011},
        {0.585410196624968, 0.138196601125011, 0.138196601125011},
        {0.138196601125011, 0.585410196624968, 0.138196601125011},
        {0.138196601125011, 0.138196601125011, 0.585410196624968}
    };

    double r[3];

    for (int i = 0; i < 4; i++) {
        monolis_shape_3d_tet_2nd_integral_point(i, r);
        monolis_test_check_eq_R1("monolis_shape_3d_tet_2nd_integral_point r0", r[0], expected[i][0]);
        monolis_test_check_eq_R1("monolis_shape_3d_tet_2nd_integral_point r1", r[1], expected[i][1]);
        monolis_test_check_eq_R1("monolis_shape_3d_tet_2nd_integral_point r2", r[2], expected[i][2]);
    }
}

void monolis_shape_3d_tet_2nd_is_inside_domain_c_test()
{
    monolis_std_log_string("monolis_shape_3d_tet_2nd_is_inside_domain");

    double inside_points[][3] = {
        {0.3, 0.3, 0.3}, {0.0, 0.0, 0.0}, {0.5, 0.0, 0.0}, {0.1, 0.1, 0.1}
    };
    double outside_points[][3] = {
        {-0.1, 0.1, 0.1}, {1.1, 0.0, 0.0}, {0.5, 0.5, 0.5}, {0.0, 1.0, 1.0}
    };

    bool is_inside;

    for (int i = 0; i < 4; i++) {
        monolis_shape_3d_tet_2nd_is_inside_domain(inside_points[i], &is_inside);
        monolis_test_check_eq_I1("monolis_shape_3d_tet_2nd_is_inside_domain inside", is_inside, 1);
    }

    for (int i = 0; i < 4; i++) {
        monolis_shape_3d_tet_2nd_is_inside_domain(outside_points[i], &is_inside);
        monolis_test_check_eq_I1("monolis_shape_3d_tet_2nd_is_inside_domain outside", is_inside, 0);
    }
}

void monolis_shape_3d_tet_2nd_shapefunc_c_test()
{
    monolis_std_log_string("monolis_shape_3d_tet_2nd_shapefunc");

    double test_point[3] = {0.3, 0.3, 0.3};
    double func[10];
    monolis_shape_3d_tet_2nd_shapefunc(test_point, func);

    double expected[10] = {
        (2.0 * (1.0 - 0.3 - 0.3 - 0.3) - 1.0) * (1.0 - 0.3 - 0.3 - 0.3),
        0.3 * (2.0 * 0.3 - 1.0),
        0.3 * (2.0 * 0.3 - 1.0),
        0.3 * (2.0 * 0.3 - 1.0),
        4.0 * 0.3 * (1.0 - 0.3 - 0.3 - 0.3),
        4.0 * 0.3 * 0.3,
        4.0 * 0.3 * (1.0 - 0.3 - 0.3 - 0.3),
        4.0 * 0.3 * (1.0 - 0.3 - 0.3 - 0.3),
        4.0 * 0.3 * 0.3,
        4.0 * 0.3 * 0.3
    };

    for (int i = 0; i < 10; i++) {
        monolis_test_check_eq_R1("monolis_shape_3d_tet_2nd_shapefunc", func[i], expected[i]);
    }
}

void monolis_shape_3d_tet_2nd_shapefunc_deriv_c_test()
{
    monolis_std_log_string("monolis_shape_3d_tet_2nd_shapefunc_deriv");

    double test_point[3] = {0.0, 0.0, 0.0};
    double* func[10];
    for (int i = 0; i < 10; i++) {
        func[i] = (double*)malloc(3 * sizeof(double));
    }

    monolis_shape_3d_tet_2nd_shapefunc_deriv(test_point, func);

    double expected[10][3] = {
        {1.0 - 4.0 * (1.0 - 0.0 - 0.0 - 0.0), 1.0 - 4.0 * (1.0 - 0.0 - 0.0 - 0.0), 1.0 - 4.0 * (1.0 - 0.0 - 0.0 - 0.0)},
        {4.0 * 0.0 - 1.0, 0.0, 0.0},
        {0.0, 4.0 * 0.0 - 1.0, 0.0},
        {0.0, 0.0, 4.0 * 0.0 - 1.0},
        {4.0 * (1.0 - 2.0 * 0.0 - 0.0 - 0.0), 4.0 * 0.0, -4.0 * 0.0},
        {4.0 * 0.0, 4.0 * 0.0, 0.0},
        {-4.0 * 0.0, 4.0 * (1.0 - 0.0 - 2.0 * 0.0 - 0.0), -4.0 * 0.0},
        {-4.0 * 0.0, -4.0 * 0.0, 4.0 * (1.0 - 0.0 - 0.0 - 2.0 * 0.0)},
        {4.0 * 0.0, 0.0, 4.0 * 0.0},
        {0.0, 4.0 * 0.0, 4.0 * 0.0}
    };

    for (int i = 0; i < 10; i++) {
        for (int j = 0; j < 3; j++) {
            monolis_test_check_eq_R1("monolis_shape_3d_tet_2nd_shapefunc_deriv", func[i][j], expected[i][j]);
        }
    }

    for (int i = 0; i < 10; i++) {
        free(func[i]);
    }
}

void monolis_shape_3d_tet_2nd_test()
{
    monolis_shape_3d_tet_2nd_num_integral_point_c_test();
    monolis_shape_3d_tet_2nd_weight_c_test();
    monolis_shape_3d_tet_2nd_integral_point_c_test();
    monolis_shape_3d_tet_2nd_is_inside_domain_c_test();
    monolis_shape_3d_tet_2nd_shapefunc_c_test();
    monolis_shape_3d_tet_2nd_shapefunc_deriv_c_test();
}
