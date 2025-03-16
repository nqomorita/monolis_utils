#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "monolis_utils.h"

#define TOL 1.0e-12

void monolis_shape_2d_tri_2nd_num_integral_point_c_test()
{
    monolis_std_log_string("monolis_shape_2d_tri_2nd_num_integral_point");

    int n = monolis_shape_2d_tri_2nd_num_integral_point();

    monolis_test_check_eq_I1("monolis_shape_2d_tri_2nd_num_integral_point", n, 4);
}

void monolis_shape_2d_tri_2nd_weight_c_test()
{
    monolis_std_log_string("monolis_shape_2d_tri_2nd_weight");

    double expected = 0.5;

    for (int i = 0; i < 4; i++) {
        double w = monolis_shape_2d_tri_2nd_weight(i);
        monolis_test_check_eq_R1("monolis_shape_2d_tri_2nd_weight", w, expected);
    }
}

void monolis_shape_2d_tri_2nd_integral_point_c_test()
{
    monolis_std_log_string("monolis_shape_2d_tri_2nd_integral_point");

    double expected[4][2] = {
        {0.166666666666667, 0.166666666666667},
        {0.666666666666667, 0.166666666666667},
        {0.166666666666667, 0.666666666666667},
        {0.333333333333333, 0.333333333333333}
    };

    double r[2];

    for (int i = 0; i < 4; i++) {
        monolis_shape_2d_tri_2nd_integral_point(i, r);
        monolis_test_check_eq_R1("monolis_shape_2d_tri_2nd_integral_point r0", r[0], expected[i][0]);
        monolis_test_check_eq_R1("monolis_shape_2d_tri_2nd_integral_point r1", r[1], expected[i][1]);
    }
}

void monolis_shape_2d_tri_2nd_is_inside_domain_c_test()
{
    monolis_std_log_string("monolis_shape_2d_tri_2nd_is_inside_domain");

    double inside_points[][2] = {
        {0.3, 0.3}, {0.0, 0.0}, {0.5, 0.0}, {0.1, 0.1}
    };
    double outside_points[][2] = {
        {-0.1, 0.1}, {1.1, 0.0}, {0.5, 0.5}, {0.0, 1.0}
    };

    bool is_inside;

    for (int i = 0; i < 4; i++) {
        monolis_shape_2d_tri_2nd_is_inside_domain(inside_points[i], &is_inside);
        monolis_test_check_eq_I1("monolis_shape_2d_tri_2nd_is_inside_domain inside", is_inside, 1);
    }

    for (int i = 0; i < 4; i++) {
        monolis_shape_2d_tri_2nd_is_inside_domain(outside_points[i], &is_inside);
        monolis_test_check_eq_I1("monolis_shape_2d_tri_2nd_is_inside_domain outside", is_inside, 0);
    }
}

void monolis_shape_2d_tri_2nd_shapefunc_c_test()
{
    monolis_std_log_string("monolis_shape_2d_tri_2nd_shapefunc");

    double test_point[2] = {0.3, 0.3};
    double func[6];
    monolis_shape_2d_tri_2nd_shapefunc(test_point, func);

    double st = 1.0 - 0.3 - 0.3;
    double xi = 0.3;
    double et = 0.3;
    double expected[6] = {
        st * (2.0 * st - 1.0),
        xi * (2.0 * xi - 1.0),
        et * (2.0 * et - 1.0),
        4.0 * xi * st,
        4.0 * xi * et,
        4.0 * et * st
    };

    for (int i = 0; i < 6; i++) {
        monolis_test_check_eq_R1("monolis_shape_2d_tri_2nd_shapefunc", func[i], expected[i]);
    }
}

void monolis_shape_2d_tri_2nd_shapefunc_deriv_c_test()
{
    monolis_std_log_string("monolis_shape_2d_tri_2nd_shapefunc_deriv");

    double test_point[2] = {0.0, 0.0};
    double* func[6];
    for (int i = 0; i < 6; i++) {
        func[i] = (double*)malloc(2 * sizeof(double));
    }

    monolis_shape_2d_tri_2nd_shapefunc_deriv(test_point, func);

    double expected[6][2] = {
        {1.0 - 4.0 * (1.0 - 0.0 - 0.0), 1.0 - 4.0 * (1.0 - 0.0 - 0.0)},
        {4.0 * 0.0 - 1.0, 0.0},
        {0.0, 4.0 * 0.0 - 1.0},
        {4.0 * (1.0 - 0.0 - 0.0 - 0.0), 4.0 * 0.0},
        {4.0 * 0.0, 4.0 * 0.0},
        {4.0 * 0.0, 4.0 * (1.0 - 0.0 - 0.0 - 0.0)}
    };

    for (int i = 0; i < 6; i++) {
        for (int j = 0; j < 2; j++) {
            monolis_test_check_eq_R1("monolis_shape_2d_tri_2nd_shapefunc_deriv", func[i][j], expected[i][j]);
        }
    }

    for (int i = 0; i < 6; i++) {
        free(func[i]);
    }
}

/*
void monolis_shape_2d_tri_2nd_shapefunc_2nd_deriv_c_test()
{
    monolis_std_log_string("monolis_shape_2d_tri_2nd_shapefunc_2nd_deriv");

    double func[6][2][2];
    monolis_shape_2d_tri_2nd_shapefunc_2nd_deriv(func);

    double expected[6][2][2] = {
        {{4.0, 4.0}, {4.0, 4.0}},
        {{4.0, 0.0}, {0.0, 0.0}},
        {{0.0, 0.0}, {0.0, 4.0}},
        {{-8.0, -4.0}, {-4.0, 0.0}},
        {{0.0, 4.0}, {4.0, 0.0}},
        {{0.0, -4.0}, {-4.0, -8.0}}
    };

    for (int i = 0; i < 6; i++) {
        for (int j = 0; j < 2; j++) {
            for (int k = 0; k < 2; k++) {
                monolis_test_check_eq_R1("monolis_shape_2d_tri_2nd_shapefunc_2nd_deriv", func[i][j][k], expected[i][j][k]);
            }
        }
    }
}
*/

void monolis_shape_2d_tri_2nd_test()
{
    monolis_shape_2d_tri_2nd_num_integral_point_c_test();
    monolis_shape_2d_tri_2nd_weight_c_test();
    monolis_shape_2d_tri_2nd_integral_point_c_test();
    monolis_shape_2d_tri_2nd_is_inside_domain_c_test();
    monolis_shape_2d_tri_2nd_shapefunc_c_test();
    monolis_shape_2d_tri_2nd_shapefunc_deriv_c_test();
    //monolis_shape_2d_tri_2nd_shapefunc_2nd_deriv_c_test();
}
