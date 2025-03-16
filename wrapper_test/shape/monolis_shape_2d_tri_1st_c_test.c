#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "monolis_utils.h"

#define TOL 1.0e-12

void monolis_shape_2d_tri_1st_num_integral_point_c_test()
{
    monolis_std_log_string("monolis_shape_2d_tri_1st_num_integral_point");

    int n = monolis_shape_2d_tri_1st_num_integral_point();

    monolis_test_check_eq_I1("monolis_shape_2d_tri_1st_num_integral_point", n, 3);
}

void monolis_shape_2d_tri_1st_weight_c_test()
{
    monolis_std_log_string("monolis_shape_2d_tri_1st_weight");

    double expected[3] = {0.166666666666666, 0.166666666666666, 0.166666666666666};

    for (int i = 0; i < 3; i++) {
        double w = monolis_shape_2d_tri_1st_weight(i);
        monolis_test_check_eq_R1("monolis_shape_2d_tri_1st_weight", w, expected[i]);
    }
}

void monolis_shape_2d_tri_1st_integral_point_c_test()
{
    monolis_std_log_string("monolis_shape_2d_tri_1st_integral_point");

    double expected[3][2] = {
        {0.166666666666667, 0.166666666666667},
        {0.666666666666667, 0.166666666666667},
        {0.166666666666667, 0.666666666666667}
    };

    double r[2];

    for (int i = 0; i < 3; i++) {
        monolis_shape_2d_tri_1st_integral_point(i, r);
        monolis_test_check_eq_R1("monolis_shape_2d_tri_1st_integral_point r0", r[0], expected[i][0]);
        monolis_test_check_eq_R1("monolis_shape_2d_tri_1st_integral_point r1", r[1], expected[i][1]);
    }
}

void monolis_shape_2d_tri_1st_node_point_c_test()
{
    monolis_std_log_string("monolis_shape_2d_tri_1st_node_point");

    double expected[3][2] = {
        {0.0, 0.0},
        {1.0, 0.0},
        {0.0, 1.0}
    };

    double r[2];

    for (int i = 0; i < 3; i++) {
        monolis_shape_2d_tri_1st_node_point(i, r);
        monolis_test_check_eq_R1("monolis_shape_2d_tri_1st_node_point r0", r[0], expected[i][0]);
        monolis_test_check_eq_R1("monolis_shape_2d_tri_1st_node_point r1", r[1], expected[i][1]);
    }
}

void monolis_shape_2d_tri_1st_is_inside_domain_c_test()
{
    monolis_std_log_string("monolis_shape_2d_tri_1st_is_inside_domain");

    double inside_points[][2] = {
        {0.5, 0.2}, {0.0, 0.0}, {0.3, 0.3}, {0.1, 0.8}
    };
    double outside_points[][2] = {
        {-0.1, 0.1}, {1.1, 0.0}, {0.5, 0.6}, {0.1, 1.1}
    };

    bool is_inside;

    for (int i = 0; i < 4; i++) {
        monolis_shape_2d_tri_1st_is_inside_domain(inside_points[i], &is_inside);
        monolis_test_check_eq_I1("monolis_shape_2d_tri_1st_is_inside_domain inside", is_inside, 1);
    }

    for (int i = 0; i < 4; i++) {
        monolis_shape_2d_tri_1st_is_inside_domain(outside_points[i], &is_inside);
        monolis_test_check_eq_I1("monolis_shape_2d_tri_1st_is_inside_domain outside", is_inside, 0);
    }
}

void monolis_shape_2d_tri_1st_shapefunc_c_test()
{
    monolis_std_log_string("monolis_shape_2d_tri_1st_shapefunc");

    double test_point[2] = {0.3, 0.3};
    double func[3];
    double expected[3] = {0.4, 0.3, 0.3};

    monolis_shape_2d_tri_1st_shapefunc(test_point, func);

    for (int i = 0; i < 3; i++) {
        monolis_test_check_eq_R1("monolis_shape_2d_tri_1st_shapefunc", func[i], expected[i]);
    }
}

void monolis_shape_2d_tri_1st_shapefunc_deriv_c_test()
{
    monolis_std_log_string("monolis_shape_2d_tri_1st_shapefunc_deriv");

    double test_point[2] = {0.0, 0.0};
    double* func[2];
    for (int i = 0; i < 2; i++) {
        func[i] = (double*)malloc(3 * sizeof(double));
    }

    monolis_shape_2d_tri_1st_shapefunc_deriv(test_point, func);

    double expected[2][3] = {
        {-1.0, 1.0, 0.0},
        {-1.0, 0.0, 1.0}
    };

    for (int i = 0; i < 2; i++) {
        for (int j = 0; j < 3; j++) {
            monolis_test_check_eq_R1("monolis_shape_2d_tri_1st_shapefunc_deriv", func[i][j], expected[i][j]);
        }
    }

    for (int i = 0; i < 2; i++) {
        free(func[i]);
    }
}

void monolis_shape_2d_tri_1st_test()
{
    monolis_shape_2d_tri_1st_num_integral_point_c_test();
    monolis_shape_2d_tri_1st_weight_c_test();
    monolis_shape_2d_tri_1st_integral_point_c_test();
    monolis_shape_2d_tri_1st_node_point_c_test();
    monolis_shape_2d_tri_1st_is_inside_domain_c_test();
    monolis_shape_2d_tri_1st_shapefunc_c_test();
    monolis_shape_2d_tri_1st_shapefunc_deriv_c_test();
}
