#!/bin/bash

./monolis_utils_test -test_i 1 -test_r 2.0 -test_s test_string

mpirun -np 2 ./monolis_utils_test -test_i 1 -test_r 2.0 -test_s test_string
