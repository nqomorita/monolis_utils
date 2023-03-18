#!/bin/bash

INP=driver/input
OUT=driver/output

./monolis_utils_c_test

mpirun -np 2 ./monolis_utils_c_test
