#!/bin/bash

./monolis_utils_test -test_i 1 -test_r 2.0 -test_s test_string -i a.txt -o b.txt -in c.txt -ie d.txt 2 10.0 20.0

mpirun -np 2 ./monolis_utils_test -test_i 1 -test_r 2.0 -test_s test_string -i a.txt -o b.txt -in c.txt -ie d.txt 2 10.0 20.0
