#!/bin/bash

set -e

INP=driver/input
OUT=driver/output

./monolis_utils_c_test

mpirun --oversubscribe --allow-run-as-root -np 2 ./monolis_utils_c_test
