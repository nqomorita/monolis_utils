#!/bin/bash

./monolis_utils_test -test_i 1 -test_r 2.0 -test_s test_string -i a.txt -o b.txt -in c.txt -ie d.txt 2 10.0 20.0

mpirun --oversubscribe --allow-run-as-root -np 2 ./monolis_utils_test -test_i 1 -test_r 2.0 -test_s test_string -i a.txt -o b.txt -in c.txt -ie d.txt 2 10.0 20.0

cd driver

../../bin/monolis_dbc_all_surf_hex -i input/elem.hex.dat -o dbc.hex.dat 2 1.0 2.0

../../bin/monolis_dbc_all_surf_tet -i input/elem.tet.dat -o dbc.tet.dat 2 1.0 2.0

../../bin/monolis_extract_all_surf_hex -i input/elem.hex.dat -o surf.hex.dat

../../bin/monolis_extract_all_surf_tet -i input/elem.tet.dat -o surf.tet.dat

../../bin/monolis_p_refiner_tet -in input/node.tet.dat -ie input/elem.tet.dat -on node.ref.p.tet.dat -oe elem.ref.p.tet.dat

../../bin/monolis_h_refiner_tet -in input/node.tet.dat -ie input/elem.tet.dat -on node.ref.h.tet.dat -oe elem.ref.h.tet.dat

../../bin/monolis_h_refiner_hex -in input/node.hex.dat -ie input/elem.hex.dat -on node.ref.h.hex.dat -oe elem.ref.h.hex.dat
