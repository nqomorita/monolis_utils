#!/bin/bash

INP=driver/input
OUT=driver/output

../bin/monolis_dbc_all_surf_hex -i ${INP}/elem.hex.dat -o ${OUT}/dbc.hex.dat 2 1.0 2.0

../bin/monolis_dbc_all_surf_tet -i ${INP}/elem.tet.dat -o ${OUT}/dbc.tet.dat 2 1.0 2.0

../bin/monolis_extract_all_surf_hex -i ${INP}/elem.hex.dat -o ${OUT}/surf.hex.dat

../bin/monolis_extract_all_surf_tet -i ${INP}/elem.tet.dat -o ${OUT}/surf.tet.dat

../bin/monolis_p_refiner_tet -in ${INP}/node.tet.dat -ie ${INP}/elem.tet.dat -on ${OUT}/node.ref.p.tet.dat -oe ${OUT}/elem.ref.p.tet.dat

../bin/monolis_h_refiner_tet -in ${INP}/node.tet.dat -ie ${INP}/elem.tet.dat -on ${OUT}/node.ref.h.tet.dat -oe ${OUT}/elem.ref.h.tet.dat

../bin/monolis_h_refiner_hex -in ${INP}/node.hex.dat -ie ${INP}/elem.hex.dat -on ${OUT}/node.ref.h.hex.dat -oe ${OUT}/elem.ref.h.hex.dat

./monolis_utils_test -test_i 1 -test_r 2.0 -test_s test_string -i a.txt -o b.txt -in c.txt -ie d.txt 2 10.0 20.0

mpirun --oversubscribe --allow-run-as-root  -np 2 ./monolis_utils_test -test_i 1 -test_r 2.0 -test_s test_string -i a.txt -o b.txt -in c.txt -ie d.txt 2 10.0 20.0
