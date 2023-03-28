#!/bin/bash

INPF=driver/input.f
OUTF=driver/output.f
INPC=driver/input.c
OUTC=driver/output.c

../bin/monolis_dbc_all_surf_hex -i ${INPF}/elem.hex.dat -o ${OUTF}/dbc.hex.dat 2 1.0 2.0

../bin/monolis_dbc_all_surf_tet -i ${INPF}/elem.tet.dat -o ${OUTF}/dbc.tet.dat 2 1.0 2.0

../bin/monolis_extract_all_surf_hex -i ${INPF}/elem.hex.dat -o ${OUTF}/surf.hex.dat

../bin/monolis_extract_all_surf_tet -i ${INPF}/elem.tet.dat -o ${OUTF}/surf.tet.dat

../bin/monolis_p_refiner_tet -in ${INPF}/node.tet.dat -ie ${INPF}/elem.tet.dat -on ${OUTF}/node.ref.p.tet.dat -oe ${OUTF}/elem.ref.p.tet.dat

../bin/monolis_h_refiner_tet -in ${INPF}/node.tet.2elem.dat -ie ${INPF}/elem.tet.2elem.dat -on ${OUTF}/node.ref.h.tet.dat -oe ${OUTF}/elem.ref.h.tet.dat

../bin/monolis_h_refiner_hex -in ${INPF}/node.hex.2elem.dat -ie ${INPF}/elem.hex.2elem.dat -on ${OUTF}/node.ref.h.hex.dat -oe ${OUTF}/elem.ref.h.hex.dat

../bin/monolis_dbc_all_surf_hex -i ${INPC}/elem.hex.dat -o ${OUTC}/dbc.hex.dat 2 1.0 2.0

../bin/monolis_dbc_all_surf_tet -i ${INPC}/elem.tet.dat -o ${OUTC}/dbc.tet.dat 2 1.0 2.0

../bin/monolis_extract_all_surf_hex -i ${INPC}/elem.hex.dat -o ${OUTC}/surf.hex.dat

../bin/monolis_extract_all_surf_tet -i ${INPC}/elem.tet.dat -o ${OUTC}/surf.tet.dat

../bin/monolis_p_refiner_tet -in ${INPC}/node.tet.dat -ie ${INPC}/elem.tet.dat -on ${OUTC}/node.ref.p.tet.dat -oe ${OUTC}/elem.ref.p.tet.dat

../bin/monolis_h_refiner_tet -in ${INPC}/node.tet.2elem.dat -ie ${INPC}/elem.tet.2elem.dat -on ${OUTC}/node.ref.h.tet.dat -oe ${OUTC}/elem.ref.h.tet.dat

../bin/monolis_h_refiner_hex -in ${INPC}/node.hex.2elem.dat -ie ${INPC}/elem.hex.2elem.dat -on ${OUTC}/node.ref.h.hex.dat -oe ${OUTC}/elem.ref.h.hex.dat

./monolis_utils_test -test_i 1 -test_r 2.0 -test_s test_string -i a.txt -o b.txt -in c.txt -ie d.txt -n 2 2 10.0 20.0

mpirun --oversubscribe --allow-run-as-root -np 2 ./monolis_utils_test -test_i 1 -test_r 2.0 -test_s test_string -i a.txt -o b.txt -in c.txt -ie d.txt -n 2 2 10.0 20.0
