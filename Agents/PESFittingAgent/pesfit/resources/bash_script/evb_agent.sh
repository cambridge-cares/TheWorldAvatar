#!/bin/bash

# create FIELD files for state 1 and state 2

chmod 777 run_dl_field.sh
./run_dl_field.sh

# modify charges in FIELD and create CONFIG files for all scan points (state 1 and state 2) (DL_POLY inputs)

chmod 777 run_dl_poly.sh
./run_dl_poly.sh

chmod 777 run_mods.sh
./run_mods.sh

chmod 777 run_validation.sh
./run_validation.sh

chmod 777 plot_results.sh
./plot_results.sh