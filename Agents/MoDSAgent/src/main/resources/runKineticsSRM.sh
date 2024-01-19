#!/bin/bash

unset "${!I_MPI_@}"
unset "${!SLURM_@}"
unset "${!PMI_@}"
unset "${!MPIR_CVAR_@}"

unset HYDI_CONTROL_FD
unset GFORTRAN_UNBUFFERED_PRECONNECTED
unset DAPL_NETWORK_PROCESS_NUM

CURRENT_DIR=$PWD
pushd $1
./$2 -w $CURRENT_DIR/
popd
