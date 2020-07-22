#!/bin/bash
# Author: S. Mosbach (sm453@cam.ac.uk)
echo 'This wrapper script launches MoDS through Slurm.'
echo

#function usage {
#    echo "Usage: $0 ../../../outputs/Release/bin/MoDS_mpi hh:mm:ss <ntasks> \"<nodelist>\""
#    echo "<ntasks> is the total number of tasks to run. Maximal values:"
#    echo "    20 per CARES node, 16 per Darwin node"
#    echo "<nodelist> is a list of node names to run the job on:"
#    echo "    cpu-g-[1-20] are the CARES nodes, cpu-g-[21-36] are Darwin nodes"
#    echo "    (type sinfo to see currently idle nodes)"
#    echo "Warning: Underestimate the run-time and your job will be killed pre-maturely..."
#}

#if [ $# -ne 4 ]
#then
#    usage
#    exit
#fi

#if [ -f $1 ]
#then

#usremailadr=$(git config user.email)

#echo "Notification emails will be sent to: $usremailadr"
#echo '(NB Edit your git config in order to change this.)'
#echo
#echo 'Submitting job to Slurm...'

eval "$(conda shell.bash hook)"
conda activate pycantera

SCRATCH_DIRECTORY=/rds/user/$USER/hpc-work/scratch/$SLURM_JOBID/
export MODSDIR=$SCRATCH_DIRECTORY
mkdir -p $MODSDIR
cd $MODSDIR

cp $SLURM_SUBMIT_DIR/*.zip
mv *.zip input.zip
unzip input.zip

cd input

sbatch --mail-user=$usremailadr --job-name=SSTest --time=96:00:00 --ntasks=16 ./modsslurm_como.sh /home/jb2197/Codes_kinetics/mods-backend/outputs/Release/bin/MoDS_mpi

cp -pr $SCRATCH_DIRECTORY/* $SLURM_SUBMIT_DIR
cd $SLURM_SUBMIT_DIR
rm -rf $SCRATCH_DIRECTORY || exit $?


#echo "Type \"squeue --jobs=<JOBID>\" or \"squeue -u $USER\" to watch it."
#echo
#echo 'Done.'

#else

#echo "Executable \"$1\" not found."
#usage
#exit
#
#fi
