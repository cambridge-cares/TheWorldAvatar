#!/bin/bash
# Slurm job submission script for the SRM driver.
# It is being used by a wrapper script.
# DO NOT EXECUTE THIS DIRECTLY ON THE COMMAND LINE!
# DO NOT SUBMIT THIS DIRECTLY TO SLURM!

# This version of the script is specifically for running the 
# SRM Driver via the Docker KG system set up at CMCL.

#SBATCH -p test
#SBATCH -A test-account
#SBATCH --mem=4096M
#SBATCH --time=12:00:00
#SBATCH --ntasks=2
#SBATCH --output slurm.%u.%j.%N.stdout.txt   # (%u,%j,%N)=(user, job allocation #, node)  
#SBATCH --error slurm.%u.%j.%N.errout.txt    #
#SBATCH --mail-type=END,FAIL                 # notifications for job done & fail

echo -e "Running script as user... $USER"

# Set temporary folder for the SRM's execution
SCRATCH_DIRECTORY=/tmp/$USER/$SLURM_JOBID/
export SRMWORKINGDIR=$SCRATCH_DIRECTORY

# Extract input files archive to the execution folder
mkdir -p $SRMWORKINGDIR
unzip -j -d $SRMWORKINGDIR *.zip 

# Change to the driver's directory
SRMDIR=/usr/local/srm-driver/
cd $SRMDIR

# MPI configuration variables
numprocess=1
processpernode=1
SRM=./driver

# Execute the simulation
CMD="mpirun -ppn $processpernode -np $numprocess \"$SRM\" -w $SRMWORKINGDIR"
echo -e "\nExecuting command:\n$CMD\n==================\n"
eval $CMD

echo
echo 'Slurm job diagnostics:'
sacct --job $SLURM_JOBID --format "JobName,Submit,Elapsed,AveCPU,CPUTime,UserCPU,TotalCPU,NodeList,NTasks,AveDiskRead,AveDiskWrite"

# Pack all output files to output.zip (3 steps)
cd $SRMWORKINGDIR

zip -r output.zip .						
cp output.zip $SLURM_SUBMIT_DIR
cd $SLURM_SUBMIT_DIR
rm -rf $SCRATCH_DIRECTORY || exit $?
