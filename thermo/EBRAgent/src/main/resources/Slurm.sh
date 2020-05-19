#!/bin/bash
# Slurm job submission script for Kinetics and SRM Engine Suite.
# It is being used by a wrapper script.
# DO NOT EXECUTE THIS DIRECTLY ON THE COMMAND LINE!
# DO NOT SUBMIT THIS DIRECTLY TO SLURM!

#SBATCH -p como
#SBATCH -A COMO-SL2-CPU
#SBATCH --nodes=1                            # -N number of nodes
#SBATCH --tasks=1                          # total, <=number of nodes*32
#SBATCH --cpus-per-task=16                         #
#! How much real memory is required per node (in MB)? Not setting this
#! will lead to a default of (1/16)*total memory per task.
#! Setting a larger amount per task increases the number of cores.
#SBATCH --output slurm.%u.%j.%N.stdout.txt   # (%u,%j,%N)=(user, job allocation #, node)
#SBATCH --error slurm.%u.%j.%N.errout.txt    #
#SBATCH --time=00:20:00                      #Wallclock time use.
#SBATCH --mail-type=END,FAIL                 # notifications for job done & fail

#! Optionally modify the environment seen by the application
#! (note that SLURM reproduces the environment at submission irrespective of ~/.bashrc):

#! Number of nodes and tasks per node allocated by SLURM (do not change):
numnodes=$SLURM_JOB_NUM_NODES
numtasks=$SLURM_NTASKS
#! mpi_tasks_per_node=$(echo "$SLURM_TASKS_PER_NODE" | sed -e  's/^\([0-9][0-9]*\).*$/\1/')

#! Number of MPI tasks to be started by the application per node and in total (do not change):
#! np=$[${numnodes}*${mpi_tasks_per_node}]

SCRATCH_DIRECTORY=/rds/user/$USER/hpc-work/scratch/$SLURM_JOBID/
export TMPDIR=$SCRATCH_DIRECTORY
mkdir -p $TMPDIR
cd $TMPDIR

cp $SLURM_SUBMIT_DIR/$SLURM_JOB_NAME.json .
cp $SLURM_SUBMIT_DIR/comoenthalpyestimationpaper.jar .
cp $SLURM_SUBMIT_DIR/*.zip .
mv *.zip input.zip
#cp $SLURM_SUBMIT_DIR/$SLURM_JOB_NAME.chk .
#cp $SLURM_SUBMIT_DIR/$SLURM_JOB_NAME.rwf .

#Run Java
java -jar comoenthalpyestimationpaper.jar input.json

echo
echo 'Slurm job diagnostics:'
#sacct --job $SLURM_JOBID --format "JobName,Submit,Elapsed,AveCPU,CPUTime,UserCPU,TotalCPU,NodeList,NTasks,AveDiskRead,AveDiskWrite"

cp -pr $SCRATCH_DIRECTORY/* $SLURM_SUBMIT_DIR
cd $SLURM_SUBMIT_DIR
rm -rf $SCRATCH_DIRECTORY || exit $?
