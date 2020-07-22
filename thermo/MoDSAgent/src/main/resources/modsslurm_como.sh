#!/bin/bash
# Slurm job submission script for MoDS.
# It is being used by a wrapper script.
# DO NOT EXECUTE THIS DIRECTLY ON THE COMMAND LINE!
# DO NOT SUBMIT THIS DIRECTLY TO SLURM!

#SBATCH -p como
#SBATCH -A COMO-SL2-CPU
#SBATCH --mem=64000
#SBATCH --output slurm.%u.%j.%N.stdout.txt   # (%u,%j,%N)=(user, job allocation #, node)  
#SBATCH --error slurm.%u.%j.%N.errout.txt    #
#SBATCH --mail-type=END,FAIL                 # notifications for job done & fail

#! Number of nodes and tasks per node allocated by SLURM (do not change):
numnodes=$SLURM_JOB_NUM_NODES
mpi_tasks_per_node=$(echo "$SLURM_TASKS_PER_NODE" | sed -e  's/^\([0-9][0-9]*\).*$/\1/')

#! Number of MPI tasks to be started by the application per node and in total (do not change):
np=$[${numnodes}*${mpi_tasks_per_node}]

CMD="mpirun -ppn $mpi_tasks_per_node -np $np \"$1\""
echo -e "\nExecuting command:\n$CMD\n==================\n"
eval $CMD

echo
echo 'Slurm job diagnostics:'
sacct --job $SLURM_JOBID --format "JobName,Submit,Elapsed,AveCPU,CPUTime,UserCPU,TotalCPU,NodeList,NTasks,AveDiskRead,AveDiskWrite"
