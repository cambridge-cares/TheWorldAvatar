#!/bin/bash
# Slurm job submission script for OSCML.
# It is being used by a wrapper script.
# DO NOT EXECUTE THIS DIRECTLY ON THE COMMAND LINE!
# DO NOT SUBMIT THIS DIRECTLY TO SLURM!

#SBATCH -p como
#SBATCH -A COMO-SL2-CPU
#SBATCH --nodes=1                            # -N number of nodes
#SBATCH --ntasks=1                           # total, <=number of nodes*cpus-per-task
#SBATCH --cpus-per-task=20
#SBATCH --cores-per-socket=10
#! How much real memory is required per node (in MB)? Not setting this
#! will lead to a default of (1/32)*total memory per task.
#! Setting a larger amount per task increases the number of cores.
##SBATCH --mem=   # 63900 is the maximum value allowed per node.
##SBATCH --output slurm.%u.%j.%N.stdout.txt   # (%u,%j,%N)=(user, job allocation #, node)  
##SBATCH --error slurm.%u.%j.%N.errout.txt    #
##SBATCH --mail-type=END,FAIL                 # notifications for job done & fail


CMD="python $@"
echo -e "\nExecuting command:\n$CMD\n==================\n"
eval $CMD


echo
echo 'Slurm job diagnostics:'
sacct --job $SLURM_JOBID --format "JobName,Submit,Elapsed,AveCPU,CPUTime,UserCPU,TotalCPU,NodeList,NTasks,AveDiskRead,AveDiskWrite"
