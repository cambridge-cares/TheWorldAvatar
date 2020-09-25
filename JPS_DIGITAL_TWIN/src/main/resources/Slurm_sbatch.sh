#!/bin/bash
# Slurm job submission script for the SRM driver.
# It is being used by a wrapper script.
# DO NOT EXECUTE THIS DIRECTLY ON THE COMMAND LINE!
# DO NOT SUBMIT THIS DIRECTLY TO SLURM!
#SBATCH --time=00:20:00
#SBATCH --ntasks=16
#SBATCH --nodes=1                            # -N number of nodes
#SBATCH --output slurm.%u.%j.%N.stdout.txt   # (%u,%j,%N)=(user, job allocation #, node)  
#SBATCH --error slurm.%u.%j.%N.errout.txt    #
#SBATCH --mail-type=END,FAIL                 # notifications for job done & fail
#! Number of nodes and tasks per node allocated by SLURM (do not change):
numnodes=$SLURM_JOB_NUM_NODES
mpi_tasks_per_node=$(echo "$SLURM_TASKS_PER_NODE" | sed -e  's/^\([0-9][0-9]*\).*$/\1/')
# Set folder for the execution
#SCRATCH_DIRECTORY=${HOME}/hpc-work/scratch/$SLURM_JOBID/
#mkdir -p $SCRATCH_DIRECTORY
# Extract input files to the execution folder
unzip -o *.zip -d .
#cd $SCRATCH_DIRECTORY
# Read input arguments
#project_name=$(cat input.json | jq .project_name)
#activity=$(cat input.json | jq .activity)
#output=$(cat input.json | jq .output)
#output_file_extension=$(cat input.json | jq .output_file_extension)
#password=$(cat input.json | jq .password)
#Execute the arguments
#CMD="gORUN.exe" $project_name $activity $output $password
export CMD="gORUN.exe Final_HPC sim Fornow Aravind123@@"
echo -e "\nExecuting command:\n$CMD\n==================\n"
eval $CMD
#Copying output to the job folder
#cp /output/$output$output_file_extension $SLURM_SUBMIT_DIR				
#cd $SLURM_SUBMIT_DIR
#rm -rf $SCRATCH_DIRECTORY || exit $?