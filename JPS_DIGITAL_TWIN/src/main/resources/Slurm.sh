#!/bin/bash
# Slurm job submission script for Kinetics and SRM Engine Suite.
# It is being used by a wrapper script.
# DO NOT EXECUTE THIS DIRECTLY ON THE COMMAND LINE!
# DO NOT SUBMIT THIS DIRECTLY TO SLURM!
#SBATCH --mail-user=aravind@u.nus.edu
#SBATCH --mail-type=END
#SBATCH --nodes=1                            # -N number of nodes
#SBATCH --ntasks=20                          # total, <=number of nodes*16
#! How much real memory is required per node (in MB)? Not setting this
#! will lead to a default of (1/16)*total memory per task.
#! Setting a larger amount per task increases the number of cores.
#SBATCH --output slurm.%u.%j.%N.stdout.txt   # (%u,%j,%N)=(user, job allocation #, node)
#SBATCH --time=00:10:20                      #Wallclock time use.
#ln -s /home/adev01/gPROMSAgent_5154434330700/SRT_10.25.188.122_446653966157500/liblapack.so.3
echo "Starting of slurm"
#! Number of nodes and tasks per node allocated by SLURM (do not change):
numnodes=$SLURM_JOB_NUM_NODES
# Set folder for the execution
#SCRATCH_DIRECTORY=${HOME}/hpc-work/scratch/$SLURM_JOBID/
#mkdir -p $SCRATCH_DIRECTORY
# Extract input files to the execution folder
unzip -o *.zip -d .
#project_name=
#activity=$(cat input.json | jq .activity)
#output=$(cat input.json | jq .output)
#output_file_extension=$(cat input.json | jq .output_file_extension)
#password=$(cat input.json | jq .password)
#cd $SCRATCH_DIRECTORY
# Read input arguments
#exec=gORUN.exe
project_name=Final_HPC
activity=sim
output=fornow
output_file_extension=.gPLOT
password=Aravind123@@
echo "display command 1"
echo $1
#./executable.sh
CMD="gORUN.exe "$project_name" "$activity" "$output" "$password
echo $CMD
#echo -e "\nExecuting command:\n$CMD\n==================\n"
eval $CMD
#Execute the arguments
#CMD="gORUN.exe" $project_name $activity $output $password
#echo -e "\nExecuting command:\n$CMD\n==================\n"
#eval $CMD
#Copying output to the job folder
echo $PWD
cp $PWD/output/$output$output_file_extension .			
#cd $SLURM_SUBMIT_DIR
#rm -rf $SCRATCH_DIRECTORY || exit $?
