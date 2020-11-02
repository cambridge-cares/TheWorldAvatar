There are 3 test cases for EPISODE in CSD3. 

testcase1:  for singapore area (70km by 70km) with dx=dy=1km (this is a case showing the very 1st simulation that with "restart" variable =0)
testcase2:  for hongkong (40km by 40km) with dx=dy=1km (this is a case showing the very 1st simulation that with "restart" variable =0)
testcase3:  for hongkong (40km by 40km) with dx=dy=1km (this is a restart case that requires two additional input files - "plume_segments" and "icmhour_2020_moving00.nc" that generated from last hour's simulation).

bashrc-example is an example of a compatible bashrc file to run the simulations successfully.

To run these test cases, run the following commands:
cp -r ~/Codes/JParkSimulator-git/JPS_CITY_CHEM/src/main/resources/* ~/
cd ~/testcase1/
sbatch --job-name=input Slurm.sh

WARNING: It is not possible to run two simulations at the same time due to limitations with the Episode code, so you must wait for one simulation to complete before submitting another one.

Once the first test case completes, proceed to run the rest, i.e.:
cd ~/testcase2/
sbatch --job-name=input Slurm.sh

wait for simulation to complete, then,

cd ~/testcase3/
sbatch --job-name=input Slurm.sh

The main output file to compare is 3D_instantanous_mainconc_center.dat, reference output files are available for each test case.