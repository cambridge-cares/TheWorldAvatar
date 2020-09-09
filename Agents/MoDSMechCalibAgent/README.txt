1. Install Cantera to a conda environment on CSD3:

Please follow Option 1 or 2 in: 
https://cantera.org/install/conda-install.html
Then modify the ${json.cantera.environment} in input.json with the name of environment where Cantera is installed. 
Note: It is required to set conda base environment as the default envrionment once login to CSD3, this should be true once conda is installed. Otherwise, please uncomment lines between "# >>> conda initialise >>>" and "# <<< conda initialize <<<" in /home/user/.bashrc file. 

2. Upload executable srm_driver64_20200819build and UnitDictionary.xml to same folder on CSD3. 
Then modify the ${kinetics.folder.path} in xxxagent-properties file with the absolute path to the folder where both files are located. 

3. Upload executable MoDS_mpi to a folder on CSD3. 
Then modify the ${json.mods.executable.path} in input.json with the absolute path to execute MoDS_mpi, i.e., the path should end with /MoDS_mpi. 

4. The path to execute AutoMechCalibAgent:
/AutoMechCalibAgent/coordination/request
The path to execute MoDSSensAnaAgent:
/MoDSSeneAnaAgent/job/request
The path to execute MoDSMechCalibAgent:
/MoDSMechCalibAgent/job/request