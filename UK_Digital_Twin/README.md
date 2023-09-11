# README for OptimalPowerFlowAnalysis.py
## Discription
'/TheWorldAvatar/UK_Digital_Twin/UK_Power_System_SMR_Replacement/SMR_Replacement/OptimalPowerFlowAnalysis.py' is the main script for conducting the SMR replacement with the Optimal Power Flow (OPF) analysis. This script can also conduct the OPF analysis without SMR replacement. The main script produces results that can be provided as inputs to post processing methods.

The following are the steps for setting up the environment for the VSCode IDE and running the main script.

Step 1: preparation of the environment 
- Install WSL and Ubuntu.
- Clone the required git repository.
- Install and use the VSCode IDE.
- Install Open JDK by running the command `sudo apt-get install -y openjdk-11-jdk-headless`.
- Install conda on Ubuntu using the instructions below:
   `wget https://repo.anaconda.com/archive/Anaconda3-2023.07-1-Linux-x86_64.sh`
   `chmod +x ./Anaconda3-2023.07-1-Linux-x86_64.sh`
   `./Anaconda3-2023.07-1-Linux-x86_64.sh`
   `source ~/.bashrc`
- Check the version installed:
   `conda --version`
   If it shows the version number, you have succssfully installed conda.
- Create a conda environment by following the instruction below:
   `conda create --name env_ukdtpowsys python=3.8`
   You can check whether the envionment is created by running `conda info --envs`
- Activate the conda environment:
   `conda activate env_ukdtpowsys`
- Following the activation of the conda environment, install the required packages by running the command `pip install -r requirements.txt`. Note: we assume that you are running the command at the same path of this README file.

Step 2: How to build the base world knowledge graph for the UK power system?
- 2.1 Check the data files: check the folder `\UK_Digital_Twin\Data files\DUKES`. There would be folders with the data pubulished by UK.GOV in different years, e.g. 2019. Go to the folder called `2019`. The .csv files there are the processed files based on the DUKES raw data file `\UK_Digital_Twin\Data file\DUKES\2019\DUKES2019.xls` that was downloaded from `https://www.gov.uk/government/statistics/electricity-chapter-5-digest-of-united-kingdom-energy-statistics-dukes`. In that page there are many files and the excel file named 'Power stations in the United Kingdom (DUKES 5.11)' was downloaded. Columns relevant to the study are copied from the excel sheet (tab) named `5.11 Full list` within this excel file and pasted in producing the CSV files, e.g., plantname.csv, planttype.csv, etc. Each CSV file has exactly the same number of rows. Among the .csv files, only the 'gpslocation.csv' is manully created by collecting coordinates from the Google maps which contain all the lat-lon locations of the power plants listed in DUKES. After 2022, the original DUKES data started reporting the GPS locations but they are not accurate enough. Therefore, till 2023 the location of the power plant is still based on the manually collected one.
- 2.2 Create namespaces for building the base world knowledge graph: By default, Blazegraph is the triple store used for managing the KG. The creation of namespaces in the Blazegraph endpoint was done manually in this work. The recommended names of the namespaces are `ONS_subset`, `UKPopulationData` and `UKPowerSystemBaseWorld`. All these namespaces should be created with the option `Enable geospatial` ticked. Create `ONS_subset` to manage the subset of ONS data which was available from `https://statistics.data.gov.uk/sparql`. Create `UKPopulationData` and `UKPowerSystemBaseWorld` to store the triples describing the base world of the UK power system. The namespace `UKPopulationData` is specifically designed to contain the population data while `UKPowerSystemBaseWorld` is for the other data. After the creation of these three namespaces, set up the endpoint IRIs in `\UK_Digital_Twin\UK_Digital_Twin_Package\endPoint.py`.
- 2.3 Create a replica of the required subset of ONS data: Copy all the files except the batch (.bat) files from Dropbox by following the instructions below:

- Copy all the files from `Dropbox (Cambridge University)\CoMo shared\wx243\c4e-wx243-TWAPowerSystem\Codes\resources\ONSData\ONSKG_AreaBoundariesAndLACode` to `\UK_Digital_Twin\resources\ONSData\ONSKG_AreaBoundariesAndLACode`

- Copy all the files from `Dropbox (Cambridge University)\CoMo shared\wx243\c4e-wx243-TWAPowerSystem\Codes\resources\ONSData\ONSKG_WithinRelations` to `\UK_Digital_Twin\resources\ONSData\ONSKG_WithinRelations`

- Copy all the files from `Dropbox (Cambridge University)\CoMo shared\wx243\c4e-wx243-TWAPowerSystem\Codes\resources\ONSData\ONSKG_AreaCode` to `\UK_Digital_Twin\resources\ONSData\ONSKG_AreaCode`

- Copy all the files from `Dropbox (Cambridge University)\CoMo shared\wx243\c4e-wx243-TWAPowerSystem\Codes\resources\ONSData\ONSObsolete` to `\UK_Digital_Twin\resources\ONSData\ONSObsolete`

- Copy all the files from `Dropbox (Cambridge University)\CoMo shared\wx243\c4e-wx243-TWAPowerSystem\Codes\resources\ONSData\WithinGB` to `\UK_Digital_Twin\resources\ONSData\WithinGB`

Now, simply run `ons_data_uploader.py` from the path of `\UK_Digital_Twin\UK_Power_System_Base_World_Initialiser\UploadONS\` using the following command if you are at the UK_Digital_Twin folder:

   `python UK_Power_System_Base_World_Initialiser/UploadONS/ons_data_uploader.py`

Note: It is assumed that you have already uploaded all ONS Data required for this project to the KG, so you can go to item 2.4.

In case, you are intrested in downloading ONS Data from the ONS Endpoint on your own without using the files from Dropbox, you can follow the steps below.

Run the batch files from the following paths in a Command-Line Interface (CLI) or terminal: 

`\UK_Digital_Twin\resources\ONSBatchFiles\ONSKG_AreaBoundariesAndLACode`
`\UK_Digital_Twin\resources\ONSBatchFiles\ONSKG_WithinRelations`
`\UK_Digital_Twin\resources\ONSBatchFiles\ONSKG_AreaCode`
`\UK_Digital_Twin\resources\ONSBatchFiles\ONSObsolete`
`\UK_Digital_Twin\resources\ONSBatchFiles\WithinGB`

 If you are running this code from the Linux environment, use the following command before running a batch file.

   `chmod +x <BATCH_FILE_NAME.bat>`

One example is given below regarding how to run the above command. For the batch file onsareaboundariesandlacode.bat, run the following.

   `chmod +x onsareaboundariesandlacode.bat`

Now run the following command to execute a batch file.

   `./<BATCH_FILE_NAME.bat>`

One example is given below regarding how to run the above command. For the batch file onsareaboundariesandlacode.bat, run the following.

   `./onsareaboundariesandlacode.bat`

Run all batch files available in those paths. While running a batch file, you will see .gz files being created at its same level.

This will query the original ONS Endpoint and download the required data in .gz format at the same path of the batch file. In Linux, you can unzip the .gz files using the following command:

   `gunzip *.gz`

Now make sure that the Blazegraph namespace `ONS_subset` does not contain any triples. You can delete the existing `ONS_subset` namespace and create it newly. Do not forget to enable the geospatial option.

Note: We preferred to create a replica to avoid the issue of query failure due to several reasons, for example, change of administrative area code hierarchy. Nevertheless, if you do not want to create a replica of ONS data locally and are interested in using the ONS Endpoint directly, simply put the endpoint IRI of `ONSEndpoint` as `http://statistics.data.gov.uk/sparql.json` in `\UK_Digital_Twin\UK_Digital_Twin_Package\endPoint.py`, and set the `type` of `ONS` in `\UK_Digital_Twin\UK_Digital_Twin_Package\EndPointConfigAndBlazegraphRepoLabel.py` as `ORIGINAL`. 

An example ONS structure with the type of 'ORIGINAL' is provided below:
ONS = {
    'label': 'ons',
    'type':'ORIGINAL',
    'endpoint_iri' : ONSEndpoint,
    'queryendpoint_iri' : ONSEndpoint,
    'updateendpoint_iri' : ONSEndpoint}


- 2.4 Create the base world: Go to `\UK_Digital_Twin\UK_Power_System_Base_World_Initialiser\` and run `UKPowerSystem_BaseWorld.py` using the following command.

   `python UK_Power_System_Base_World_Initialiser/UKPowerSystem_BaseWorld.py`

This Python module will upload all UK power plants and their attributes including name, GPS location, related LA Code, owner, commission year, installed capacity, fuel type and technology type. It uploads UK electricity consumption data for each local administrative area as well as UK population data. It also uploads 10-bus topology and 29-bus topology of the UK power transmission network.

Note: For the reproducability of results reported in Preprint 312, you do not need to modify the `config.json` file available in this folder. For running the code with data from a different year like 2022, modify the `config.json` file as follows:

{
    "DUKESDataVersion": "2022",
    "startTime_of_EnergyConsumption":"2017",
    "ifUpdateToTripleStore": "True",
    ...
}

Similarly, you can change also year of energy consumption data.

Step 3 (OPTIONAL): Ready for conducting the scenario analysis of SMR replacement with optimal power flow (OPF) analysis?
- 3.1 Pre-requirement: If you want to post-process the result produced by SMR replacement, copy all the files from the path of `Dropbox (Cambridge University)\CoMo shared\wx243\c4e-wx243-TWAPowerSystem\Codes\UK_Digital_Twin\resources\RegionalBreakdown_images` to the `UK_Digital_Twin\resources\required_images` folder.

- 3.2 Conduct SMR Replacement: 
Set up the attributes of the object
- topologyNodeIRI: There are two bus topologies, 10 bus model and 29 bus model. 
	topologyNodeIRI_10Bus = "http://www.theworldavatar.com/kb/ontoenergysystem/PowerGridTopology_b22aaffa-fd51-4643-98a3-ff72ee04e21e" 
    topologyNodeIRI_29Bus = "http://www.theworldavatar.com/kb/ontoenergysystem/PowerGridTopology_6017554a-98bb-4896-bc21-e455cb6b3958" 

- eliminateClosedPlantIRIList: identifies the power plants which are not going to be considered in OPF.

- agentIRI: specify the IRI of the agent which perform the current work (This attribute is relevant to the markup of the results which is so far not fully functionalised. Therefore, a fake IRI is provided.)  

- slackBusNodeIRI: specify the IRI of the slack bus of the bus model.
	slackBusNodeIRI_10Bus = "http://www.theworldavatar.com/kb/ontopowsys/BusNode_1f3c4462-3472-4949-bffb-eae7d3135591" 
   slackBusNodeIRI_29Bus = "http://www.theworldavatar.com/kb/ontopowsys/BusNode_bc386bcb-33ab-4569-80c5-00dc9d0bffb8"

- loadAllocatorName: specify the name of the method used for allocate the demand to the bus, there are two avaliable methods 'regionalDemandLoad' and 'closestDemandLoad'.
	'regionalDemandLoad' is to allocate the demand of the same offical region of the UK to the bus in this region. This method is originally design to the 10 bus model in which each region only has one bus. This method cannot deal with the situation that each region has more than one bus but it is suitable for more than one region sharing the same bus. This method is a relatively rough way for assigning the demand which is not always recommanded.
	'closestDemandLoad' is the method that allocates the demand to its closet bus which can be used for any bus model.

- EBusModelVariableInitialisationMethodName: specify the method name of the initialising the bus specifications. There are two methods avaliable, "defaultInitialisation" and "preSpecified".
	"defaultInitialisation" is designed for 10 bus model while the "preSpecified" is for 29 bus model.

- startTime_of_EnergyConsumption: specify the demand data start time for data querying. The demand data published from UK GOV is updated each year. The knowledge graph which represents those data is labeled with its start time, e.g. "2017-01-31".
