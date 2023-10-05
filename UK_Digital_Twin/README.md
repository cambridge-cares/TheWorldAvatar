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
- 2.1 Check the data files: check the folder `\UK_Digital_Twin\Data files\DUKES`. There would be folders with the data pubulished by UK.GOV in different years, e.g. 2019. Go to the folder called `2019`. The .csv files there are the processed files based on the DUKES raw data file `\UK_Digital_Twin\Data file\DUKES\2019\DUKES2019.xls` that was downloaded from `https://www.gov.uk/government/statistics/electricity-chapter-5-digest-of-united-kingdom-energy-statistics-dukes`. In that page there are many files and the excel file named 'Power stations in the United Kingdom (DUKES 5.11)' was downloaded. Columns relevant to the study are copied from the excel sheet (tab) named `5.11 Full list` within this excel file and pasted in producing the CSV files, e.g., plantname.csv, planttype.csv, etc. 

   The content of `builtyear.csv` is from column "Year of commission";

   The content of `designcapacity.csv` is from column "Installed Capacity (MW)";

   The content of `energyGen.csv` is modified from column "Type";

   The content of `genTech.csv` is modified from column "Type";

   The content of `gpslocation.csv` is collected by hands;

   The content of `owner.csv` is from column "Company Name";

   The content of `plantname.csv` is from column "Station Name";

   The content of `planttype.csv` is modified from column "Type";

   The content of `primaryFuel.csv` is modified from column "Fuel";

   The first column from `propertydata.csv` is installed capacity, the second column is commissioned year and the third and fourth columns are lat-lon locations.

   The content of `regionaladdress.csv` is modified from column "Location: Scotland, Wales, Northern Ireland or English region";

Each CSV file has exactly the same number of rows. Among the .csv files, only the 'gpslocation.csv' is manully created by collecting coordinates from the Google maps which contain all the lat-lon locations of the power plants listed in DUKES. After 2022, the original DUKES data started reporting the GPS locations but they are not accurate enough. Therefore, till 2023 the location of the power plant is still based on the manually collected one.
- 2.2 Create namespaces for building the base world knowledge graph: By default, Blazegraph is the triple store used for managing the KG. The creation of namespaces in the Blazegraph endpoint was done manually in this work. The recommended names of the namespaces are `ONS_subset`, `UKPopulationData` and `UKPowerSystemBaseWorld`. All these namespaces should be created with the option `Enable geospatial` ticked. Create `ONS_subset` to manage the subset of ONS data which was available from `https://statistics.data.gov.uk/sparql`. Create `UKPopulationData` and `UKPowerSystemBaseWorld` to store the triples describing the base world of the UK power system. The namespace `UKPopulationData` is specifically designed to contain the population data while `UKPowerSystemBaseWorld` is for the other data. After the creation of these three namespaces, set up the endpoint IRIs in `\UK_Digital_Twin\UK_Digital_Twin_Package\endPoint.py`.
- 2.3 Create a replica of the required subset of ONS data: Copy all the files except the batch (.bat) files from Dropbox by following the instructions below:

- Copy all the files from `Dropbox (Cambridge University)\CoMo shared\wx243\c4e-wx243-TWAPowerSystem\Codes\resources\ONSData\ONSKG_AreaBoundariesAndLACode` to `\UK_Digital_Twin\resources\ONSData\ONSKG_AreaBoundariesAndLACode`

- Copy all the files from `Dropbox (Cambridge University)\CoMo shared\wx243\c4e-wx243-TWAPowerSystem\Codes\resources\ONSData\ONSKG_WithinRelations` to `\UK_Digital_Twin\resources\ONSData\ONSKG_WithinRelations`

- Copy all the files from `Dropbox (Cambridge University)\CoMo shared\wx243\c4e-wx243-TWAPowerSystem\Codes\resources\ONSData\ONSKG_AreaCode` to `\UK_Digital_Twin\resources\ONSData\ONSKG_AreaCode`

- Copy all the files from `Dropbox (Cambridge University)\CoMo shared\wx243\c4e-wx243-TWAPowerSystem\Codes\resources\ONSData\ONSObsolete` to `\UK_Digital_Twin\resources\ONSData\ONSObsolete`

- Copy all the files from `Dropbox (Cambridge University)\CoMo shared\wx243\c4e-wx243-TWAPowerSystem\Codes\resources\ONSData\WithinGB` to `\UK_Digital_Twin\resources\ONSData\WithinGB`

Now, simply run `ons_data_uploader.py` from the path of `\UK_Digital_Twin\UK_Power_System_Base_World_Initialiser\UploadONS\` using the following command, 

   `python ons_data_uploader.py`

if you are at the UK_Digital_Twin folder path, simply run this command instead:

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
    'endpoint_iri' : ONSEndpoint_iri,
    'queryendpoint_iri' : ONSEndpoint_iri,
    'updateendpoint_iri' : ONSEndpoint_iri}


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

Step 3 (OPTIONAL): How to conduct the scenario analysis of SMR replacement with optimal power flow (OPF) analysis?
- 3.1 Pre-requirement: If you want to post-process the result produced by SMR replacement, copy all the files from the path of `Dropbox (Cambridge University)\CoMo shared\wx243\c4e-wx243-TWAPowerSystem\Codes\UK_Digital_Twin\resources\RegionalBreakdown_images` to the `UK_Digital_Twin\resources\required_images` folder.

- 3.2 Conduct SMR Replacement: For reproducing the results reported in Preprint 312, you need to run `SMRReplacemenWithOptimalFlowAnalysis.py`.

First simulation: Go to `\UK_Digital_Twin\UK_Power_System_SMR_Replacement\SMR_Replacement\` and run `SMRReplacemenWithOptimalFlowAnalysis.py` using the following command.

   `python UK_Power_System_SMR_Replacement/SMR_Replacement/SMRReplacemenWithOptimalFlowAnalysis.py`

Once the module starts, the terminal will firstly show the following message asking to provide an input.

`Please specify the number of bus and number of branch (e.g. 1014 for 10-bus and 14-branch, 2999 for 29-bus and 99-branch): ` 

This is to determine which config file to use to initialise the SMR replacement simulation. There are two config files avaliable in this project which are accordingly with the number of topologies. You can enter either `1014` to use the 10-bus model or `2999` to use the 29-bus model. For reproducing  the results reported in Preprint 312, use `2999`.

After setting up the model type, the terminal will then show up the following message to specify the LCOE of the SMR.

`Please specify the LCOE of SMR (£/MWh): `

The range could be any number. In this study, due to the prototype picked for the SMR, 40 ~ 60 £/MWh is insterested.

For reproducing  the results reported in Preprint 312, use `40`.

Following this step, the terminal will ask for an input `Please specify if you want to generate the geojson file for visulisation (1 for Yes, 2 for No): ` to configurate the generation of GeoJSON files. 

For reproducing  the results reported in Preprint 312, use `1`.

Then, you will see the following message asking for an input: `Please specify if you want to use the existing results for post-processing (1 for Yes, 2 for No): ` To specify if this run is for simulation or results post-processing. If this is the first time execution for calculating the result then you can enter `2`. Otherwise, entering `1` is for post processing the existed result files to create charts or diagrams. While choosing the post processing mode, please make sure that that results files are avaiable from the path `/UK_Digital_Twin/outputs/smr_replacements/` and the folder name is corresponding to the configs which are set up in the previous steps.

For reproducing  the results reported in Preprint 312, use `2`.

The next step lets you decide whether to create PDF files for Pareto Front `Please specify if you want to create PDF files for Pareto Front (1 for Yes, 2 for No): `. Enter `1` for creating PDFs and `2` for not. It is recommended to select `1` for simulation and for post processing, it can be any value which will not affect the proccess.

For reproducing  the results reported in Preprint 312, use `1`.

The last step is to confirm the number of the round by providing an input when you see the following message: `Please confirm the round of this run, ranging from 1 to 6: `

This is done to overcome the `Out of Memory` issue. You have to run this code 6 times. For the first time provide `1`, for the second time provide `2`, and so on. 

Note: keep patience because the code might take as long as 8 hours for each round on a machine with 32 GBs of memory.

First post-processing: You will post-process the results generated in the first simulation. Go to `\UK_Digital_Twin\UK_Power_System_SMR_Replacement\SMR_Replacement\` and run `SMRReplacemenWithOptimalFlowAnalysis.py` using the following command.

   `python UK_Power_System_SMR_Replacement/SMR_Replacement/SMRReplacemenWithOptimalFlowAnalysis.py`

Once the module starts, the terminal will firstly show the following sentence

`Please specify the number of bus and number of branch (e.g. 1014 for 10-bus and 14-branch, 2999 for 29-bus and 99-branch): ` 

for determing which config file is used to initialise the SMR replacement simulation. There are two config files avaliable in this project with are accordingly with the number of topologies. You can enter either `1014` to use the 10-bus model or `2999` to use the 29-bus model. For reproducing  the results reported in Preprint 312, use `2999`.

After setting up the model type, the terminal will then show up the following question to specify the LCOE of the SMR

`Please specify the LCOE of SMR (£/MWh): `

The range could be any number. In this study, due to the prototype picked for the SMR, 40 ~ 60 £/MWh is insterested.

For reproducing  the results reported in Preprint 312, use `40`.

Following this step, the terminal will ask for the question `Please specify if generate the geojson file for visulisation (1 for Yes, 2 for No): ` to configurate the generation of GeoJSON files. For entering `1` is to generate the GeoJSON file for visulisation purpose and it is recommended. 

For reproducing  the results reported in Preprint 312, use `1`.

Then, you will see from the terminal asks that `Please specify if use the existing results for post-processing (1 for Yes, 2 for No): ` to specify if this running is for simulation or results post-processing. If this is the first time execution for calculating the result then you can enter `2`. Otherwise, enter `1` for post-processing the existing result files to create charts or diagrams. While choosing the post processing mode, please make sure that that results files are avaiable from the path `/UK_Digital_Twin/outputs/smr_replacements/` and the folder name is corresponding to the configs which are set up in the previous steps.

For reproducing  the results reported in Preprint 312, use `1`.

At the last step you need to decide whether to create the PDF files for Pareto Front `Please specify if creates the PDF files for Pareto Front (1 for Yes, 2 for No): `. Enters `1` for creating PDFs and `2` for not. It is recommended to select `1` for simulation and for post processing, it can be any value which will not affect the proccess.

For reproducing  the results reported in Preprint 312, use `1`.

Once the code execution is finished, you will find figures reported in Preprint 312. Figures 2-6 may be found in the following path in PDF format:
`UK_Digital_Twin\outputs\smr_replacement_fig`.

Note: this is the post-processing step and is much faster than the first run.

Second simulation: Go to `\UK_Digital_Twin\UK_Power_System_SMR_Replacement\SMR_Replacement\` and run `SMRReplacemenWithOptimalFlowAnalysis.py` using the following command.

   `python UK_Power_System_SMR_Replacement/SMR_Replacement/SMRReplacemenWithOptimalFlowAnalysis.py`

Once the module starts, the terminal will firstly show the following message asking to provide an input.

`Please specify the number of bus and number of branch (e.g. 1014 for 10-bus and 14-branch, 2999 for 29-bus and 99-branch): ` 

This is to determine which config file to use to initialise the SMR replacement simulation. There are two config files avaliable in this project with are accordingly with the number of topologies. You can enter either `1014` to use the 10-bus model or `2999` to use the 29-bus model. For reproducing  the results reported in Preprint 312, use `2999`.

After setting up the model type, the terminal will then show up the following message to specify the LCOE of the SMR.

`Please specify the LCOE of SMR (£/MWh): `

The range could be any number. In this study, due to the prototype picked for the SMR, 40 ~ 60 £/MWh is insterested.

For reproducing  the results reported in Preprint 312, use `60`.

Following this step, the terminal will ask for an input `Please specify if generate the geojson file for visulisation (1 for Yes, 2 for No): ` to configurate the generation of GeoJSON files. 

For reproducing  the results reported in Preprint 312, use `1`.

Then, you will see the following message asking for an input: `Please specify if use the existing results for post-processing (1 for Yes, 2 for No): ` To specify if this run is for simulation or results post-processing. If this is the first time execution for calculating the result then you can enter `2`. Otherwise, entering `1` is for post-processing the existing result files to create charts or diagrams. While choosing the post processing mode, please make sure that that results files are avaiable from the path `/UK_Digital_Twin/outputs/smr_replacements/` and the folder name is corresponding to the configs which are set up in the previous steps.

For reproducing  the results reported in Preprint 312, use `2`.

The next step you need to decide whether to create the PDF files for Pareto Front `Please specify if creates the PDF files for Pareto Front (1 for Yes, 2 for No): `. Enters `1` for creating PDFs and `2` for not. It is recommended to select `1` for simulation and for post processing, it can be any value which will not affect the proccess.

For reproducing  the results reported in Preprint 312, use `1`.

The last step is to confirm the number of the round by providing an input when you see the following message: `Please confirm the round of this run, ranging from 1 to 6: `

This is done to overcome the `Out of Memory` issue. You have to run this code 6 times. For the first time provide `1`, for the second time provide `2`, and so on. 

Note: keep patience because the code might take as long as 8 hours for each round on a machine with 32 GBs of memory.

Second post-processing: You will post-process the results generated in the second simulation. Go to `\UK_Digital_Twin\UK_Power_System_SMR_Replacement\SMR_Replacement\` and run `SMRReplacemenWithOptimalFlowAnalysis.py` using the following command.

   `python UK_Power_System_SMR_Replacement/SMR_Replacement/SMRReplacemenWithOptimalFlowAnalysis.py`

Once the module starts, the terminal will firstly show the following sentence

`Please specify the number of bus and number of branch (e.g. 1014 for 10-bus and 14-branch, 2999 for 29-bus and 99-branch): ` 

for determing which config file is used to initialise the SMR replacement simulation. There are two config files avaliable in this project with are accordingly with the number of topologies. You can enter either `1014` to use the 10-bus model or `2999` to use the 29-bus model. For reproducing  the results reported in Preprint 312, use `2999`.

After setting up the model type, the terminal will then show up the following question to specify the LCOE of the SMR

`Please specify the LCOE of SMR (£/MWh): `

The range could be any number. In this study, due to the prototype picked for the SMR, 40 ~ 60 £/MWh is insterested.

For reproducing  the results reported in Preprint 312, use `60`.

Following this step, the terminal will ask for the question `Please specify if generate the geojson file for visulisation (1 for Yes, 2 for No): ` to configurate the generation of GeoJSON files. For entering `1` is to generate the GeoJSON file for visulisation purpose and it is recommended. 

For reproducing  the results reported in Preprint 312, use `1`.

Then, you will see from the terminal asks that `Please specify if use the existing results for post-processing (1 for Yes, 2 for No): ` to specify if this running is for simulation or results post-processing. If this is the first time execution for calculating the result then you can enter `2`. Otherwise, entering `1` is for post processing the existed result files to create charts or diagrams. While choosing the post processing mode, please make sure that that results files are avaiable from the path `/UK_Digital_Twin/outputs/smr_replacements/` and the folder name is corresponding to the configs which are set up in the previous steps.

For reproducing  the results reported in Preprint 312, use `1`.

At the last step you need to decide whether to create the PDF files for Pareto Front `Please specify if creates the PDF files for Pareto Front (1 for Yes, 2 for No): `. Enters `1` for creating PDFs and `2` for not. It is recommended to select `1` for simulation and for post processing, it can be any value which will not affect the proccess.

For reproducing  the results reported in Preprint 312, use `1`.

Once the code execution is finished, you will find figures reported in Preprint 312. Figures 2-6 may be found in the following path in PDF format:
`UK_Digital_Twin\outputs\smr_replacement_fig`.

In this folder, you will find the figure 2,3 and 4 displayed in prepint 312.

Step 4 (OPTIONAL): How to visulise the SMR adoption on the UK regional power output map as the carbon tax increasing?

In this step, we are going to create one of the 16 subfigures of figure 5 reported in preprint 312 based on 29-bus model. The following instructions detail how to create a subfigure corresponding to different conditions.

- 4.1 Check the required materials and folder path structure: to conduct Step 4, please make sure that the step 3 is done as this step is required for this post processing step.

Please check if the required files exist by going to `\UK_Digital_Twin\resources\HTML_Files\SMR_FossilPlant_output\` and make sure the following files are within this folder.

   `SMR_EMPTY.geojson`
   `index.html`

- 4.1.1 Copy files into the designated path and folder.

Taking the 29-bus model and the £60/MWh LCOE as an example, go to `\UK_Digital_Twin\outputs\MAPBOX_files\SMR_FossilPlant_output\29bus_LCOE_60£\weather_WBSB\`.

In this folder you will find that there are three sub-folders named as

   `weight0.25`,
   `weight0.5`,
   `weight0.75`.

Open each sub-folder, there are four sub-folders included which are listed as 

   `carbonTax0`,
   `carbonTax40`,
   `carbonTax50`,
   `carbonTax150`.

The name of the folder referring to the condition of creating the visulisation of the each subfigure in figure 5 of prepint 312.

If we use the first subfigure as the demonstration example, which is generated under the condition as the weather condition is WBSB, weight 0.25, carbon tax 0 and 0 SMR . It is worthy to mention that the number of the SMR denoted in each subfigure in preprint 312 is the optimised result. In this example, 0 SMR means that when carbon tax set to be 0, the most economic solution is with no SMR adoption.

To create this figure, four files are needed to be copied into the scenario folder `\UK_Digital_Twin\outputs\MAPBOX_files\SMR_FossilPlant_output\29bus_LCOE_60£\weather_WBSB\weight_0.25\carbonTax_0\`.

If the subfigure you are going to generate corresponds to a scenario with 0 SMR, follow step 4.2. If the subfigure you are going to generate corresponds to a scenario with SMR>0, follow step 4.3.

- 4.2 How to generate a subfigure of figure 5 with 0 SMR.
- 4.2.1 The first file is the GeoJSON for the operational fossil fired plants including the power plants fueled by coal, oil and natural gas. Go to the folder `\UK_Digital_Twin\outputs\smr_replacement_fig\fossilFuelPowerPlantGEOJSON\<YYYYMMDD-hhmm>\` and find the file `fossilFuelPowerPlantGeoJSON_(SMR_0_CarbonTax_0_weatherCondition_WBSB_weight_0.25).geojson` then copy it to the scenario folder `\UK_Digital_Twin\outputs\MAPBOX_files\SMR_FossilPlant_output\29bus_LCOE_60£\weather_WBSB\weight_0.25\carbonTax_0\`.

- 4.2.2 The next is to copy the file `TotalOutputForRegionalArea_(SMR_0_CarbonTax_0_weatherCondition_WBSB_weight_0.25).geojson` from the folder `\UK_Digital_Twin\outputs\smr_replacement_fig\regionalOutputJSONFiles\<YYYYMMDD-hhmm>\29\` to the scenario folder `\UK_Digital_Twin\outputs\MAPBOX_files\SMR_FossilPlant_output\29bus_LCOE_60£\weather_WBSB\weight_0.25\carbonTax_0\`.

- 4.2.3 Then the following step is to copy the GeoJSON file which demonstrates the SMR deployment, however for this example, there is no SMR to be introduced. Therefore, in order to aviod causing any error, please copy the empty GeoJSON file from the path `\UK_Digital_Twin\resources\HTML_Files\SMR_FossilPlant_output\SMR_EMPTY.geojson` to the scenario folder `\UK_Digital_Twin\outputs\MAPBOX_files\SMR_FossilPlant_output\29bus_LCOE_60£\weather_WBSB\weight_0.25\carbonTax_0\`.

- 4.2.4 The final step is to copy the `index.html` template from `\UK_Digital_Twin\resources\HTML_Files\SMR_FossilPlant_output` into the scenario folder `\UK_Digital_Twin\outputs\MAPBOX_files\SMR_FossilPlant_output\29bus_LCOE_60£\weather_WBSB\weight_0.25\carbonTax_0\`. In the `index.html`, please do the following modification:

- 4.2.4.1 Replace `<SMR_GEOJSON_FILE_NAME>` with `SMR_EMPTY.geojson`
- 4.2.4.2 Replace `<FOSSIL_POWER_PLANT_GEOJSON_FILE_NAME>` with `fossilFuelPowerPlantGeoJSON_(SMR_0_CarbonTax_0_weatherCondition_WBSB_weight_0.25).geojson`
- 4.2.4.3 Replace `<OUTPUT_POWER_PLANT_GEOJSON_FILE_NAME>` with `TotalOutputForRegionalArea_(SMR_0_CarbonTax_0_weatherCondition_WBSB_weight_0.25).geojson`

- 4.2.4.4 Start the http.server for the visulisation in Google mapbox.

Run the following command in the terminal
    `cd outputs/MAPBOX_files/SMR_FossilPlant_output/29bus_LCOE_60£/weather_WBSB/weight_0.25/carbonTax_0`

Then start up the http server at the port 8000 by running the command in the terminal
    `python -m http.server 8000`

When the server is initialised, please go to `http://localhost:8000/` to check the result. If you want to terminite the server, simply press `Ctrl + C`.

- 4.3 How to generate a subfigure of figure 5 with SMR>0:

The figure generated in 4.2 had 0 SMR introduced, but the following example generates the third figure in the first row of figure 5 to demonstrate how to create the visulisation for the scenario with SMR adoption.

The subfigure displayed in figure 5 (row 1 column 3) is created under the weather condition WBSB, carbon tax £55/t, weight 0.25 and the optimised number of SMR is 28 (as labeled on the top-left conner of each subfigure in figure 5). 

To create this figure, four files are needed to be copied into the scenario folder `\UK_Digital_Twin\outputs\MAPBOX_files\SMR_FossilPlant_output\29bus_LCOE_60£\weather_WBSB\weight_0.25\carbonTax_55\`.

- 4.3.1 The first file is the GeoJSON for the operational fossil fired plants including the power plants fueled by coal, oil and natural gas. Go to the folder `\UK_Digital_Twin\outputs\smr_replacement_fig\fossilFuelPowerPlantGEOJSON\<YYYYMMDD-hhmm>\` and find the file `fossilFuelPowerPlantGeoJSON_(SMR28_CarbonTax_55_weatherCondition_WBSB_weight_0.25).geojson` then copy it to the scenario folder `\UK_Digital_Twin\outputs\MAPBOX_files\SMR_FossilPlant_output\29bus_LCOE_60£\weather_WBSB\weight_0.25\carbonTax_55\`.

- 4.3.2 The next step is to copy the file `TotalOutputForRegionalArea_(SMR_28_CarbonTax_55_weatherCondition_WBSB_weight_0.25).geojson` from the folder `\UK_Digital_Twin\outputs\smr_replacement_fig\regionalOutputJSONFiles\<YYYYMMDD-hhmm>\29\` to the scenario folder `\UK_Digital_Twin\outputs\MAPBOX_files\SMR_FossilPlant_output\29bus_LCOE_60£\weather_WBSB\weight_0.25\carbonTax_55\`.

- 4.3.3 Then the following step is to copy the GeoJSON file which demonstrates the SMR deployment, please copy the GeoJSON file from the path `\UK_Digital_Twin\outputs\smr_replacement_fig\GeneratorJSONFiles_29bus60LCOE\28_SMRs_55_CarbonTax\29BusModel_28_SMRs_Introduced_CarbonTax55_WeatherCondition_WBSB_weighter_0.25_SMR.geojson` to the scenario folder `\UK_Digital_Twin\outputs\MAPBOX_files\SMR_FossilPlant_output\29bus_LCOE_60£\weather_WBSB\weight_0.25\carbonTax_55\`.

- 4.3.4 The final step is to copy the `index.html` template from `\UK_Digital_Twin\resources\HTML_Files\SMR_FossilPlant_output` into the scenario folder `\UK_Digital_Twin\outputs\MAPBOX_files\SMR_FossilPlant_output\29bus_LCOE_60£\weather_WBSB\weight_0.25\carbonTax_55\`. In the `index.html`, please do the following modification:

- 4.3.4.1 Replace `<SMR_GEOJSON_FILE_NAME>` with `29BusModel_28_SMRs_Introduced_CarbonTax55_WeatherCondition_WBSB_weighter_0.25_SMR.geojson`
- 4.3.4.2 Replace `<FOSSIL_POWER_PLANT_GEOJSON_FILE_NAME>` with `fossilFuelPowerPlantGeoJSON_(SMR_0_CarbonTax_0_weatherCondition_WBSB_weight_0.25).geojson`
- 4.3.4.3 Replace `<OUTPUT_POWER_PLANT_GEOJSON_FILE_NAME>` with `TotalOutputForRegionalArea_(SMR_0_CarbonTax_0_weatherCondition_WBSB_weight_0.25).geojson`

- 4.3.4.4 Start the http.server for the visulisation in Google mapbox.

Run the following command in the terminal
    `cd outputs/MAPBOX_files/SMR_FossilPlant_output/29bus_LCOE_60£/weather_WBSB/weight_0.25/carbonTax_55`

Then start up the http server at the port 8000 by running the command in the terminal
    `python -m http.server 8000`

When the server is initialised, please go to `http://localhost:8000/` to check the result. If you want to terminite the server, simply press `Ctrl + C`.

It should be noticed that due to the random nature of genetic algorithm, the results may look slightly different from the one reported in preprint 312 as by nature genetic algorithms cannot generate the exactly same Pareto Front every time.

By following the above two examples, you can now generate the rest of the subfigures of figure 5 by choosing the corresponding files based on weight (e.g., 0.25, 0.5 and 0.75), number of SMRs (0, 19, 28, 33, 32) and carbon tax (e.g., 0, 40, 50, 55, 100 and 150)

Step 5 (OPTIONAL): How to visulise the branch loss and regional net demand?

- 5.1 Reproduce the first subfigure of figure 6 in preprint 312: This figure is created based on the weather condition WBSB, weight 0.25 and 33 SMRs. You are going to visualise the subfigure in an online geojson viewer. Please go to this geojson viewer website: `https://geojson.io/#map=2/0/20`. Note: set the mode of the map to be light if it is not set already. Then go to the folder `\UK_Digital_Twin\outputs\smr_replacement_fig\netDemandingGeoJSONFiles\<YYYYMMDD-hhmm>\RegionalAreaNetDemanding`, open the file `RegionalAreaNetDemanding_(SMR_33_CarbonTax_150_weatherCondition_WBSB_weight_0.25).geojson` and copy the content to the `</>JSON` tab.

- 5.2 In this step, you will be appending additional features to the GeoJSON content of the visulaisation in `https://geojson.io/#map=2/0/20` in Item 5.1 to show the overalyed lines representing grid branches. Now open the file `\UK_Digital_Twin\outputs\smr_replacement_fig\branchLossGeoJSONFiles\<YYYYMMDD-hhmm>\29\BranchGrid_(SMR_33_CarbonTax_150_weatherCondition_WBSB_weight_0.25).geojson` and copy line 5 to line 1588 from this geojson file to the `</>JSON` tab after the last feature in `https://geojson.io/#map=2/0/20`. Add `,` after the last feature and paste the lines that you already have copied. The feature has this structure:

{
	"type": "Feature",
	"properties": {
	...
	},
	"geometry":  ...             
}

After adding a `,`, it will look like the following:

{
	"type": "Feature",
	"properties": {
	...
	},
	"geometry":  ...             
},

Paste the copied lines after the last comma.

It should be noticed that due to the random nature of genertic algorithm, the results may look slightly different from the one reported in preprint 312 as genetic algorithms cannot generate the exactly same Pareto Front every time.

By following the above example, you now can generate the second and third subfigures of figure 6 by choosing the corresponding files with weight 0.50 and 0.75, respectively.


Step 5 (optional): use the patched PYPOWER for Power Flow (PF) analysis

If you're going to perform PF analysis and meet any problem about the convegence. Please install the PYPOWER with the patch as shown in following step. If you already installed PYPOWER, please uninstall it before running any commands below. 

Run the following git command from `UK_Digital_Twin\UK_Digital_Twin_Package` to clone the PYPOWER repository.

`git clone https://github.com/rwl/PYPOWER.git`

Run the following command from `UK_Digital_Twin\UK_Digital_Twin_Package\PYPOWER` to apply patches.

`git apply < ../svd_calculation.patch`

Note: for SMR repleacement study, there is NO need to install this patched version as the model used in this study is OPF not PF.