# README for OptimalPowerFlowAnalysis.py
## Discription
'/TheWorldAvatar/UK_Digital_Twin/UK_Power_System_SMR_Replacement/OptimalPowerFlowAnalysis.py' is the main script for conducting the SMR replacement with Optimal Power Flow (OPF) analysis. This script also can product OPF without SMR replacement. 
The main scrip also include the results post processing methods.

The follow are the steps for setting up the environment of IDE and run the main script.

Step 1: preparation of the environment 
- Install WSL and Ubuntu.
- Create a working branch in WSL (recommanded VS code integrated IDE).
- Install conda within WSL.
- Install JAVA JDK `sudo apt-get install -y openjdk-11-jdk-headless`.
- Install the package from 'Dropbox (Cambridge University)\CoMo shared\wx243\SMR_Replacement\Codes\requirements.txt'.

Step 2: copy the required files (post processing)
This step can be skipped if the post processing is not needed.
Copy the folder from the path: 'Dropbox (Cambridge University)\CoMo shared\wx243\SMR_Replacement\Codes\requiredFiles' to any local folder, e.g. "d:/xxx/requiredFiles/". 

Step 3: create object of Class 
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
