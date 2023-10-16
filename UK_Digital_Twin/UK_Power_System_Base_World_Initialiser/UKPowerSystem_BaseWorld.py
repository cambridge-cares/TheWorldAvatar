"""
This python module is created for initialising the base world entities of the power system.
The base world includes the power plant data, energy demand data, population data, and power grid topology entities.
"""

import os, sys, json
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package import UKDigitalTwin as UKDT
from UK_Power_Plant_Generator import powerPlantABoxGeneration
from UK_Energy_Consumption_Generator import energyConsumptionABoxGeneration
from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLabel as endPointList
from UK_Digital_Twin_Package.KGLocalStoragePath import defaultKGStoragePath
from UK_Power_Grid_Topology_Generator.topologyABoxGeneration import rootNodeAndNameSpace, createTopologyGraph
from UKPopulationDensity import UKPopulationDensityABoxCreator

with open('./UK_Power_System_Base_World_Initialiser/config.json', 'r') as config_file:
    config_data = json.load(config_file)

dt = UKDT.UKDigitalTwin()

class powerSystemBaseWorldInitialiser(object):
      
    def __init__(self, PopulationVersion:int, DUKESDataVersion:int, startTime_of_EnergyConsumption:str,         
        endPointURL:str, KGStoragePath:str, ifUpdateToTripleStore:bool, topologyConfigList:list,
        limitLines:int, pointDistance:int, endPointURL_POPULATION:str):

        ## folder location cheching
        folder = os.path.exists(KGStoragePath)
        if not folder:                
            os.makedirs(KGStoragePath)           
            print("---  new folder %s...  ---" % KGStoragePath)
        else:
            print("---  There has npy folder!  ---")
    
        ## 1. Specify data version
        self.PopulationVersion = PopulationVersion
        self.DUKESDataVersion = DUKESDataVersion
        self.startTime_of_EnergyConsumption = startTime_of_EnergyConsumption

        ## 2. Specify triple store and local file path
        self.KGStoragePath = KGStoragePath
        if ifUpdateToTripleStore in "True":
            self.ifUpdateToTripleStore = True
        else:
            self.ifUpdateToTripleStore = False
        self.endPointURL = endPointURL
        self.endPointURL_POPULATION = endPointURL_POPULATION

        ## 3. Specify the topology config
        self.topologyConfigList = topologyConfigList

        ## 4. Specify limits of the population data files
        self.limitLines = limitLines
        self.pointDistance = pointDistance
 
    def BaseWorldInitialiser(self):
        ## Create population density
        UKPopulationDensityABoxCreator.addUKPopulationDensityTriples(self.PopulationVersion, self.limitLines, self.pointDistance, self.endPointURL_POPULATION, self.KGStoragePath, self.ifUpdateToTripleStore)
        ## 1. Create the power plant instances     
        self.UKElectricitySystemIRI, self.GBElectricitySystemIRI, self.NIElectricitySystemIRI = powerPlantABoxGeneration.addUKPowerPlantTriples(self.DUKESDataVersion, self.endPointURL, self.KGStoragePath, self.ifUpdateToTripleStore)
        ## 2. Create the electricity consumption triple    
        energyConsumptionABoxGeneration.addUKElectricityConsumptionTriples(self.GBElectricitySystemIRI, self.startTime_of_EnergyConsumption,
                                                                            self.endPointURL, self.KGStoragePath, self.ifUpdateToTripleStore)
        ## 3. Create the topology    
        for topo in self.topologyConfigList:
            topoConfig = rootNodeAndNameSpace(topo['BusNum'], topo['BranchNum'], topo['slackBusIndex'], self.endPointURL, topo['ElectricitySystemName'])
            topo['TopologyIRI'], topo['slackBusNodeIRI'] = createTopologyGraph(topoConfig, topo['generatorClusterFunctionName'], topo['voltageLevel'], self.endPointURL, self.KGStoragePath, self.ifUpdateToTripleStore)

        return self.topologyConfigList
    
if __name__ == '__main__':
    DUKESDataVersion = config_data["DUKESDataVersion"]
    startTime_of_EnergyConsumption = config_data["startTime_of_EnergyConsumption"]
    endPointURL = endPointList.UKPowerSystemBaseWorld['endpoint_iri']
    endPointURL_POPULATION = endPointList.UKPopulationData['endpoint_iri']
    KGStoragePath = defaultKGStoragePath
    ifUpdateToTripleStore = config_data["ifUpdateToTripleStore"]
    topologyConfigList = config_data["topologyConfigList"] 
    PopulationVersion = config_data["PopulationVersion"] 
    limitLines = config_data["limitLines"]
    pointDistance = config_data["pointDistance"]

    testModelInitialisier = powerSystemBaseWorldInitialiser(PopulationVersion, DUKESDataVersion, startTime_of_EnergyConsumption,         
        endPointURL, KGStoragePath, ifUpdateToTripleStore, topologyConfigList, limitLines, pointDistance, endPointURL_POPULATION)
    testModelInitialisier.BaseWorldInitialiser()
    # print(testModelInitialisier.topologyConfigList)
    
