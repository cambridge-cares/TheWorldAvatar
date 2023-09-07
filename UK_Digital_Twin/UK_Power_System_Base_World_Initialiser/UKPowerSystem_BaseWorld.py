##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 06 Sept 2023         #
##########################################
"""
This python module is created for initialising the base world entities of the power system.
The base world includes the power plant data, energy demand data, population data, and power grid topology entities.
"""

import os, sys
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package import UKDigitalTwin as UKDT
from UK_Power_Plant_Generator import powerPlantABoxGeneration
from UK_Energy_Consumption_Generator import energyConsumptionABoxGeneration
from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLabel as endPointList
from UK_Digital_Twin_Package.KGLocalStoragePath import defaultKGStoragePath
from UK_Power_Grid_Topology_Generator.topologyABoxGeneration import rootNodeAndNameSpace, createTopologyGraph
from UKPopulationDensity import UKPopulationDensityABoxCreator

dt = UKDT.UKDigitalTwin()

class powerSystemBaseWorldInitialiser(object):
      
    def __init__(self, DUKESDataVersion:int, startTime_of_EnergyConsumption:str,         
        endPointURL:str, KGStoragePath:str, ifUpdateToTripleStore:bool, 
        topologyConfigList:list):
    
        ## 1. Specify data version
        self.DUKESDataVersion = DUKESDataVersion
        self.startTime_of_EnergyConsumption = startTime_of_EnergyConsumption

        ## 2. Specify triple store and local file path
        self.KGStoragePath = KGStoragePath
        self.ifUpdateToTripleStore = ifUpdateToTripleStore
        self.endPointURL = endPointURL

        ## 3. Specify the topology config
        self.topologyConfigList = topologyConfigList
 
    def BaseWorldInitialiser(self):
        ## Create population density
        ##TODO: add the population method from alineware
        ## UKPopulationDensityABoxCreator.addUKPopulationDensityTriples()
        ## 1. Create the power plant instances     
        self.UKElectricitySystemIRI, self.GBElectricitySystemIRI, self.NIElectricitySystemIRI = powerPlantABoxGeneration.addUKPowerPlantTriples(self.DUKESDataVersion, self.endPointURL, self.KGStoragePath,  self.ifUpdateToTripleStore)
        ## 2. Create the electricity consumption triple    
        energyConsumptionABoxGeneration.addUKElectricityConsumptionTriples(self.GBElectricitySystemIRI, self.startTime_of_EnergyConsumption,
                                                                            self.endPointURL, self.KGStoragePath, self.ifUpdateToTripleStore)
        ## 3. Create the topology    
        for topo in self.topologyConfigList:
            topoConfig = rootNodeAndNameSpace(topo['BusNum'], topo['BranchNum'], topo['slackBusIndex'], self.endPointURL, topo['ElectricitySystemName'])
            topo['TopologyIRI'], topo['slackBusNodeIRI'] = createTopologyGraph(topoConfig, topo['generatorClusterFunctionName'], topo['voltageLevel'], self.endPointURL, self.KGStoragePath, self.ifUpdateToTripleStore)

        return self.topologyConfigList
    
if __name__ == '__main__':
    DUKESDataVersion = "2019"
    startTime_of_EnergyConsumption = "2017"
    endPointURL = endPointList.UKPowerSystemBaseWorld['endpoint_iri']
    KGStoragePath = defaultKGStoragePath
    ifUpdateToTripleStore = True
    topologyConfigList = [{
        'BusNum': 10,
        'BranchNum': 14,
        'slackBusIndex': 1, 
        'ElectricitySystemName': 'Great_Britain',
        'generatorClusterFunctionName': 'sameRegionWithBus',
        'voltageLevel': ["275", "400"]
        }, 
        {
        'BusNum': 29,
        'BranchNum': 99,
        'slackBusIndex': 27, 
        'ElectricitySystemName': 'Great_Britain',
        'generatorClusterFunctionName': 'closestBus',
        'voltageLevel': []
        }]
    testModelInitialisier = powerSystemBaseWorldInitialiser(DUKESDataVersion, startTime_of_EnergyConsumption,         
        endPointURL, KGStoragePath, ifUpdateToTripleStore, topologyConfigList)
    testModelInitialisier.BaseWorldInitialiser()
    print(testModelInitialisier.topologyConfigList)
    
