##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 13 April 2022        #
##########################################

"""This moudel is desogned to creat the UK base world inluding the entities of power plant the the enenrgy comsumption."""

import os, sys
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Power_Plant_Generator.powerPlantABoxGeneration import addUKPowerPlantTriples
from UK_Energy_Consumption_Generator.energyConsumptionABoxGeneration import addUKElectricityConsumptionTriples
from UK_Power_Grid_Topology_Generator.topologyABoxGeneration import rootNodeAndNameSpace, createTopologyGraph

class UKBaseWorldABoxGenerator():
    
    def __init__(self, DUKES_Version:int, ConsumptionData_Version:int, powerPlantOWLFileLocalPath = None, updateLocalPowerPlantOWLFileFlag:bool = True, \
                 energyConsumptionOWLFileLocalPath = None, updateLocalEnergyConsumptionOWLFileFlag:bool = True):
        
        """Configration of the power plant entity generation"""     
        self.DUKES_Version:int = DUKES_Version
        self.powerPlantOWLFileLocalPath = powerPlantOWLFileLocalPath
        self.updateLocalPowerPlantOWLFileFlag = updateLocalPowerPlantOWLFileFlag
    
        """Configration of the energy consumption entity generation"""     
        self.ConsumptionData_Version:int = ConsumptionData_Version
        self.energyConsumptionOWLFileLocalPath = energyConsumptionOWLFileLocalPath
        self.updateLocalEnergyConsumptionOWLFileFlag = updateLocalEnergyConsumptionOWLFileFlag
         
    def UKBaseWorldABoxGenerator(self):
        
        self.UKElectricitySystemIRI, self.GBElectricitySystemIRI, self.NIElectricitySystemIRI = \
                                                addUKPowerPlantTriples(self.DUKES_Version, self.powerPlantOWLFileLocalPath, self.updateLocalPowerPlantOWLFileFlag)
        
        addUKElectricityConsumptionTriples(self.GBElectricitySystemIRI, self.ConsumptionData_Version, self.energyConsumptionOWLFileLocalPath, self.updateLocalEnergyConsumptionOWLFileFlag)
        return
    

class UKTopologyCreator():
    
    def __init__(self, numOfBus:int, numOfBranch:int, tripleStoreLabel:str, ElectricitySystemName:str, generatorClusterFunctionName:str, voltageLevel:list, \
                 ontologyOWLFileLocalPath = None, updateBusAndBranchOntologyOWLFileFlag:bool = True):
        self.numOfBus = numOfBus
        self.numOfBranch = numOfBranch
        # self.electricitySystemIRI = electricitySystemIRI  
        self.ontologyOWLFileLocalPath = ontologyOWLFileLocalPath
        self.updateBusAndBranchOntologyOWLFileFlag = updateBusAndBranchOntologyOWLFileFlag
        self.tripleStoreLabel = tripleStoreLabel
        self.ElectricitySystemName = ElectricitySystemName
        self.generatorClusterFunctionName = generatorClusterFunctionName
        self.voltageLevel = voltageLevel
    
    
    
    def createTopology(self):
        self.topoConfig = rootNodeAndNameSpace(self.numOfBus, self.numOfBranch, self.tripleStoreLabel, self.ElectricitySystemName)
        createTopologyGraph(self.topoConfig, self.generatorClusterFunctionName, self.voltageLevel, self.ontologyOWLFileLocalPath, self.updateBusAndBranchOntologyOWLFileFlag)
        return

    

if __name__ == '__main__': 
    # test_UKBaseWorldABoxGenerator = UKBaseWorldABoxGenerator(2019, 2017)
    # test_UKBaseWorldABoxGenerator.UKBaseWorldABoxGenerator()
    # test_UKTopologyCreator_1 = UKTopologyCreator(10, 14, "ukdigitaltwin_test2", "Great_Britain", "sameRegionWithBus", ["275", "400"])
    # test_UKTopologyCreator_1.createTopology()
    
    test_UKTopologyCreator_2 = UKTopologyCreator(29, 99, "ukdigitaltwin_test2", "Great_Britain", "closestBus", [])
    test_UKTopologyCreator_2.createTopology()