##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 10 June 2021         #
##########################################

"""The first time generating UK power plant and energy consumption KG"""

import os, sys, json
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Power_Plant_Generator.powerPlantABoxGeneration import addUKPowerPlantTriples
from UK_Energy_Consumption_Generator.energyConsumptionABoxGeneration import addUKElectricityConsumptionTriples
from UK_Power_Grid_Topology_Generator.topologyABoxGeneration import createTopologyGraph
from UK_Digital_Twin_Package.BlazegraphLookupTableUpdater import updateLookUpTable
from UK_Digital_Twin_Package import UKPowerPlant as UKpp
from UK_Digital_Twin_Package import UKEnergyConsumption as UKec

if __name__ == '__main__': 
    # # DUKES_Version = 2019
    # DUKES_Version = int(input('Specify DUKES Data Version: '))
    # addUKPowerPlantTriples('default', DUKES_Version, False)
    
    # # ElectricityConsumption_Version = 2017
    # ElectricityConsumption_Version = int(input('Specify Electricity Consumption Data Version: '))
    # addUKElectricityConsumptionTriples('default', ElectricityConsumption_Version, False)
    
    # When UK power plant and energy consumption KGs being generated and upload to the como server, the lookup table needs to be updated
    UKPowerPlant_endpoint = UKpp.UKPowerPlant().endpoint
    UKEnergyConsumption_endpoint = UKec.UKEnergyConsumption().endpoint
    
    updateLookUpTable(UKPowerPlant_endpoint['endpoint_iri'], UKPowerPlant_endpoint['queryendpoint_iri'], UKPowerPlant_endpoint['updateendpoint_iri'], UKPowerPlant_endpoint['lable'], True)
    updateLookUpTable(UKEnergyConsumption_endpoint['endpoint_iri'], UKEnergyConsumption_endpoint['queryendpoint_iri'], UKEnergyConsumption_endpoint['updateendpoint_iri'], UKEnergyConsumption_endpoint['lable'], True)
    
    # createTopologyGraph('sleepycat',False)
    
    print('terminated')