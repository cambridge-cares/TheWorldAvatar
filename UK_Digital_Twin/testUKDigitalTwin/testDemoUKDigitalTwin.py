##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 24 June 2021         #
##########################################

"""Test generating UK digital twin KG from sketch"""

import os, sys, json
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Power_Plant_Generator.powerPlantABoxGeneration import addUKPowerPlantTriples
from UK_Energy_Consumption_Generator.energyConsumptionABoxGeneration import addUKElectricityConsumptionTriples
from UK_Power_Grid_Topology_Generator.topologyABoxGeneration import createTopologyGraph, addEBusNodes, addELineNodes, addEGenNodes
from UK_Power_Grid_Model_Generator.model_EGenABoxGeneration import createModel_EGen
from UK_Power_Grid_Model_Generator.model_EBusABoxGeneration import createModel_EBus
from UK_Power_Grid_Model_Generator.model_ELineABoxGeneration import createModel_ELine
from Top_Node_Generator.topnodeABoxGeneration import generateTopNodeGraph
from UK_Digital_Twin_Package.BlazegraphLookupTableUpdater import updateLookUpTable
from UK_Digital_Twin_Package import UKPowerPlant as UKpp
from UK_Digital_Twin_Package import UKEnergyConsumption as UKec
from UK_Digital_Twin_Package import UKPowerGridTopology as UK_Topo

if __name__ == '__main__': 
    # """DUKES_Version = 2019"""
    # DUKES_Version = int(input('Specify DUKES Data Version: '))
    # filePath_PP = str(input('Specify power plant owl files path: '))
    # if filePath_PP == "None":
    #     filePath_PP = None
    # addUKPowerPlantTriples('default', DUKES_Version, filePath_PP, True)
    
    # """ElectricityConsumption_Version = 2017"""
    # ElectricityConsumption_Version = int(input('Specify Electricity Consumption Data Version: '))
    # filePath_ec = str(input('Specify Electricity Consumption owl files path: '))
    # if filePath_ec == "None":
    #     filePath_ec = None
    # addUKElectricityConsumptionTriples('default', ElectricityConsumption_Version, filePath_ec, True)
    
    # # # # When UK power plant and energy consumption KGs being generated and upload to the como server, the lookup table needs to be updated
    # # # UKPowerPlant_endpoint = UKpp.UKPowerPlant().endpoint
    # # # UKEnergyConsumption_endpoint = UKec.UKEnergyConsumption().endpoint
    
    # # # updateLookUpTable(UKPowerPlant_endpoint['endpoint_iri'], UKPowerPlant_endpoint['queryendpoint_iri'], UKPowerPlant_endpoint['updateendpoint_iri'], UKPowerPlant_endpoint['lable'], True)
    # # # updateLookUpTable(UKEnergyConsumption_endpoint['endpoint_iri'], UKEnergyConsumption_endpoint['queryendpoint_iri'], UKEnergyConsumption_endpoint['updateendpoint_iri'], UKEnergyConsumption_endpoint['lable'], True)
    
    # # # UKGridTopology_endpoint = UK_Topo.UKPowerGridTopology().endpoint
    # # # updateLookUpTable(UKGridTopology_endpoint['endpoint_iri'], UKGridTopology_endpoint['queryendpoint_iri'], UKGridTopology_endpoint['updateendpoint_iri'], UKGridTopology_endpoint['lable'], True)
    
    # """Creating the A-box of topology"""
    # filePath_topo = str(input('Specify topology owl files path: '))
    # if filePath_topo == "None":
    #     filePath_topo = None
    
    # createTopologyGraph('default', False, 10, 14, addEBusNodes, None, None, filePath_topo, True)
    # createTopologyGraph('default', False, 10, 14, None, addELineNodes, None, filePath_topo, True)
    # createTopologyGraph('default', False, 10, 14, None,  None, addEGenNodes, filePath_topo, True)
    
    """Creating the A-box of grid model"""
    filePath_model = str(input('Specify model owl files path: '))
    if filePath_model == "None":
        filePath_model = None
    
    # createModel_EGen('default', False, 2019, filePath_model, True)
    # createModel_EBus('default', False, 2019, filePath_model, True)   
    createModel_ELine('default', False, 10, 14, 2019, filePath_model, True) 
    
    """Creating the top node of UK digital twin"""
    filePath_topNode = str(input('Specify top node owl files path: '))
    if filePath_topNode == "None":
        filePath_topNode = None
    generateTopNodeGraph('default', False, filePath_topNode, True)
       
    print('terminated')