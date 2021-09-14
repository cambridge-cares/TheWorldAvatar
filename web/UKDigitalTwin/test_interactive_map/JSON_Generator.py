##############################################
# Author: Wanni Xie (wx243@cam.ac.uk)        #
# Last Update Date: 14 Sept 2021             #
##############################################

from GPSLocationChecker import check_GPS_char, GPS_special_chars, check_GPS
from queryStrings import queryPowerPlantForVisualisation, queryUKElectricityConsumptionAndAssociatedGEOInfo, queryGridModeltForVisualisation_Bus, queryGridModeltForVisualisation_Branch
from geoJSONCreator import powerPlantgeoJSONCreator, elecConsAndGEOInfogeoJSONCreator, busModelJSONCreator, BranchAndBusConnectionGPSLocationJSONCreator

"""Endpoints"""
ONS = "http://statistics.data.gov.uk/sparql.json"
ukdigitaltwin_label = "ukdigitaltwin"
ukdigitaltwinendpoint = "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ukdigitaltwin/sparql"


# """PowerPlant query"""
# ret_pow = queryPowerPlantForVisualisation(ukdigitaltwin_label)
# ret_pow = check_GPS(ret_pow)

# """Electricity consumption query"""
# ret_elec_region = queryUKElectricityConsumptionAndAssociatedGEOInfo(ukdigitaltwinendpoint, ONS, True) # query the region consumption
# ret_elec_area = queryUKElectricityConsumptionAndAssociatedGEOInfo(ukdigitaltwinendpoint, ONS, False) # query the sub areas consumption

# """Grid model query"""
# ret_grid_model_bus = queryGridModeltForVisualisation_Bus(ukdigitaltwin_label)
ret_grid_model_branch = queryGridModeltForVisualisation_Branch(ukdigitaltwin_label)

"""Labels"""
class_label_pp = 'UK_PowerPlants'
class_label_elec_region = "UK_Electricity_Consumption_Regions"
class_label_elec_area = "UK_Electricity_Consumption_Areas"
class_label_busPara = "UK_Grid_Model_Bus_Parameter"
class_label_busInputVar = "UK_Grid_Model_Bus_InputVar"
class_label_branch = "UK_Grid_Model_Branch"
class_label_FromBus = "UK_Grid_Model_Branch_FromBus_GPSPoints"

"""Create GEOJSON files"""
# powerPlantgeoJSONCreator(ret_pow, class_label_pp)
# elecConsAndGEOInfogeoJSONCreator(ret_elec_region, class_label_elec_region)
# elecConsAndGEOInfogeoJSONCreator(ret_elec_area, class_label_elec_area)
# busModelJSONCreator(ret_grid_model_bus, class_label_busPara, class_label_busInputVar)
BranchAndBusConnectionGPSLocationJSONCreator(ret_grid_model_branch, class_label_branch, class_label_FromBus) 
print("*******************The JSON_Generator.py is finished*******************")