####################################################
# Author: Wanni Xie (wx243@cam.ac.uk)              #
# Last Update Date: 25 August 2021                 #
####################################################

from GPSLocationChecker import check_GPS_char, GPS_special_chars, check_GPS
from queryStrings import queryPowerPlantForVisualisation, queryUKElectricityConsumptionAndAssociatedGEOInfo, queryGridModeltForVisualisation_Bus, queryGridModeltForVisualisation_Bus_alt
from geoJSONCreator import powerPlantgeoJSONCreator, elecConsAndGEOInfogeoJSONCreator, busModelJSONCreator

"""Endpoints"""
powerPlant = "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKPowerPlantKG"
electricity_consumption_RDF4j_Endpoint = "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKEnergyConsumptionKG"
ONS = "http://statistics.data.gov.uk/sparql.json"
topoEndpoint = "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKPowerGridTopology"
busModelEndpoint = "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKPowerGridModel"

"""PowerPlant query"""
# ret_pow = queryPowerPlantForVisualisation(powerPlant)
# ret_pow = check_GPS(ret_pow)

"""PowerPlant query"""
# ret_elec_region = queryUKElectricityConsumptionAndAssociatedGEOInfo(electricity_consumption_RDF4j_Endpoint, ONS, True) # query the region consumption
# ret_elec_area = queryUKElectricityConsumptionAndAssociatedGEOInfo(electricity_consumption_RDF4j_Endpoint, ONS, False) # query the sub areas consumption

"""Grid model query"""
ret_grid_model_bus = queryGridModeltForVisualisation_Bus(topoEndpoint, busModelEndpoint)
# ret_grid_model_bus = queryGridModeltForVisualisation_Bus_alt(topoEndpoint, busModelEndpoint)

"""Labels"""
class_label_pp = 'UK_PowerPlants'
class_label_elec_region = "UK_Electricity_Consumption_Regions"
class_label_elec_area = "UK_Electricity_Consumption_Areas"
class_label_busPara = "UK_Grid_Model_Bus_Parameter"
class_label_busInputVar = "UK_Grid_Model_Bus_InputVar"



"""Create GEOJSON files"""
# powerPlantgeoJSONCreator(ret_pow, class_label_pp)
# elecConsAndGEOInfogeoJSONCreator(ret_elec_region, class_label_elec_region)
# elecConsAndGEOInfogeoJSONCreator(ret_elec_area, class_label_elec_area)
busModelJSONCreator(ret_grid_model_bus, class_label_busPara, class_label_busInputVar)