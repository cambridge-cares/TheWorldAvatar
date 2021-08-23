####################################################
# Author: Wanni Xie (wx243@cam.ac.uk)              #
# Last Update Date: 18 August 2021                 #
####################################################

from GPSLocationChecker import check_GPS_char, GPS_special_chars, check_GPS
from queryStrings import queryPowerPlantForVisualisation, queryUKElectricityConsumptionAndAssociatedGEOInfo
from geoJSONCreator import powerPlantgeoJSONCreator, elecConsAndGEOInfogeoJSONCreator

"""Endpoints"""
powerPlant = "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKPowerPlantKG"
electricity_consumption_RDF4j_Endpoint = "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKEnergyConsumptionKG"
ONS = "http://statistics.data.gov.uk/sparql.json"

"""PowerPlant query"""
ret_pow = queryPowerPlantForVisualisation(powerPlant)
ret_pow = check_GPS(ret_pow)

"""PowerPlant query"""
ret_elec_region = queryUKElectricityConsumptionAndAssociatedGEOInfo(electricity_consumption_RDF4j_Endpoint, ONS, True) # query the region consumption
ret_elec_area = queryUKElectricityConsumptionAndAssociatedGEOInfo(electricity_consumption_RDF4j_Endpoint, ONS, False) # query the sub areas consumption


"""Labels"""
class_label_pp = 'UK_PowerPlants'
class_label_elec_region = "UK_Electricity_Consumption_Regions"
class_label_elec_area = "UK_Electricity_Consumption_Areas"

"""Create GEOJSON files"""
#powerPlantgeoJSONCreator(ret_pow, class_label_pp)
elecConsAndGEOInfogeoJSONCreator(ret_elec_region, class_label_elec_region)
elecConsAndGEOInfogeoJSONCreator(ret_elec_area, class_label_elec_area)