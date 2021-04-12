#############################################################################################
# Author: Zach and Dave                                                                     #
# Date: 23 March 2021                                                                       #
# Acknowledgement: we are thankful to Feroz Farazi (msff2@cam.ac.uk) for reviewing the code #
#############################################################################################
"""Here we demonstrate how to query information from a triple store and put it in a variable"""

import SparqlQuery as sq
import PowerPlant as pp
import Queries as q
import Lucode as l
import numpy as np
import json

from kgConnection.app_module import doTask

"""Sets the name of the OntoCropMapGML knowledge base"""
onto_crop_map_gml_kb = "ontocropmapgml"
"""Sets the name of the OntoCropEnergy knowledge base"""
onto_crop_energy_kb = "ontocropenergy"
"""Sets the name of the UKPowerPlant knowledge base"""
uk_power_plant_kb = "ukpowerplant"
"""The name of power plant"""
power_plant_name = "Ely UK"

"""The name of power plant is extracted from the IRI as it is not codified as a separate attribute"""
def extract_name(iri):
    if "#" in iri:
        tokensHashSeparated = iri.split("#")
        if len(tokensHashSeparated) > 1:
            if ".owl" in tokensHashSeparated[0]:
                tokensHashSeparated[0] = tokensHashSeparated[0].replace(".owl", "")
            if "/" in tokensHashSeparated[0]:
                tokensSlashSepated = tokensHashSeparated[0].split("/")
                length = len(tokensSlashSepated)
                name = tokensSlashSepated[length - 1]
                if "_" in name:
                    return name.replace("_", " ")

"""Retrieves the details of the power plant required for the use case"""
def get_power_plant_data(endpoint, query):
    """Sets the SPARQL query to retrieve IRIs of the UK power plant of interest"""
    results = doTask(query, uk_power_plant_kb, True, True)
    results = json.loads(results)
    countPowerPlant = 0
    print ('Retrieves and assigns the name, capacity, latitude, longitude and fuel type of all power plants:')
    for result in results:
        countPowerPlant = countPowerPlant+ 1
        print('Plant %d:' %countPowerPlant)
        """Puts the IRI of power plant in a variable"""
        power_plant_iri = result["powerPlantIRI"]
        """Creates an instance of the PowerPlant class to store the name, capacity, latitude, longitude and fuel type"""
        power_plant = pp.PowerPlant()
        """Extracts and assigns the name of power plant to the instance"""
        power_plant.name = extract_name(power_plant_iri)
        """Queries the latitude and longitude of power plant"""
        coordinates = json.loads(doTask(q.get_power_plant_coordinates(power_plant_iri), uk_power_plant_kb, True, True))
        for coordinate in coordinates:
            """Assigns the latitude of power plant to the class instance"""
            power_plant.latitude = coordinate["latitude"]
            """Assigns the longitude of power plant to the class instance"""
            power_plant.longitude = coordinate["longitude"]
        """Queries the fuel type of power plant"""
        fuel_types = json.loads(doTask(q.get_fuel_type(power_plant_iri), uk_power_plant_kb, True, True))
        for fuel_type in fuel_types:
            """Assigns the fuel type of power plant to the class instance"""
            power_plant.fuel_type = fuel_type["fuel"]
        """Queries the capacity of power plant"""
        capacity = json.loads(doTask(q.get_capacity(power_plant_iri), uk_power_plant_kb, True, True))
        for c in capacity:
            """Assigns the capacity of power plant to the class instance"""
            power_plant.capacity = c["capacity"]
            """Assigns the units of capacity of power plant to the class instance"""
            power_plant.capacity_units = c["capacity_units"]
        """Prints the name, capacity, latitude, longitude and fuel type of power plant so that you can see"""
        print(' Name: ', power_plant.name)
        print(' Capacity: ', power_plant.capacity, ' Units: ', power_plant.capacity_units)
        print(' Latitude: ', power_plant.latitude)
        print(' Longitude: ', power_plant.longitude)
        print(' Fuel type: ', power_plant.fuel_type)
    return power_plant

"""Generates a map between LUCODE IRIs and crop map IRIs"""
def get_lucode_crop_map(onto_crop_energy_endpoint):
    lucode_crop_map = dict()
    crops = json.loads(doTask(q.get_lucode_label(), onto_crop_energy_kb, True, True))
    print('crops:', crops)
    for c in crops:
        """Puts the IRI of the crop in a variable"""
        crop_iri = c["Crop"]
        """Assigns the lucode of crop to the class instance"""
        lucode_iri = c["Lucode"]
        lucode = lucode_iri[-4:]
        """Creates the map between IRIs"""
        lucode_crop_map[lucode_iri] = crop_iri
        lucode_crop_map[lucode_iri.replace("www.", "")] = crop_iri
        print('Crop: ', crop_iri)
        print(' LucodeIRI: ', lucode_iri)
        print(' Lucode: ', lucode)
    return lucode_crop_map

"""Calculates different parameters of crops, including energy and power, produced within a geographical region"""
def calculate_crop_parameters(lucode_crop_map, onto_crop_map_gml_endpoint, onto_crop_energy_endpoint, power_plant):
    P = []
    """Combines the desired coordinates into a usable form for the geospatial query"""
    latlon = '"' + str(power_plant.latitude) + '#' + str(power_plant.longitude) + '"'
    """Counts the number of polygons within a circle centred at the coordinates of a power plant"""
    results = json.loads(doTask(q.get_lucode_count(latlon), onto_crop_map_gml_kb, True, True))
    for result in results:
        """Creates an instance of the Crop class to store the name, lucode, yeild and energy"""
        crop = l.Crop()
        """Assigns the number of instances of crop to the class instance"""
        if result["Lucode"] in lucode_crop_map.keys():
            crop.crop_number = result["LucodeTotal"]
            lucode_iri = result["Lucode"]
            print(lucode_crop_map.get(lucode_iri))
            results_yield = json.loads(doTask(q.get_crop_yield(lucode_crop_map.get(lucode_iri)), onto_crop_energy_kb, True, True))
            for result in results_yield:
                """Assigns the yield of crop to the class instance"""
                crop.crop_yield = result["Yield"]
            print(lucode_crop_map.get(lucode_iri))
            results_energy = json.loads(doTask(q.get_crop_energy(lucode_crop_map.get(lucode_iri)), onto_crop_energy_kb, True, True))
            for result in results_energy:
                """Assigns the energy of crop to the class instance"""
                crop.crop_energy = result["HHV"]

            print(' Quantity: ', crop.crop_number)
            print(' Yield: ', crop.crop_yield)
            print(' Energy: ', crop.crop_energy)

            power = float(crop.crop_number) * 0.4156872 * float(crop.crop_yield) * 1000 * float(
                crop.crop_energy) * 1000 / (365.25 * 24 * 3600)
            print(' Power: ', power)
            P.append(power)

    print(P)
    available_power = np.sum(P)
    output_power = float(available_power) * 0.325
    print('Total Available Power: ', available_power, ' kW')
    print('Output Power: ', output_power, 'kW')

"""If the name of a power plant is provided, it extracts the IRI of the power plant, finds the geospatial 
location of it and calculates the bioenergy available in its surroundings"""
def execute_energy_use_case(power_plant_name):
    """Replaces spaces with underscores to form an IRI from the name"""
    power_plant_name = power_plant_name.replace(" ", "_")
    power_plant = get_power_plant_data(uk_power_plant_kb, q.get_use_case_power_plant_iri(power_plant_name.replace(" ", "_")))
    lucode_crop_map = get_lucode_crop_map(onto_crop_energy_kb)
    calculate_crop_parameters(lucode_crop_map, onto_crop_map_gml_kb, onto_crop_energy_kb, power_plant)

"""The following block of code runs first if this module is executed"""
if __name__ == '__main__':
    execute_energy_use_case(power_plant_name)