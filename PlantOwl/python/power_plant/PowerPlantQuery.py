##########################################
# Author: Feroz Farazi (msff2@cam.ac.uk) #
# Date: 24 Nov 2020                      #
##########################################

"""Here we demonstrate how to query information from a triple store and put it in a variable"""

import SparqlQuery as sq
import PowerPlant as pp
import Queries as q

powerPlants = []

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

"""The following block of code runs first if this module is executed"""
if __name__ == '__main__':

    """Sets the URL of the triple store deployed on CoMo to query UK power plants"""
    endpoint = "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKPowerPlant"

    """Sets the SPARQL query to retrieve IRIs of UK power plants"""
    results = sq.query_endpoint(endpoint, q.get_all_power_plant_iri())

    print ('Retrieves and assigns the name, capacity, latitude, longitude and fuel type of all power plants:')
    countPowerPlant = 0
    for result in results["results"]["bindings"]:
        countPowerPlant = countPowerPlant+ 1
        print('Plant %d:' %countPowerPlant)
        """Puts the IRI of power plant in a variable"""
        power_plant_iri = result["powerPlantIRI"]['value']
        """Creates an instance of the PowerPlant class to store the name, capacity, latitude, longitude and fuel type"""
        power_plant = pp.PowerPlant()
        """Extracts and assigns the name of power plant to the instance"""
        power_plant.name = extract_name(power_plant_iri)
        """Queries the latitude and longitude of power plant"""
        coordinates = sq.query_endpoint(endpoint, q.get_power_plant_coordinates(power_plant_iri))
        for coordinate in coordinates["results"]["bindings"]:
            """Assigns the latitude of power plant to the class instance"""
            power_plant.latitude = coordinate["latitude"]['value']
            """Assigns the longitude of power plant to the class instance"""
            power_plant.longitude = coordinate["longitude"]['value']
        """Queries the fuel type of power plant"""
        fuel_types = sq.query_endpoint(endpoint, q.get_fuel_type(power_plant_iri))
        for fuel_type in fuel_types["results"]["bindings"]:
            """Assigns the fuel type of power plant to the class instance"""
            power_plant.fuel_type = fuel_type["fuel"]['value']
        """Queries the capacity of power plant"""
        capacity = sq.query_endpoint(endpoint, q.get_capacity(power_plant_iri))
        for c in capacity["results"]["bindings"]:
            """Assigns the capacity of power plant to the class instance"""
            power_plant.capacity = c["capacity"]['value']
            """Assigns the units of capacity of power plant to the class instance"""
            power_plant.capacity_units = c["capacity_units"]['value']

        """Prints the name, capacity, latitude, longitude and fuel type of power plant so that you can see"""
        print(' Name: ', power_plant.name)
        print(' Capacity: ', power_plant.capacity, ' Units: ', power_plant.capacity_units)
        print(' Latitude: ', power_plant.latitude)
        print(' Longitude: ', power_plant.longitude)
        print(' Fuel type: ', power_plant.fuel_type)

