##########################################
# Author: Feroz Farazi (msff2@cam.ac.uk) #
# Date: 24 Nov 2020                      #
##########################################

"""This module defines SPARQL queries"""

def get_all_power_plant_iri():
    query = """
        PREFIX powerplant:<http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        SELECT ?powerPlantIRI
        WHERE
        {
        ?powerPlantIRI rdf:type powerplant:PowerPlant .
        } LIMIT 10
        """
    return query

def get_power_plant_coordinates(power_plant_iri):
    query = """
    PREFIX system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX space_and_time_extended:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
    SELECT ?latitude ?longitude
    WHERE
    {
    <%s> space_and_time_extended:hasGISCoordinateSystem ?CoordinateSystem .
    ?CoordinateSystem  space_and_time_extended:hasProjectedCoordinate_x ?x_coordinate .
    ?CoordinateSystem  space_and_time_extended:hasProjectedCoordinate_y ?y_coordinate .
    ?x_coordinate  system:hasValue ?GPS_x_coordinate .
    ?y_coordinate  system:hasValue ?GPS_y_coordinate . 
    ?GPS_x_coordinate  system:numericalValue ?latitude .
    ?GPS_y_coordinate  system:numericalValue ?longitude .
    }
    """ % power_plant_iri
    return query

def get_fuel_type(power_plant_iri):
    query = """
    PREFIX powerplant:  			<http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
    PREFIX rdf:        				<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    SELECT ?fuel
    WHERE
    {
    <%s> technical_system:realizes ?Generation_Type .
    ?Generation_Type powerplant:consumesPrimaryFuel ?fuel .
    }
    """ % power_plant_iri
    return query

def get_capacity(power_plant_iri):
    query = """
    PREFIX system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX system_realization: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#>
    SELECT ?capacity ?capacity_units
    WHERE
    {
    <%s> system_realization:designCapacity ?Capacity .
    ?Capacity system:hasValue ?Capacity_value .
    ?Capacity_value system:numericalValue ?capacity .
    ?Capacity_value system:hasUnitOfMeasure ?capacity_units .
    }
    """ % power_plant_iri
    return query