################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 21 Aug 2023                            #
################################################

# The purpose of this module is to provide the actual estimation logic to 
# calculate emissions from consumed gas and provided heat

from py4jps import agentlogging

from emissionagent.datamodel.iris import *


# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


def calculate_emissions_for_consumed_gas(pollutant_iri:str, consumed_gas:float) -> dict:
    """
    Calculates the emissions for consumed gas

    Arguments:
        pollutant_iri {str} -- IRI of pollutant ID for which to calculate emissions
        consumed_gas {float} -- amount of consumed gas, MWh
    Returns:
        emission {dict} -- emission data to be instantiated as derivation output
    """
    
    # Initialise return dictionary
    emission = {
        'pollutantID': pollutant_iri,
        'temperature': 10.0,
        'density': 10.0,
        'massflow': 10.0
    }
    
    # TODO: Implement emission calculation

    return emission


def calculate_emissions_for_provided_heat(pollutant_iri:str, provided_heat:float) -> dict:
    """
    Calculates the emissions for consumed gas

    Arguments:
        pollutant_iri {str} -- IRI of pollutant ID for which to calculate emissions
        provided_heat {float} -- amount of sourced heat, MWh
    Returns:
        emission {dict} -- emission data to be instantiated as derivation output
    """
    
    # Initialise return dictionary
    emission = {
        'pollutantID': pollutant_iri,
        'temperature': 10.0,
        'density': 10.0,
        'massflow': 10.0
    }
    
    # TODO: Implement emission calculation

    return emission
