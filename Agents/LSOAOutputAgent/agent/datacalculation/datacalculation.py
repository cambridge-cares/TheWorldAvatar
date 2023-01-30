################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk) #
# Date: 30/11 2022                            #
################################################

# The purpose of this module is to perform data calculation by calling various calculation agent
# 
# NOTE: pd.DataFrame is used frequently in this module, where for all the dataframe
# without special notice, the column[0] should all be LSOA code used as identifier

import agentlogging
from agent.errorhandling.exceptions import *
from agent.dataretrieval.dataretrival import *

import numpy as np
import requests


# Initialise logger
logger = agentlogging.get_logger("prod")

# ---------------------- Calculation Agent---------------------------------- #
def call_cop_agent(url: str, temp: np.ndarray, unit:str, lsoa_sequence:np.ndarray = None):
    '''
    This module will call the calculation agent(cop) to perform calculation with the parameters and url specified
    
    Arguments:
    temperature: should be an array (np.ndarray) I suppose you are not interested in calculating single value...
    unit: unit with respect to temperature, must be wrapped as a url as per OntoMeasurement. Units below are available:
          DegreeCelsius ℃:    http://www.ontology-of-units-of-measure.org/resource/om-2/degreeCelsius
          DegreeFahrenheit ℉:    http://www.ontology-of-units-of-measure.org/resource/om-2/degreeFahrenheit
          Kelvin K:    http://www.ontology-of-units-of-measure.org/resource/om-2/kelvin
    lsoa_sequence: should be an array (np.ndarray), this is the respective lsoa code with the temperature you provided, which must be wrapped as a url as per OntoGasGrid
                   if the lsoa_sequence is not provided the agent will still run and the result will still be given, with a warning in the logger
    url: the endpoint to perform calculation
    '''
    
    # Wrapping the query
    if lsoa_sequence is not None:
        query = {
            'query':{'temperature':temp.tolist(),
                    'unit':unit,
                    'lsoa_sequence':lsoa_sequence.tolist()
                    }
        }
    else:
        query = {
            'query':{'temperature':temp.tolist(),
                    'unit':unit
                    }
        }

    headers = {'Content-type': 'application/json'}
    response = requests.get(url, headers=headers, json=query)

    data = response.json()

    cop = np.array(data['COP'])
    if lsoa_sequence is not None:
        lsoa_sequence = np.array(data['lsoa_sequence'])

    return cop, lsoa_sequence
