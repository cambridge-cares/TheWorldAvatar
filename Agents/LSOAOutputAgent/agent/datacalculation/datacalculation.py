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

# ----------------------Call Calculation Agent---------------------------------- #
def call_cop_agent(url: str, temp: np.ndarray, unit:str, t_h: float = None, hp_efficiency: float = None):
    '''
    This module will call the calculation agent(cop) to perform calculation with the parameters and url specified
    
    Arguments:
    url: the endpoint to perform calculation
    temperature: should be an array (np.ndarray) I suppose you are not interested in calculating single value...
    unit: unit with respect to temperature, must be wrapped as a url as per OntoMeasurement. Units below are available:
          DegreeCelsius ℃:    http://www.ontology-of-units-of-measure.org/resource/om-2/degreeCelsius
          DegreeFahrenheit ℉:    http://www.ontology-of-units-of-measure.org/resource/om-2/degreeFahrenheit
          Kelvin K:    http://www.ontology-of-units-of-measure.org/resource/om-2/kelvin
          t_h: hot side temperature (see equation above), if not provided, 318.15 will be used as default value.
          hp_efficiency: heat pump efficiency (see equation above), if not provided, 0.35 will be used as default value.
    '''
    
    # Wrapping the query
    query = {
        'query':{'temperature':temp.tolist(),
                'unit':unit
                }
        }
    
    if t_h:
        query['query']['t_h'] = t_h
    else:
        logger.info('Using default hot side temperature (318.15K) when calculating COP')

    if hp_efficiency:
        query['query']['hp_efficiency'] = hp_efficiency
    else:
        logger.info('Using default heat pump efficiency (0.35) when calculating COP')

    logger.info('Sending data to calculationagent_cop...')
    headers = {'Content-type': 'application/json'}
    response = requests.get(url, headers=headers, json=query)

    data = response.json()

    cop = np.array(data['COP'])
    logger.info('COP successfully calculated')

    return cop

