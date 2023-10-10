################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 09 Oct 2022                            #
################################################

# This module provides some methods and functionality required by the agent 
# flaskapp

import uuid
from datetime import datetime, timedelta

from py4jps import agentlogging

from agent.datamodel import *
from agent.kgutils.kgclient import KGClient
from agent.utils.agent_configs import QUERY_ENDPOINT, UPDATE_ENDPOINT, \
                                      FORECASTING_AGENT, DH_OPTIMISATION_AGENT, \
                                      EMISSION_ESTIMATION_AGENT

# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


def validate_input_params(params_dict):
    """
    Validate suitability of received HTTP request parameters (converted to dict)
    """

    def _validate_int_parameter(param_name, param_value):
        if not isinstance(param_value, int) or param_value <= 0:
            raise ValueError(f"{param_name} should be an integer greater than 0")

    # Check if all required keys are present
    required_keys = ['start', 'optHorizon', 'numberOfTimeSteps', 'timeDelta', 
                     'heatDemandDataLength', 'gridTemperatureDataLength']
    for key in required_keys:
        if key not in params_dict:
            raise ValueError(f"Missing required parameter: {key}")

    # Validate startDateTime
    start_date_time = params_dict['start']
    try:
        datetime.strptime(start_date_time, '%Y-%m-%dT%H:%M:%SZ')
    except ValueError:
        raise ValueError("Invalid start dateTime format. Expected format: YYYY-MM-DDThh:mm:ssZ")

    # Convert startDateTime to unix timestamp if needed
    unix_timestamp = datetime.timestamp(datetime.strptime(start_date_time, '%Y-%m-%dT%H:%M:%SZ'))
    params_dict['start'] = int(unix_timestamp)

    # Validate integer parameters
    _validate_int_parameter('optHorizon', params_dict['optHorizon'])
    _validate_int_parameter('numberOfTimeSteps', params_dict['numberOfTimeSteps'])
    _validate_int_parameter('heatDemandDataLength', params_dict['heatDemandDataLength'])
    _validate_int_parameter('gridTemperatureDataLength', params_dict['gridTemperatureDataLength'])

    # Validate frequency and map to constants
    valid_frequencies = {
        "day": (TIME_UNIT_DAY, int(timedelta(days=1).total_seconds())),
        "hour": (TIME_UNIT_HOUR, int(timedelta(hours=1).total_seconds())),
        "minute": (TIME_UNIT_MINUTE, int(timedelta(minutes=1).total_seconds())),
        "second": (TIME_UNIT_SECOND, int(timedelta(seconds=1).total_seconds()))
    }

    frequency = params_dict['timeDelta']
    if frequency not in valid_frequencies:
        raise ValueError("Invalid timeDelta. Valid options: 'day', 'hour', 'minute', 'second'")
    params_dict['timeDelta'] = valid_frequencies[frequency][0]
    params_dict['timeDelta_unix'] = valid_frequencies[frequency][1]

    return params_dict


def upload_triples(sparql_client):
    """
    Upload triples from 'resources' folder and add covariates to forecasting models
    
    Arguments:
        sparql_client {KGClient} - initialise KG client
    """
    
    # Import triples from 'resources' folder upon app startup
    logger.info("Importing triples from 'resources/triples' folder...")
    sparql_client.initialise_namespace('./resources/triples')
    logger.info("Successfully imported triples.")
    
    # Add covariate relationships to uploaded forecasting model instances
    # TODO: Add covariate relationships for grid temperatures if necessary
    logger.info("Adding covariate relationships to relevant forecasting models...")
    if sparql_client.check_if_triple_exist(fc_model_heat_demand, RDF_TYPE, TS_FORECASTINGMODEL):
        # Get ambient air temperature and public holiday covariate IRIs
        # NOTE: assumes exactly one instance of each type in the KG, i.e.,
        #       ideally instantiate district heating in separate namespace
        temp, holi = sparql_client.get_heat_demand_covariates()
        # Add covariate relationships
        sparql_client.instantiate_covariate_relationships(fc_model_heat_demand, [temp, holi])
        logger.info("Covariate relationships successfully added.")
    else:
        msg = 'Heat demand forecasting model instance not found in KG. '
        msg += 'Please ensure it is instantiated and properly referenced in iris.py'
        logger.error(msg)
        raise ValueError(msg)
    

def raise_value_error(msg):
    logger.error(msg)
    raise ValueError(msg)


def create_new_time_instances():
    # Simulation time
    sim_t = KB + 'SimulationTime_' + str(uuid.uuid4())
    # Optimisation interval
    opti_int = KB + 'OptimisationInterval_' + str(uuid.uuid4())
    opti_t1 = KB + 'OptimisationStartInstant_' + str(uuid.uuid4())
    opti_t2 = KB + 'OptimisationEndInstant_' + str(uuid.uuid4())
    # Heat demand & grid temperature data length
    heat_length = KB + 'HeatDemandDuration_' + str(uuid.uuid4())
    tmp_length = KB + 'GridTemperatureDuration_' + str(uuid.uuid4())
    # Forecast frequency
    freq = KB + 'Frequency_' + str(uuid.uuid4())
    
    return sim_t, opti_int, opti_t1, opti_t2, heat_length, tmp_length, freq