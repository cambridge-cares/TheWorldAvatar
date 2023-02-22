################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 22 Feb 2023                            #
################################################

# The purpose of this module is to define functions to be called via Celery
# Decorated functions become task objects with methods to call it in the background

from celery import shared_task

from py4jps import agentlogging

from agent.errorhandling.exceptions import InvalidInput
from agent.utils.env_configs import OCGML_ENDPOINT
from agent.utils.stack_configs import QUERY_ENDPOINT, UPDATE_ENDPOINT
from agent.kgutils.initialise_kb import create_blazegraph_namespace, upload_ontology
from agent.kgutils.initialise_ocgml import upload_ocgml_quads
from agent.datainstantiation.postcodes import initialise_postcodes
from agent.datainstantiation.epc_instantiation import instantiate_epc_data_for_certificate, \
                                                        instantiate_epc_data_for_all_postcodes, \
                                                        add_ocgml_building_data

# Initialise logger
logger = agentlogging.get_logger("prod")


# Define task to initialise postcodes for provided local authority district
@shared_task(ignore_result=False)
def task_initialise_postcodes(**inputs) -> dict:
    try:
        # Instantiate postcodes
        postcodes = initialise_postcodes(**inputs)
        if not postcodes:
            return {'msg': 'Local authority code already instantiated'}
        else:
            return {'Newly instantiated postcodes': postcodes}
    except Exception as ex:
        logger.error('Unable to instantiate local authority with postcodes.', ex)
        return {'msg': 'Postcode instantiation failed: ' + str(ex)}

