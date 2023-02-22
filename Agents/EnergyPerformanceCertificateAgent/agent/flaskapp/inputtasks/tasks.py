################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 22 Feb 2023                            #
################################################

# The purpose of this module is to define functions to be called via Celery
# Decorated functions become task objects with methods to call it in the background

from celery import shared_task

from py4jps import agentlogging

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


# Define task to initialise/update single EPC
@shared_task(ignore_result=False)
def task_instantiate_epc_data_for_certificate(**inputs) -> dict:
    try:
        # Instantiate/update EPC
        epcs = instantiate_epc_data_for_certificate(**inputs)
        return_dict = {'Newly instantiated EPCs': epcs[0],
                       'Updated EPCs': epcs[1]}
        for key, value in return_dict.items():
            print(key, ' : ', value)
        return return_dict   
                            
    except Exception as ex:
        logger.error('Unable to instantiate EPC data.', ex)
        return {'msg': 'EPC data instantiation failed: ' + str(ex)}


# Define task to initialise/update all EPCs
@shared_task(ignore_result=False)
def task_instantiate_epc_data_for_all_uprns(**inputs) -> dict:
    try:
        # Instantiate/update all EPCs
        epcs = instantiate_epc_data_for_all_postcodes(**inputs)
        return_dict = {'Newly instantiated EPCs': epcs[0][0],
                       'Updated EPCs': epcs[0][1],
                       'Newly instantiated parent buildings': epcs[1][0],
                       'Updated parent buildings': epcs[1][1]}
        for key, value in return_dict.items():
            print(key, ' : ', value)
        return return_dict

    except Exception as ex:
        logger.error('Unable to instantiate EPC data.', ex)
        return {'msg': 'EPC data instantiation failed: ' + str(ex)}


# Define task to initialise/update all EPCs
@shared_task(ignore_result=False)
def task_add_ocgml_building_data() -> dict:
    try:
        # Retrieve and instantiate building footprint/elevation
        ocgml = add_ocgml_building_data()
        return_dict = {'Instantiated PostGIS footprints': ocgml[0],
                       'Already instantiated PostGIS footprints': ocgml[1],
                       'Deleted building elevations': ocgml[2],
                       'Instantiated building elevations': ocgml[3]}
        for key, value in return_dict.items():
            print(key, ' : ', value)
        return return_dict

    except Exception as ex:
        logger.error('Unable to instantiate PostGIS features and/or OntoBuiltEnv building elevations.', ex)
        return {'msg': 'Instantiating OntoCityGml data failed: ' + str(ex)}
