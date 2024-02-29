################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 14 Apr 2022                            #
################################################

import pathlib
import datetime as dt
from flask import Blueprint, request, jsonify

from agent.dataretrieval.stations import create_json_output_files
from agent.utils.readings_mapping import TIME_FORMAT
from agent.errorhandling.exceptions import InvalidInput


# Initialise logger
from py4jps import agentlogging
logger = agentlogging.get_logger("prod")

outputtasks_bp = Blueprint(
    'outputtasks_bp', __name__
)

# Define route for API request to retrieve station and reading data and create
# output files for Digital Twin Visualisation Framework
# All query parameters are expected as SINGLE JSON object 'query' (to follow
# the convention introduced in the JPS_BASE_LIB)
@outputtasks_bp.route('/airqualityagent/retrieve/all', methods=['GET'])
def api_retrieve_all_stations():
    #
    # Check arguments (query parameters)
    #
    inputs = {'outdir': None,
              'observation_types': None,
              'circle_center': None,
              'circle_radius': None,
              'tmin': None
    }

    # Get received 'query' JSON object which holds all parameters
    try:
        query = request.json['query']
    except Exception as ex:
        logger.error('No JSON "query" object could be identified.')
        raise InvalidInput('No JSON "query" object could be identified.') from ex

    # Output directory
    try:
        inputs['outdir'] = str(query['outDir'])
    except Exception as ex:
        logger.error('Required output directory could not be determined.')
        raise InvalidInput('Required output directory could not be determined.') from ex
    if not pathlib.Path.exists(pathlib.Path(inputs['outdir'])):
        logger.error('Provided output directory does not exist.')
        raise InvalidInput('Provided output directory does not exist.')

    if 'observationTypes' in query:
        try:
            obstypes = list(query['observationTypes'])
            obstypes = [str(i) for i in obstypes]
            # Remove potential EMS namespace if IRI is provided
            inputs['observation_types'] = [i.split('/')[-1].split('>')[0] for i in obstypes]
        except Exception as ex:
            logger.error('Parameter "observationTypes" not provided in expected format (list of strings).')
            raise InvalidInput('Parameter "observationTypes" not provided in expected format (list of strings).') from ex

    # Parameters for potential geospatial search
    if 'circleCenter' in query and 'circleRadius' in query:
        try:
            inputs['circle_center'] = str(query['circleCenter'])
            inputs['circle_radius'] = str(query['circleRadius'])
        except Exception as ex:
            logger.error('Parameter "circleCenter" and/or "circleRadius" not provided in expected format.')
            raise InvalidInput('Parameter "circleCenter" and/or "circleRadius" not provided in expected format.') from ex
        if '#' not in inputs['circle_center']:
            logger.error('Parameter "circleCenter" does not follow "lat#lon" format.')
            raise InvalidInput('Parameter "circleCenter" does not follow "lat#lon" format.')           

    # Get earliest time stamp to retrieve
    tnow = dt.datetime.today()
    tnow = dt.datetime(tnow.year, tnow.month, tnow.day, tnow.hour)
    try:
        diff = dt.timedelta(days=int(request.args['daysBack']))
    except:
        logger.info('Duration to retrieve could not be determined, using default.')
        diff = dt.timedelta(days=14)
    tmin = tnow - diff
    inputs['tmin'] = tmin.strftime(TIME_FORMAT)
    
    #
    # Create output files
    #
    try:
        # Instantiate stations
        create_json_output_files(**inputs)
        print(f"Output files written to: {inputs['outdir']}")
        return jsonify({'msg': f"Output files written to: {inputs['outdir']}"}), 200

    except Exception as ex:
        logger.error('Retrieving outputs failed: ' + str(ex))
        return jsonify({'msg': 'Retrieving outputs failed: ' + str(ex)}), 500
