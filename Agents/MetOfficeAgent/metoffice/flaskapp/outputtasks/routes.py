###############################################
# Authors: Markus Hofmeister (mh807cam.ac.uk) #    
# Date: 14 Apr 2022                           #
###############################################

import pathlib
import datetime as dt
from flask import Blueprint, request, jsonify

#import agentlogging
from metoffice.dataretrieval.stations import create_json_output_files
from metoffice.utils.readings_mapping import TIME_FORMAT
from metoffice.errorhandling.exceptions import InvalidInput


# Initialise logger
#logger = agentlogging.get_logger("dev")

outputtasks_bp = Blueprint(
    'outputtasks_bp', __name__
)

# Define route for API request to retrieve station and reading data and create
# output files for Digital Twin Visualisation Framework
# All query parameter are expected as SINGLE JSOn object 'query' (to follow
# the convention introduced in the JPS_BASE_LIB)
@outputtasks_bp.route('/api/metofficeagent/retrieve/all', methods=['GET'])
def api_update_all_stations():
    #
    # Check arguments (query parameters)
    #
    inputs = {'outdir': None,
              'observation_types': None,
              'split_obs_fcs': False,
              'circle_center': None,
              'circle_radius': None,
              'tmin': None
    }

    # Get received 'query' JSON object which holds all parameters
    try:
        query = request.json['query']
    except:
        #logger.error('No JSON "query" object could be identified.')
        raise InvalidInput('No JSON "query" object could be identified.')

    # Output directory
    try:
        inputs['outdir'] = str(query['outDir'])
    except:
        #logger.error('Required output directory could not be determined.')
        raise InvalidInput('Required output directory could not be determined.')
    if not pathlib.Path.exists(pathlib.Path(inputs['outdir'])):
        #logger.error('Provided output directory does not exist.')
        raise InvalidInput('Provided output directory does not exist.')

    if 'observationTypes' in query:
        try:
            obstypes = list(query['observationTypes'])
            obstypes = [str(i) for i in obstypes]
            # Remove potential EMS namespace if IRI is provided
            inputs['observation_types'] = [i.split('#')[-1].split('>')[0] for i in obstypes]
        except:
            #logger.error('Parameter "observationTypes" not provided in expected format (list of strings).')
            raise InvalidInput('Parameter "observationTypes" not provided in expected format (list of strings).')
    
    # Whether to provide joint or separate output files
    if 'splitObsFcs' in query:
        try:
            inputs['split_obs_fcs'] = bool(query['splitObsFcs'])
        except:
            #logger.error('Parameter "splitObsFcs" not provided in expected format (boolean).')
            raise InvalidInput('Parameter "splitObsFcs" not provided in expected format (boolean).')

    # Parameters for potential geospatial search
    if 'circleCenter' in query and 'circleRadius' in query:
        try:
            inputs['circle_center'] = str(query['circleCenter'])
            inputs['circle_radius'] = str(query['circleRadius'])
        except:
            #logger.error('Parameter "circleCenter" and/or "circleRadius" not provided in expected format.')
            raise InvalidInput('Parameter "circleCenter" and/or "circleRadius" not provided in expected format.')
        if '#' not in inputs['circle_center']:
            #logger.error('Parameter "circleCenter" does not follow "lat#lon" format.')
            raise InvalidInput('Parameter "circleCenter" does not follow "lat#lon" format.')           

    # Get earliest time stamp to retrieve
    tnow = dt.datetime.today()
    tnow = dt.datetime(tnow.year, tnow.month, tnow.day, tnow.hour)
    try:
        diff = dt.timedelta(days=int(request.args['daysBack']))
    except:
        #logger.info('Duration to retrieve could not be determined, using default.')
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
        return jsonify({"status": '200', "msg": "success"})

    except Exception as ex:
        print(ex)
        return jsonify({"status": '500', 'errormsg': 'Update failed'})
