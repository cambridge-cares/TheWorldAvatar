################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk) #
# Date: 30/11 2022                            #
################################################

from flask import Blueprint, request, jsonify

import agentlogging
from agent.datamodel.spec import YEAR
from agent.datainstantiation.readings import read_from_web_elec,read_from_web_gas,read_from_web_fuel_poverty,read_from_web_temp

# Initialise logger
logger = agentlogging.get_logger("prod")


downloadtasks_bp = Blueprint(
    'downloadtasks_bp', __name__
)

# Define route for API request to download electricity data
@downloadtasks_bp.route('/api/electricityconsumptionagent/download/electricity', methods=['GET'])
def api_download_electricity():
    # Check arguments (query parameters)
    if len(request.args) > 0:
        #print("Query parameters provided, although not required. " \
        #      + "Provided arguments will be neglected.")
        logger.warning("Query parameters provided, although not required. \
                        Provided arguments will be neglected.")
    try:
        # Download only
        read_from_web_elec()
        print(f'xlsx file for electricity consumption in year {YEAR} successfully downloaded at current folder')
        return jsonify({"status": '200', "msg": "success"})

    except Exception as ex:
        print(ex)
        return jsonify({"status": '500', 'errormsg': 'Instantiation failed'})

# Define route for API request to download gas data
@downloadtasks_bp.route('/api/electricityconsumptionagent/download/gas', methods=['GET'])
def api_download_gas():
    # Check arguments (query parameters)
    if len(request.args) > 0:
        #print("Query parameters provided, although not required. " \
        #      + "Provided arguments will be neglected.")
        logger.warning("Query parameters provided, although not required. \
                        Provided arguments will be neglected.")
    try:
        # Download only
        read_from_web_gas()
        print(f'xlsx file for gas consumption in year {YEAR} successfully downloaded at current folder')
        return jsonify({"status": '200', "msg": "success"})

    except Exception as ex:
        print(ex)
        return jsonify({"status": '500', 'errormsg': 'Instantiation failed'})

# Define route for API request to download fuelpoverty data
@downloadtasks_bp.route('/api/electricityconsumptionagent/download/fuelpoverty', methods=['GET'])
def api_download_fuelpoverty():
    # Check arguments (query parameters)
    if len(request.args) > 0:
        #print("Query parameters provided, although not required. " \
        #      + "Provided arguments will be neglected.")
        logger.warning("Query parameters provided, although not required. \
                        Provided arguments will be neglected.")
    try:
        # Download only
        read_from_web_fuel_poverty()
        print(f'xlsx file for fuelpoverty in year {YEAR} successfully downloaded at current folder')
        return jsonify({"status": '200', "msg": "success"})

    except Exception as ex:
        print(ex)
        return jsonify({"status": '500', 'errormsg': 'Instantiation failed'})

# Define route for API request to download temperature data
@downloadtasks_bp.route('/api/electricityconsumptionagent/download/temperature', methods=['GET'])
def api_download_temperature():
    # Check arguments (query parameters)
    if len(request.args) > 0:
        #print("Query parameters provided, although not required. " \
        #      + "Provided arguments will be neglected.")
        logger.warning("Query parameters provided, although not required. \
                        Provided arguments will be neglected.")
    try:
        # Download only
        read_from_web_temp(var_name='tas')
        read_from_web_temp(var_name='tasmin')
        read_from_web_temp(var_name='tasmax')
        print(f'xlsx file for hadUK climate 1km grid data in year {YEAR} successfully downloaded at current folder')
        return jsonify({"status": '200', "msg": "success"})

    except Exception as ex:
        print(ex)
        return jsonify({"status": '500', 'errormsg': 'Instantiation failed'})