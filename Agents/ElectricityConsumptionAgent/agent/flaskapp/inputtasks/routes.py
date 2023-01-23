################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk) #
# Date: 30/11 2022                            #
################################################

from flask import Blueprint, request, jsonify

import agentlogging
from agent.datainstantiation.readings import upload_all, upload_elec_data_to_KG, upload_gas_data_to_KG, upload_fuel_poverty_to_KG, upload_Geoinfo_to_KG, upload_hadUK_climate_to_KG
from agent.utils.CEDA_env_config import record_login_info
# Initialise logger
logger = agentlogging.get_logger("prod")


inputtasks_bp = Blueprint(
    'inputtasks_bp', __name__
)
# Define route for API request to download and instantiate all Electricity Consumption/meter data to KG (GET request)
@inputtasks_bp.route('/api/lsoainputagent/prerequisite/login', methods=['GET'])
def api_login():
    # Check arguments (query parameters)
    if len(request.args) > 0:
        #print("Query parameters provided, although not required. " \
        #      + "Provided arguments will be neglected.")
        logger.warning("Query parameters provided, although not required. \
                        Provided arguments will be neglected.")
    try:
        record_login_info()
        return jsonify({"status": '200', "msg": "success"})

    except Exception as ex:
        print(ex)
        return jsonify({"status": '500', 'errormsg': 'Instantiation failed'})

# Define route for API request to download and instantiate all Electricity Consumption/meter data to KG (GET request)
@inputtasks_bp.route('/api/lsoainputagent/instantiate/electricity', methods=['GET'])
def api_instantiate_electricity():
    # Check arguments (query parameters)
    if len(request.args) > 0:
        #print("Query parameters provided, although not required. " \
        #      + "Provided arguments will be neglected.")
        logger.warning("Query parameters provided, although not required. \
                        Provided arguments will be neglected.")
    try:
        # Download and instantiate
        num_elec = upload_elec_data_to_KG()
        print(f'Number of LSOA area with instantiated Electricity consumption/meters :{num_elec}')
        return jsonify({'LSOA_with_electricity':num_elec})

    except Exception as ex:
        print(ex)
        return jsonify({"status": '500', 'errormsg': 'Instantiation failed'})

# Define route for API request to download and instantiate all UK subregional (LSOA) Gas Consumption/meter/nonmeter data
@inputtasks_bp.route('/api/lsoainputagent/instantiate/gas', methods=['GET'])
def api_instantiate_gas():
    # Check arguments (query parameters)
    if len(request.args) > 0:
        #print("Query parameters provided, although not required. " \
        #      + "Provided arguments will be neglected.")
        logger.warning("Query parameters provided, although not required. \
                        Provided arguments will be neglected.")
    try:
        # Download and instantiate
        num_gas = upload_gas_data_to_KG()
        print(f'Number of LSOA area with instantiated Gas consumption/meters/nonmeters :{num_gas}')
        return jsonify({'LSOA_with_gas':num_gas})

    except Exception as ex:
        print(ex)
        return jsonify({"status": '500', 'errormsg': 'Instantiation failed'})

# Define route for API request to download and instantiate all UK subregional (LSOA) fuel poverty data
@inputtasks_bp.route('/api/lsoainputagent/instantiate/fuelpoverty', methods=['GET'])
def api_instantiate_fuelpoverty():
    # Check arguments (query parameters)
    if len(request.args) > 0:
        #print("Query parameters provided, although not required. " \
        #      + "Provided arguments will be neglected.")
        logger.warning("Query parameters provided, although not required. \
                        Provided arguments will be neglected.")
    try:
        # Download and instantiate
        num_fuelpoor = upload_fuel_poverty_to_KG()
        print(f'Number of LSOA area with instantiated Fuel Poverty :{num_fuelpoor}')
        return jsonify({'LSOA_with_fuelpoverty':num_fuelpoor})

    except Exception as ex:
        print(ex)
        return jsonify({"status": '500', 'errormsg': 'Instantiation failed'})

# Define route for API request to download and instantiate all hadUK climate data in 1km grid
@inputtasks_bp.route('/api/lsoainputagent/instantiate/temperature', methods=['GET'])
def api_instantiate_temperature():
    # Check arguments (query parameters)
    if len(request.args) > 0:
        #print("Query parameters provided, although not required. " \
        #      + "Provided arguments will be neglected.")
        logger.warning("Query parameters provided, although not required. \
                        Provided arguments will be neglected.")
    try:
        # Download and instantiate
        num_temp = upload_hadUK_climate_to_KG()
        print(f'Number of LSOA area with instantiated hadUK climate data :{num_temp}')
        return jsonify({'LSOA_with_climate':num_temp})

    except Exception as ex:
        print(ex)
        return jsonify({"status": '500', 'errormsg': 'Instantiation failed'})

# Define route for API request to instantiate all LSOA geometric shape
@inputtasks_bp.route('/api/lsoainputagent/instantiate/shape', methods=['GET'])
def api_instantiate_shape():
    # Check arguments (query parameters)
    if len(request.args) > 0:
        #print("Query parameters provided, although not required. " \
        #      + "Provided arguments will be neglected.")
        logger.warning("Query parameters provided, although not required. \
                        Provided arguments will be neglected.")
    try:
        # Instantiate
        num_shape = upload_Geoinfo_to_KG()
        print(f'Number of LSOA area with instantiated geometric shape data :{num_shape}')
        return jsonify({'LSOA_with_shape':num_shape})

    except Exception as ex:
        print(ex)
        return jsonify({"status": '500', 'errormsg': 'Instantiation failed'})

# Define route for API request to download and instantiate all data mentioned above
@inputtasks_bp.route('/api/lsoainputagent/instantiate/all', methods=['GET'])
def api_instantiate_all():
    # Check arguments (query parameters)
    if len(request.args) > 0:
        #print("Query parameters provided, although not required. " \
        #      + "Provided arguments will be neglected.")
        logger.warning("Query parameters provided, although not required. \
                        Provided arguments will be neglected.")
    try:
        # Download and instantiate
        num_elec, num_gas, num_shape, num_fuelpoor, num_temp = upload_all()
        print(f'Number of LSOA area with instantiated Electricity consumption/meters :{num_elec}')
        print(f'Number of LSOA area with instantiated Gas consumption/meters/nonmeters :{num_gas}')
        print(f'Number of LSOA area with instantiated Shape data :{num_shape}')
        print(f'Number of LSOA area with instantiated Fuel Poverty :{num_fuelpoor}')
        print(f'Number of LSOA area with instantiated hadUK climate data :{num_temp}')
        return jsonify({'LSOA_with_electricity':num_elec,'LSOA_with_gas':num_gas,'LSOA_with_fuelpoverty':num_fuelpoor,'LSOA_with_climate':num_temp, 'LSOA_with_shape':num_shape})

    except Exception as ex:
        print(ex)
        return jsonify({"status": '500', 'errormsg': 'Instantiation failed'})
