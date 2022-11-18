from flask import Flask, jsonify, request

from .error_handling.exceptions import KGException
from .solar_model import SolarModel
from PVLibAgent.data_retrieval.query_data import QueryData
from PVLibAgent.kg_utils.utils import IRI
from PVLibAgent.data_retrieval.query_timeseries import query_latest_timeseries

import agentlogging

# Create the Flask app object
app = Flask(__name__)

# Initialise logger
logger = agentlogging.get_logger("dev")

# Show an instructional message at the app root
@app.route('/')
def default():
    msg  = "To see the result of an API call, enter a URL of the form:<BR>"
    msg += "&nbsp&nbsp [this_url]/api/v1/evaluate?val=[VAL]&order=[ORDER]<BR><BR>"
    msg += "&nbsp&nbsp (where [VAL] is a float and [ORDER] is an integer between 0 and 2)"
    msg += "&nbsp&nbsp [this_url] is the host and port currently shown in the address bar"
    return msg

# Define a route for API requests
@app.route('/api/v1/evaluate', methods=['GET'])
def api():
    
    # Check arguments (query parameters)
    logger.info("Checking arguments...")
    if 'modelType' in request.args:
        try:
            modeltype = str(request.args['modelType'])
        except ValueError:
            logger.error("Unable to parse model type.")
            return "Unable to interpret model type ('%s') as a string." % request.args['modelType']
    else:
        return "Error: No 'modelType' parameter provided."

    try:
        iri = IRI
        try:
            latitude_value = QueryData.query_latitude(iri)
        except Exception as ex:
            logger.error("SPARQL query for latitude not successful")
            raise KGException("SPARQL query for latitude not successful.") from ex

        try:
            longitude_value = QueryData.query_longitude(iri)
        except Exception as ex:
            logger.error("SPARQL query for longitude not successful")
            raise KGException("SPARQL query for longitude not successful.") from ex

        # Construct and evaluate the model
        model = SolarModel(modeltype, latitude_value, longitude_value)

        try:
            air_temperature_iri = QueryData.query_air_temperature(iri)
        except Exception as ex:
            logger.error("SPARQL query for air temperature IRI not successful")
            raise KGException("SPARQL query for air temperature IRI not successful.") from ex

        try:
            wind_speed_iri = QueryData.query_wind_speed(iri)
        except Exception as ex:
            logger.error("SPARQL query for wind speed IRI not successful")
            raise KGException("SPARQL query for wind speed IRI not successful.") from ex

        try:
            ghi_iri = QueryData.query_global_horizontal_irradiance(iri)
        except Exception as ex:
            logger.error("SPARQL query for global horizontal irradiance IRI not successful")
            raise KGException("SPARQL query for global horizontal irradiance IRI not successful.") from ex

        print(wind_speed_iri)
        print(air_temperature_iri)
        print(ghi_iri)
        print(latitude_value)
        print(longitude_value)

        wind_speed = query_latest_timeseries(wind_speed_iri)


        # air_temperature = query_latest_timeseries(air_temperature_iri)
        # ghi = query_latest_timeseries(ghi_iri)

        print(type(wind_speed))
        # print(air_temperature)
        # print(ghi)

        json_object = model.calculate(latitude_value, longitude_value)
        # json_object is fed to another .py class for instantiation as timeseries
        # utils.create_blazegraph_namespace()
        # Return the result in JSON format
        return json_object

    except ValueError as ex:
        return str(ex)
