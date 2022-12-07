from flask import Flask, jsonify, request, json

from .error_handling.exceptions import KGException, TSException
from .kg_utils.tsClientForQuery import TSClientForQuery
from .kg_utils.tsClientForUpdate import TSClientForUpdate
from .solar_model import SolarModel
from PVLibAgent.data_retrieval.query_data import QueryData
from PVLibAgent.kg_utils.utils import IRI, DATACLASS, TIME_FORMAT, AIR_TEMP_IRI, WIND_SPEED_IRI, IRRADIANCE_IRI
from PVLibAgent.data_retrieval.query_timeseries import query_latest_timeseries
from PVLibAgent.data_instantiation.create_data_iris import check_data_iris
from PVLibAgent.data_instantiation.timeseries_instantiation import timeseries_instantiation

from pathlib import Path
import os
import logging

# Create the Flask app object
app = Flask(__name__)

# Define location of properties file (with Triple Store and RDB settings)
PROPERTIES_FILE = os.path.abspath(os.path.join(Path(__file__).parent, "resources", "dataIRIs.properties"))

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
    logging.basicConfig(level=logging.DEBUG)
    # Check arguments (query parameters)
    logging.info('Checking arguments...')

    if 'device' in request.args:
        try:
            device = str(request.args['device'])
        except ValueError:
            logging.error("Unable to parse device type.")
            return "Unable to interpret device type ('%s') as a string." % request.args['device']
    else:
        return "Error: No 'device' parameter provided."

    # Define location of properties file
    filepath = os.path.abspath(os.path.join(Path(__file__).parent, "resources", "model_parameters.properties"))

    if device.__contains__('weatherStation'):
        try:
            if AIR_TEMP_IRI == '' and WIND_SPEED_IRI == '' and IRRADIANCE_IRI == '':
                iri = IRI

                # Construct and evaluate the model
                model = SolarModel('ModelChain', iri, filepath)

                try:
                    air_temperature_iri = QueryData.query_air_temperature(iri)
                except Exception as ex:
                    logging.error("SPARQL query for air temperature IRI not successful")
                    raise KGException("SPARQL query for air temperature IRI not successful.") from ex

                try:
                    wind_speed_iri = QueryData.query_wind_speed(iri)
                except Exception as ex:
                    logging.error("SPARQL query for wind speed IRI not successful")
                    raise KGException("SPARQL query for wind speed IRI not successful.") from ex

                try:
                    ghi_iri = QueryData.query_global_horizontal_irradiance(iri)
                except Exception as ex:
                    logging.error("SPARQL query for global horizontal irradiance IRI not successful")
                    raise KGException("SPARQL query for global horizontal irradiance IRI not successful.") from ex

            else:
                iri = IRI

                # Construct and evaluate the model
                model = SolarModel('ModelChain', iri, filepath)

                wind_speed_iri = WIND_SPEED_IRI
                air_temperature_iri = AIR_TEMP_IRI
                ghi_iri = IRRADIANCE_IRI

            wind_speed = query_latest_timeseries(wind_speed_iri)
            air_temperature = query_latest_timeseries(air_temperature_iri)
            ghi = query_latest_timeseries(ghi_iri)
            try:
                wind_dates = [d.toString() for d in wind_speed.getTimes()]
                air_temperature_dates = [d.toString() for d in air_temperature.getTimes()]
                ghi_dates = [d.toString() for d in ghi.getTimes()]
            except Exception as ex:
                logging.error("Unable to get timestamps from timeseries object.")
                raise TSException("Unable to get timestamps from timeseries object") from ex
            if wind_dates != air_temperature_dates != ghi_dates:
                raise Exception('The timestamps for air temperature, wind speed and ghi are not the same!')

            try:
                wind_speed_values = [v for v in wind_speed.getValues(wind_speed_iri)]
                air_temperature_values = [v for v in air_temperature.getValues(air_temperature_iri)]
                ghi_values = [v for v in ghi.getValues(ghi_iri)]
            except Exception as ex:
                logging.error("Unable to get values from timeseries object.")
                raise TSException("Unable to get values from timeseries object.") from ex

            print(str(wind_speed_values[0]) + ' m/s at this timing: ' + wind_dates[0])
            print(str(air_temperature_values[0]) + ' degree celsius at this timing: ' + air_temperature_dates[0])
            print(str(ghi_values[0]) + ' W/m^2 at this timing: ' + ghi_dates[0])
            iri_list = check_data_iris.check_data_iris_and_create_if_not_exist(PROPERTIES_FILE)
            results = model.calculate(wind_dates[0], wind_speed_values[0], air_temperature_values[0], ghi_values[0])
            print(str(results))
            timestamp = results["timestamp"]
            # timestamp must have the format 2017-04-01T04:00:00Z
            timestamp_list = [wind_dates[0]]
            ac_power_value = results["AC Power(W)"]
            dc_power_value = results["DC Power(W)"]
            ac_power_list = [ac_power_value]
            dc_power_list = [dc_power_value]
            value_list = [ac_power_list, dc_power_list]
            print(ac_power_value)
            print(dc_power_value)

            try:
                boolean_value = timeseries_instantiation.check_data_has_timeseries(iri_list)
                if not boolean_value:
                    timeseries_instantiation.init_timeseries(iri_list, [DATACLASS, DATACLASS], TIME_FORMAT)
            except Exception as ex:
                logging.error("Unable to initialise timeseries")
                raise TSException("Unable to initialise timeseries") from ex
            timeseries_object = TSClientForUpdate.create_timeseries(timestamp_list, iri_list, value_list)
            timeseries_instantiation.add_timeseries_data(timeseries_object)
            return results
        except ValueError as ex:
            return str(ex)

    elif str(request.args['device']).__contains__('sensor'):
        try:
            if IRRADIANCE_IRI == '':
                iri = IRI
                # Construct and evaluate the model
                model = SolarModel('ModelChain', iri, filepath)
                try:
                    ghi_iri = QueryData.query_irradiance(iri)
                except Exception as ex:
                    logging.error("SPARQL query for irradiance IRI not successful")
                    raise KGException("SPARQL query for irradiance IRI not successful.") from ex

            else:
                iri = IRI

                # Construct and evaluate the model
                model = SolarModel('ModelChain', iri, filepath)
                ghi_iri = IRRADIANCE_IRI

            ghi = query_latest_timeseries(ghi_iri)
            try:
                ghi_dates = [d.toString() for d in ghi.getTimes()]
            except Exception as ex:
                logging.error("Unable to get timestamps from timeseries object.")
                raise TSException("Unable to get timestamps from timeseries object") from ex

            try:
                ghi_values = [v for v in ghi.getValues(ghi_iri)]
            except Exception as ex:
                logging.error("Unable to get values from timeseries object.")
                raise TSException("Unable to get values from timeseries object.") from ex

            print(str(ghi_values[0]) + ' W/m^2 at this timing: ' + ghi_dates[0])

            iri_list = check_data_iris.check_data_iris_and_create_if_not_exist(PROPERTIES_FILE)
            results = model.calculate(ghi_dates[0], '', '', ghi_values[0])
            timestamp = results["timestamp"]
            # timestamp must have the format 2017-04-01T04:00:00Z
            timestamp_list = [ghi_dates[0]]
            ac_power_value = results["AC Power(W)"]
            dc_power_value = results["DC Power(W)"]
            ac_power_list = [ac_power_value]
            dc_power_list = [dc_power_value]
            value_list = [ac_power_list, dc_power_list]
            print(str(timestamp))
            print(ac_power_value)
            print(dc_power_value)

            try:
                boolean_value = timeseries_instantiation.check_data_has_timeseries(iri_list)
                if not boolean_value:
                    timeseries_instantiation.init_timeseries(iri_list, [DATACLASS, DATACLASS], TIME_FORMAT)
            except Exception as ex:
                logging.error("Unable to initialise timeseries")
                raise TSException("Unable to initialise timeseries") from ex
            timeseries_object = TSClientForUpdate.create_timeseries(timestamp_list, iri_list, value_list)
            timeseries_instantiation.add_timeseries_data(timeseries_object)
            return results
        except ValueError as ex:
            return str(ex)

