from flask import Flask, jsonify, request, json
from flask_cors import CORS

from .error_handling.exceptions import KGException, TSException
from .kg_utils.tsClientForUpdate import TSClientForUpdate

from NTUDataAgent.data_retrieval.query_data import QueryData
from NTUDataAgent.kg_utils.utils import IRI, DATACLASS, TIME_FORMAT, AIR_TEMP_IRI, WIND_SPEED_IRI, IRRADIANCE_IRI
from NTUDataAgent.data_retrieval.query_timeseries import query_latest_timeseries, query_all_timeseries, query_timeseries_within_bounds
from NTUDataAgent.data_instantiation.create_data_iris import check_data_iris
from NTUDataAgent.data_instantiation.timeseries_instantiation import timeseries_instantiation

from NTUDataAgent.kg_utils.utils import QUERY_ENDPOINT, UPDATE_ENDPOINT
from NTUDataAgent.kg_utils.utils import DB_QUERY_URL, DB_QUERY_USER, DB_QUERY_PASSWORD, DB_UPDATE_URL, DB_UPDATE_USER, DB_UPDATE_PASSWORD

from NTUDataAgent.config.buses import BUSES

from pathlib import Path
import os
import logging
import datetime

class TZ(datetime.tzinfo):
    def utcoffset(self, dt):
        return datetime.timedelta(hours=8)
    def tzname(self, dt):
        return "Singapore"
    def dst(self, dt):
        return datetime.timedelta(0)
    
class UTC(datetime.tzinfo):
    def utcoffset(self, dt):
        return datetime.timedelta(hours=0)
    def tzname(self, dt):
        return "UTC"
    def dst(self, dt):
        return datetime.timedelta(0)
    
# Create the Flask app object
app = Flask(__name__)
CORS(app)

# Check whether it is running in a stack
def check_stack_status(request_arg):
    if 'stack' in request.args:
        try:
            if str(request.args['stack']).lower() in ['true', '1', 't', 'y', 'yes']:
                global DB_QUERY_URL, DB_QUERY_USER, DB_QUERY_PASSWORD, DB_UPDATE_URL, DB_UPDATE_USER, DB_UPDATE_PASSWORD, QUERY_ENDPOINT, UPDATE_ENDPOINT
                from NTUDataAgent.stack_utils.stack_configs import QUERY_ENDPOINT_STACK, UPDATE_ENDPOINT_STACK
                from NTUDataAgent.stack_utils.stack_configs import DB_UPDATE_URL_STACK, DB_UPDATE_USER_STACK, DB_UPDATE_PASSWORD_STACK
                from NTUDataAgent.stack_utils.stack_configs import DB_QUERY_URL_STACK, DB_QUERY_USER_STACK, DB_QUERY_PASSWORD_STACK
                DB_QUERY_URL = DB_QUERY_URL_STACK
                DB_QUERY_USER = DB_QUERY_USER_STACK
                DB_QUERY_PASSWORD = DB_QUERY_PASSWORD_STACK
                DB_UPDATE_URL = DB_UPDATE_URL_STACK
                DB_UPDATE_USER = DB_UPDATE_USER_STACK
                DB_UPDATE_PASSWORD = DB_UPDATE_PASSWORD_STACK
                QUERY_ENDPOINT = QUERY_ENDPOINT_STACK
                UPDATE_ENDPOINT = UPDATE_ENDPOINT_STACK
            else:
                logging.info("The stack parameter was set to false. Looking for local blazegraph and RDB. ")
        except ValueError:
            logging.error("Unable to parse stack parameter.")
            return "Unable to interpret stack parameter ('%s') as a string." % request.args['stack']
    else:
        logging.error("Unable to parse stack parameter.")

# Define a route for API requests
@app.route('/generate', methods=['GET'])
def api():

    #mapping of dates to use 
    hist_lower_bounds = {0:'2020-12-01T00:00:00Z',
                        1:'2020-09-01T00:00:00Z',
                        2:'2020-04-01T00:00:00Z',
                        3:'2020-10-01T00:00:00Z',
                        4:'2020-11-01T00:00:00Z',
                        5:'2020-02-01T00:00:00Z',
                        6:'2020-03-01T00:00:00Z'}
    
    hist_upper_bounds = {0:'2020-12-01T23:00:00Z',
                        1:'2020-09-01T23:00:00Z',
                        2:'2020-04-01T23:00:00Z',
                        3:'2020-10-01T23:00:00Z',
                        4:'2020-11-01T23:00:00Z',
                        5:'2020-02-01T23:00:00Z',
                        6:'2020-03-01T23:00:00Z'}


    logging.basicConfig(level=logging.DEBUG)
    # Check arguments (query parameters)
    logging.info('Checking arguments...')
    logging.info('request.args: %s' % request.args)
    check_stack_status(request.args)
    logging.info('QUERY_ENDPOINT: %s' % QUERY_ENDPOINT)
    logging.info('UPDATE_ENDPOINT: %s' % UPDATE_ENDPOINT)
    logging.info('DB_QUERY_URL: %s' % DB_QUERY_URL)
    logging.info('DB_QUERY_USER: %s' % DB_QUERY_USER)
    logging.info('DB_QUERY_PASSWORD: %s' % DB_QUERY_PASSWORD)


    try:
        date = request.args['date']
        logging.info("date: "+str(date))
        datetime_date = datetime.date.fromisoformat(date)
        weekday = datetime_date.weekday()
        lower_bound=datetime.datetime(datetime_date.year, datetime_date.month, datetime_date.day, 0, 0, tzinfo=TZ())
        upper_bound=datetime.datetime(datetime_date.year, datetime_date.month, datetime_date.day, 23, 0, tzinfo=TZ())
        logging.info("Start: "+str(lower_bound))
        logging.info("End: "+str(upper_bound))
    except ValueError:
        logging.error("Unable to parse date.")
        return "Unable to parse date."

    #select historical dates
    hist_lower_bound = hist_lower_bounds[weekday]
    hist_upper_bound = hist_upper_bounds[weekday]
    logging.info("Historical lower bound: " + hist_lower_bound)
    logging.info("Historical upper bound: " + hist_upper_bound)

    #### generate synthetic load data for each bus
    for bus in BUSES:

        iri_list = []

        logging.info("Bus: "+str(bus))

        #get bus node iri
        bus_node_iri = QueryData.query_busnode_iri(QUERY_ENDPOINT, UPDATE_ENDPOINT, bus)

        #get data iris
        p_data_iri = QueryData.query_P_iri(bus_node_iri, QUERY_ENDPOINT, UPDATE_ENDPOINT)
        q_data_iri = QueryData.query_Q_iri(bus_node_iri, QUERY_ENDPOINT, UPDATE_ENDPOINT)
        
        #get historical time series for example dates, add noise
        try:
            p_data_hist = query_timeseries_within_bounds(p_data_iri, QUERY_ENDPOINT, UPDATE_ENDPOINT, DB_QUERY_URL, DB_QUERY_USER, DB_QUERY_PASSWORD, hist_lower_bound, hist_upper_bound)
            q_data_hist = query_timeseries_within_bounds(q_data_iri, QUERY_ENDPOINT, UPDATE_ENDPOINT, DB_QUERY_URL, DB_QUERY_USER, DB_QUERY_PASSWORD, hist_lower_bound, hist_upper_bound)
        except Exception as ex:
            logging.error("Unable to get power timeseries object.")
            raise TSException("Unable to get power timeseries object.") from ex
        
        logging.info("Timeseries length: "+ str(len(p_data_hist.getTimes())))

        p_data_hist_values = [v for v in p_data_hist.getValues(p_data_iri)]
        q_data_hist_values = [v for v in q_data_hist.getValues(q_data_iri)]
        
        iri_list = [p_data_iri, q_data_iri]
        value_list = [p_data_hist_values, q_data_hist_values]
        logging.info("iri_list: " + str(iri_list))
        logging.info("value_list: " + str(value_list))

        #create new time series lower and upper bounds
        timestamps = []
        date = lower_bound
        for i in range(len(p_data_hist_values)):
            timestamps.append(date.astimezone(tz=UTC()).strftime(TIME_FORMAT))
            date += datetime.timedelta(hours=1)

        logging.info("timestamps: " + str(len(timestamps)))
        logging.info("timestamp_list: " + str(timestamps))
  
        try:
            timeseries_object = TSClientForUpdate.create_timeseries(timestamps, iri_list, value_list)
            timeseries_instantiation.add_timeseries_data(timeseries_object, QUERY_ENDPOINT, UPDATE_ENDPOINT, DB_UPDATE_URL, DB_UPDATE_USER, DB_UPDATE_PASSWORD)
        except Exception as ex:
            logging.error("Unable to add timeseries")
            raise TSException("Unable to add timeseries") from ex
        
    return "Success!"