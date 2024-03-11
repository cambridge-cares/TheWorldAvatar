from flask import Flask, jsonify, request, json
from flask_cors import CORS

from .error_handling.exceptions import KGException, TSException
from .kg_utils.tsClientForUpdate import TSClientForUpdate
from .cluster_model import ClusterModel
from NTUEnergyClusterAgent.data_retrieval.query_data import QueryData
from NTUEnergyClusterAgent.data_retrieval.query_timeseries import query_latest_timeseries, query_all_timeseries
from NTUEnergyClusterAgent.data_instantiation.timeseries_instantiation import timeseries_instantiation

from NTUEnergyClusterAgent.kg_utils.utils import QUERY_ENDPOINT, UPDATE_ENDPOINT
from NTUEnergyClusterAgent.kg_utils.utils import DB_QUERY_URL, DB_QUERY_USER, DB_QUERY_PASSWORD, DB_UPDATE_URL, DB_UPDATE_USER, DB_UPDATE_PASSWORD

from NTUEnergyClusterAgent.models.buses import BUSES

from pathlib import Path
import os
import logging
import numpy as np

# Create the Flask app object
app = Flask(__name__)
CORS(app)

# Check whether it is running in a stack
def check_stack_status():
    if 'stack' in request.args:
        try:
            if str(request.args['stack']).lower() in ['true', '1', 't', 'y', 'yes']:
                logging.info("The stack parameter was set to true. Looking for stack blazegraph and RDB. ")
                global DB_QUERY_URL, DB_QUERY_USER, DB_QUERY_PASSWORD, DB_UPDATE_URL, DB_UPDATE_USER, DB_UPDATE_PASSWORD, QUERY_ENDPOINT, UPDATE_ENDPOINT
                from NTUEnergyClusterAgent.stack_utils.stack_configs import QUERY_ENDPOINT_STACK, UPDATE_ENDPOINT_STACK
                from NTUEnergyClusterAgent.stack_utils.stack_configs import DB_UPDATE_URL_STACK, DB_UPDATE_USER_STACK, DB_UPDATE_PASSWORD_STACK
                from NTUEnergyClusterAgent.stack_utils.stack_configs import DB_QUERY_URL_STACK, DB_QUERY_USER_STACK, DB_QUERY_PASSWORD_STACK
                DB_QUERY_URL = DB_QUERY_URL_STACK
                DB_QUERY_USER = DB_QUERY_USER_STACK
                DB_QUERY_PASSWORD = DB_QUERY_PASSWORD_STACK
                DB_UPDATE_URL = DB_UPDATE_URL_STACK
                DB_UPDATE_USER = DB_UPDATE_USER_STACK
                DB_UPDATE_PASSWORD = DB_UPDATE_PASSWORD_STACK
                QUERY_ENDPOINT = QUERY_ENDPOINT_STACK
                UPDATE_ENDPOINT = UPDATE_ENDPOINT_STACK
                logging.info("QUERY ENDPOINT: "+QUERY_ENDPOINT+" UPDATE_ENDPOINT: "+UPDATE_ENDPOINT)
            else:
                logging.info("The stack parameter was set to false. Looking for local blazegraph and RDB. ")
        except ValueError:
            logging.error("Unable to parse stack parameter.")
            return "Unable to interpret stack parameter ('%s') as a string." % request.args['stack']
    else:
        logging.error("Unable to parse stack parameter.")

@app.route('/')
def default():  
    check_stack_status()

    logging.info(os.getcwd())
    model = ClusterModel("/app/NTUEnergyClusterAgent/models/BNN_model.xlsx")

    all_P_values = []

    #iterate over controllable buses
    for bus_number in BUSES:

        busNode_iri_response = QueryData.query_busnode_iris(QUERY_ENDPOINT, UPDATE_ENDPOINT, bus_number)

        busNode_iri = busNode_iri_response[0]['busNode']

        logging.info("Getting busnode:" + busNode_iri)
        try:
            P_iri = QueryData.query_P_iri(busNode_iri, QUERY_ENDPOINT, UPDATE_ENDPOINT)
            
        except Exception as ex:
            logging.error("SPARQL query for P IRI for bus node ", busNode_iri," not successful.")
            raise KGException("SPARQL query for P IRI for bus node ", busNode_iri," not successful.") from ex

        try:
            # get latest
            P_ts = query_latest_timeseries(P_iri, QUERY_ENDPOINT, UPDATE_ENDPOINT, DB_QUERY_URL, DB_QUERY_USER, DB_QUERY_PASSWORD)
            
        except Exception as ex:
            logging.error("SPARQL query for P timeseries not successful.")
            raise KGException("SPARQL query for P timeseries not successful.") from ex

        try:
            P_dates = [d.toString() for d in P_ts.getTimes()]

        except Exception as ex:
            logging.error("Unable to get timestamps from timeseries object.")
            raise TSException("Unable to get timestamps from timeseries object") from ex
        
        P_values = [v for v in P_ts.getValues(P_iri)]

        all_P_values.append(P_values)

    try:
        probabilities = model.run_neural_net(all_P_values)
    except Exception as ex:
        raise Exception("Unable to run BNN") from ex

    #assign cluster membership
    membership = []
    size = np.shape(probabilities)

    for i in range(0, size[0]):
        bus_probabilities = probabilities[i,:]
        cluster = np.argmax(bus_probabilities)
        membership.append(int(cluster))

    logging.info("BNN successful. Membership: ")
    logging.info(membership)

    return jsonify(membership)