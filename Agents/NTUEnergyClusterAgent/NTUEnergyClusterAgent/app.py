from flask import Flask, jsonify, request, json
from flask_cors import CORS

from .error_handling.exceptions import KGException, TSException
from .kg_utils.tsClientForUpdate import TSClientForUpdate
from .cluster_model import ClusterModel
from NTUEnergyClusterAgent.data_retrieval.query_data import QueryData
from NTUEnergyClusterAgent.data_retrieval.query_timeseries import query_latest_timeseries, query_all_timeseries, query_timeseries_within_bounds
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

    # caller can specify datetime to run clustering
    if 'datetime' in request.args:
        try:
            date = request.args['datetime']
            bool_latest = False
            logging.info("date time specified: "+str(date))
        except ValueError:
            logging.error("Unable to parse date.")
            return "Unable to parse date."
    else: 
        bool_latest = True

    all_P_values = []

    # caller can provide results from P2P trading (in which case datetime is not used), otherwise values will be retrieved from the KG
    if 'p2p' in request.args:
        try:
            arg = request.args['p2p']
            p2p_values = arg[1:-1]
            val1 = np.fromstring(p2p_values, sep=",")
            val2 = np.reshape(val1,(1,-1))
            np_p2p_values = np.transpose(val2)
            all_P_values = np_p2p_values.tolist()
            logging.info("Using P2P values: "+str(all_P_values))
            bool_query_kg = False
        except ValueError:
            logging.error("Unable to parse date.")
            return "Unable to parse date."
    else: 
        bool_query_kg = True
        logging.info("Query KG for data")
        if bool_latest: 
            logging.info("Using latest data")
    
    # caller can run clustering on OPF results instead of using the BNN. Voltage values are retrieved from the KG
    bool_opf = False
    if 'opf' in request.args:
        try:
            if str(request.args['opf']).lower() in ['true', '1', 't', 'y', 'yes']:
                bool_opf = True
        except ValueError:
            logging.error("Unable to parse opf value.")
            return "Unable to parse date." 


    if bool_opf:
    #### Get voltage levels from OPF resutls
        logging.info("Using results of OPF")

        membership = []
        #get bus iris
        BUSNODE_IRIS = QueryData.query_all_busnode_iris(QUERY_ENDPOINT, UPDATE_ENDPOINT)
        #loop over bus iris
        for busNode_iri in BUSNODE_IRIS:
            busNode_iri = busNode_iri['busNode']
            logging.info("running for busnode:" + busNode_iri)

        #get voltage time series
            try:
                Vm_iri = QueryData.query_Vm_iri(busNode_iri, QUERY_ENDPOINT, UPDATE_ENDPOINT)
            except Exception as ex:
                logging.error("SPARQL query for P or Q IRI for bus node ", busNode_iri," not successful.")
                raise KGException("SPARQL query for P or Q IRI for bus node ", busNode_iri," not successful.") from ex

            try:
                Vm_ts = query_timeseries_within_bounds(Vm_iri, QUERY_ENDPOINT, UPDATE_ENDPOINT, DB_QUERY_URL, DB_QUERY_USER, DB_QUERY_PASSWORD, date, date)
            except Exception as ex:
                logging.error("SPARQL query for P or Q timeseries not successful.")
                raise KGException("SPARQL query for P or Q timeseries not successful.") from ex
        
            Vm_values = [v for v in Vm_ts.getValues(Vm_iri)]

            #assign membership based on volatage
            logging.info("Vm: "+str(Vm_values[0]))
            if Vm_values[0] < 0.95:
                membership.append(0)
                logging.info("Membership: 0")
            elif Vm_values[0] > 1.05:
                membership.append(2)
                logging.info("Membership: 2")
            else:
                membership.append(1)
                logging.info("Membership: 1")
        
        #return membership
        logging.info("Clustering from OPF results successful. Membership: ")
        logging.info(membership)
        return jsonify(membership)

    else:
    #### Run BNN
        
        logging.info(os.getcwd())
        model = ClusterModel("/app/NTUEnergyClusterAgent/models/BNN_model.xlsx")   

        if bool_query_kg:
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
                    if bool_latest:
                        P_ts = query_latest_timeseries(P_iri, QUERY_ENDPOINT, UPDATE_ENDPOINT, DB_QUERY_URL, DB_QUERY_USER, DB_QUERY_PASSWORD)
                    else:
                        P_ts = query_timeseries_within_bounds(P_iri, QUERY_ENDPOINT, UPDATE_ENDPOINT, DB_QUERY_URL, DB_QUERY_USER, DB_QUERY_PASSWORD, date, date)

                except Exception as ex:
                    logging.error("SPARQL query for P timeseries not successful.")
                    raise KGException("SPARQL query for P timeseries not successful.") from ex
                
                P_values = [v for v in P_ts.getValues(P_iri)]

                all_P_values.append(P_values)

        logging.info(all_P_values)

        try:
            probabilities = model.run_neural_net(all_P_values)
        except Exception as ex:
            raise Exception("Unable to run BNN") from ex

        #assign cluster membership
        membership = []
        size = np.shape(probabilities)
        logging.info(probabilities)

        for i in range(0, size[0]):
            bus_probabilities = probabilities[i,:]
            cluster = np.argmax(bus_probabilities)
            membership.append(int(cluster))

        logging.info("BNN successful. Membership: ")
        logging.info(membership)

        return jsonify(membership)
