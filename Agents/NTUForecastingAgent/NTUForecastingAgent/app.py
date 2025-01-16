from flask import Flask, jsonify, request, json

import os
import logging

from pyderivationagent.kg_operations import PySparqlClient, PyDerivationClient
from pyderivationagent.data_model.iris import ONTODERIVATION_DERIVATIONWITHTIMESERIES

from NTUForecastingAgent.kg_utils.tsclient import TSClient

from NTUForecastingAgent.stack_utils.stack_configs import UPDATE_ENDPOINT_STACK, DB_QUERY_URL_STACK, DB_QUERY_USER_STACK, DB_QUERY_PASSWORD_STACK

# Create the Flask app object
app = Flask(__name__)

# Are defined in docker-compose, should be the same as the ones in the docker-compose of the forecastingagent
derivation_instance_base_url = os.environ["DERIVATION_INSTANCE_BASE_URL"]
ontoagent_service_iri = os.environ["ONTOAGENT_SERVICE_IRI"]

kgclient = PySparqlClient(UPDATE_ENDPOINT_STACK, UPDATE_ENDPOINT_STACK)
ts_client = TSClient(kg_client=kgclient, rdb_url=DB_QUERY_URL_STACK, 
                        rdb_user=DB_QUERY_USER_STACK, rdb_password=DB_QUERY_PASSWORD_STACK)
deriv_client = PyDerivationClient(derivation_instance_base_url, 
                                    UPDATE_ENDPOINT_STACK, UPDATE_ENDPOINT_STACK)

# Show an instructional message at the app root
@app.route('/')
def default():
    # TODO: copypasted from pvlib, change to proper message
    msg  = "To see the result of an API call, enter a URL of the form:<BR>"
    msg += "&nbsp&nbsp [this_url]/api/v1/evaluate?val=[VAL]&order=[ORDER]<BR><BR>"
    msg += "&nbsp&nbsp (where [VAL] is a float and [ORDER] is an integer between 0 and 2)"
    msg += "&nbsp&nbsp [this_url] is the host and port currently shown in the address bar"
    return msg

# Forecast Request
@app.route('/api/v1/forecast', methods=['GET'])
def api():
    logging.basicConfig(level=logging.DEBUG)
    logging.info('request.args: %s' % request.args)

    logging.info('UPDATE_ENDPOINT: %s' % UPDATE_ENDPOINT_STACK)
    logging.info('DB_QUERY_URL: %s' % DB_QUERY_URL_STACK)
    logging.info('DB_QUERY_USER: %s' % DB_QUERY_USER_STACK)
    logging.info('DB_QUERY_PASSWORD: %s' % DB_QUERY_PASSWORD_STACK)

    # Arguments for forecasting

    IRI_frequency = request.args.get("frequency", type=str)
    IRI_forecasting_model = request.args.get("forecasting_model", type=str)
    IRI_interval = request.args.get("interval", type=str)
    IRI_duration = request.args.get("duration", type=str)
    IRI_to_forecast = request.args.get("iri_to_forecast", type=str)

    # It is necessary to know what to forecast and for which interval, default values can't be used for these
    if not IRI_to_forecast:
        logging.error("No IRI to forecast provided.")
        return "No IRI to forecast provided."

    # If no interval is given, use given time positions instead to add interval to KG
    if not IRI_interval:
        time_pos1 = request.args.get("time_pos1", type=float)
        time_pos2 = request.args.get("time_pos2", type=float)
        if not time_pos1 or not time_pos2:
            logging.error("No IRI for interval and no time_pos1 and time_pos2 provided.")
            return "No interval provided."
    
    # Add triples with default values if any other arguments are missing

    update = ''

    if not IRI_frequency:
        IRI_frequency = 'http://example.org/Frequency_1'
        update += f'''
        <{IRI_frequency}> rdf:type ts:Frequency ;
            time:numericDuration 1.0 ;
            time:unitType time:unitHour ;
            ts:resampleData "false"^^xsd:boolean .

        '''
    
    if not IRI_forecasting_model:
        IRI_forecasting_model = 'http://example.org/ForecastingModel_1'
        update += f'''
        <{IRI_forecasting_model}> rdf:type ts:ForecastingModel ;
            rdfs:label "Prophet" ;
            ts:scaleData "false"^^xsd:boolean .

        '''
    
    if not IRI_duration:
        IRI_duration = 'http://example.org/Duration_1'
        update += f'''
        <{IRI_duration}> rdf:type time:Duration ;
            time:numericDuration 336.0 ;
            time:unitType time:unitHour .

        '''
    
    # Interval with given time positions
    if not IRI_interval:
        interval_name = str(time_pos1) + '_' + str(time_pos2)
        IRI_interval = 'http://example.org/OptimisationInterval_' + interval_name
        update += f'''
        <{IRI_interval}> rdf:type time:Interval ;
            time:hasBeginning ex:OptimisationStartInstant_{interval_name} ;
            time:hasEnd ex:OptimisationEndInstant_{interval_name} .
        ex:OptimisationStartInstant_{interval_name} rdf:type time:Instant ;
            time:inTimePosition ex:TimePosition_{str(time_pos1)} .
        ex:OptimisationEndInstant_{interval_name} rdf:type time:Instant ;
            time:inTimePosition ex:TimePosition_{str(time_pos2)} .
        ex:TimePosition_{str(time_pos1)} rdf:type time:TimePosition ;
            time:hasTRS <http://dbpedia.org/resource/Unix_time> ;
            time:numericPosition {time_pos1} .
        ex:TimePosition_{str(time_pos2)} rdf:type time:TimePosition ;
            time:hasTRS <http://dbpedia.org/resource/Unix_time> ;
            time:numericPosition {time_pos2} .

        '''
    
    update = f'''
    PREFIX ex: <http://example.org/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> 
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    PREFIX time: <http://www.w3.org/2006/time#> 
    PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX ts: <https://www.theworldavatar.com/kg/ontotimeseries/>

    INSERT DATA {{
        {update}
    }}
    '''
    
    #Update triples for forecasting in KG
    kgclient.performUpdate(update)

    # Request derivation (i.e., forecast)
    derivation_input_set = [IRI_to_forecast, IRI_forecasting_model, IRI_frequency,
                            IRI_interval, IRI_duration]
    derivation = deriv_client.createSyncDerivationForNewInfo(ontoagent_service_iri, 
                                                                derivation_input_set, 
                                                                ONTODERIVATION_DERIVATIONWITHTIMESERIES)

    # Retrieve forecasted time series
    query = f'''
    PREFIX ex: <http://example.org/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ts: <https://www.theworldavatar.com/kg/ontotimeseries/>
    PREFIX deriv: <https://www.theworldavatar.com/kg/ontoderivation/>

    SELECT ?fc_iri
    WHERE {{
        <{IRI_to_forecast}> ts:hasForecast ?fc_iri .
        ?fc_iri ts:hasTimeSeries ?forecasted_ts ;
            rdf:type ts:Forecast ;
            deriv:belongsto ?deriv_with_ts .
        ?forecasted_ts rdf:type ts:TimeSeries .
        ?deriv_with_ts deriv:isDerivedFrom <{IRI_to_forecast}> ;
            deriv:isDerivedFrom <{IRI_forecasting_model}> ;
            deriv:isDerivedFrom <{IRI_frequency}> ;
            deriv:isDerivedFrom <{IRI_interval}> ;
            deriv:isDerivedFrom <{IRI_duration}> ;
            deriv:isDerivedUsing <{ontoagent_service_iri}> .
    }}
    ''' 
    res = kgclient.performQuery(query)

    if len(res) == 0:
        logging.error("No forecasted time series found.")
        return "No forecasted time series found."

    dataIRI = res[0]['fc_iri']

    return dataIRI
