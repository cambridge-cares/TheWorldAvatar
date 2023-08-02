#################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk)  #
#          Magnus Mueller (mm2692@cam.ac.uk)    #
# Date: 25 Jul 2023                             #
#################################################

# The purpose of this module is to test the Agent Flask App. HTTP requests are sent 
# to the Flask App and the response is checked.

import pytest
import json
import requests
from pathlib import Path
from rdflib import Graph
from rdflib import RDF
from operator import eq, gt

from py4jps import agentlogging

import forecastingagent.datamodel as dm
from forecastingagent.agent.forcasting_config import DOUBLE, TIME_FORMAT

from . import conftest as cf


# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


def test_example_triples():
    """
    This test checks that the example triples are correct in syntax.

    Raises:
        e: If the example triples are not valid RDF.
    """
    g = Graph()
    pathlist = Path(cf.TEST_TRIPLES_DIR).glob('*.ttl')
    for path in pathlist:
        try:
            g.parse(str(path))
        except Exception as e:
            raise e


def test_example_data_instantiation(initialise_clients):
    """
    This test checks that all example data gets correctly instantiated,
    including associated time series data in PostgreSQL.
    """
    # Get required clients from fixture
    sparql_client, ts_client, _, rdb_url = initialise_clients

    ### TRIPPLE STORE ###
    # Verify that KG is empty
    assert sparql_client.getAmountOfTriples() == 0

    # Upload example test triples
    cf.initialise_triples(sparql_client)

    # Verify instantiation of expected number of triples
    triples = cf.TBOX_TRIPLES + cf.ABOX_TRIPLES
    assert sparql_client.getAmountOfTriples() == triples

    ### POSTGRESQL ###
    # Verify that Postgres database is empty
    assert cf.get_number_of_rdb_tables(rdb_url) == 0

    # Initialise and upload time series
    ts_client.init_timeseries(dataIRI=cf.IRI_TO_FORECAST_1,
                              times=cf.TIMES, values=cf.VALUES_1,
                              ts_type=DOUBLE, time_format=TIME_FORMAT)

    # Verify that expected tables and triples are created (i.e. dbTable + 1 ts table)
    assert cf.get_number_of_rdb_tables(rdb_url) == 2
    assert sparql_client.getAmountOfTriples() == (triples + cf.TS_TRIPLES)

    # Verify correct retrieval of time series data
    times, values = ts_client.retrieve_timeseries(cf.IRI_TO_FORECAST_1)
    assert times == cf.TIMES
    # Account for rounding errors
    assert pytest.approx(values, rel=1e-5) == cf.VALUES_1

    # Verify that dropping all tables works as expected
    cf.clear_database(rdb_url)
    assert cf.get_number_of_rdb_tables(rdb_url) == 0


@pytest.mark.parametrize(
    "derivation_input_set, iri_to_forecast, ts_times, ts_values",
    [
        (cf.DERIVATION_INPUTS_1, cf.IRI_TO_FORECAST_1, cf.TIMES, cf.VALUES_1),
        (cf.DERIVATION_INPUTS_2, cf.IRI_TO_FORECAST_2, cf.TIMES, cf.VALUES_3)
    ],
)
def test_create_forecast(
    initialise_clients, create_example_agent, derivation_input_set, iri_to_forecast,
    ts_times, ts_values
):
    """
    Test if forecasting agent performs derivation update as expected
    """

    # Get required clients from fixture
    sparql_client, ts_client, derivation_client, rdb_url = initialise_clients

    # Initialise all triples in test_triples + initialise time series in RDB
    # (it first DELETES ALL DATA in the specified SPARQL/RDB endpoints)
    cf.initialise_triples(sparql_client)
    cf.clear_database(rdb_url)
    ts_client.init_timeseries(dataIRI=iri_to_forecast,
                              times=ts_times, values=ts_values,
                              ts_type=DOUBLE, time_format=TIME_FORMAT)

    # Verify correct number of triples (not marked up with timestamp yet)
    triples = cf.TBOX_TRIPLES + cf.ABOX_TRIPLES + cf.TS_TRIPLES
    assert sparql_client.getAmountOfTriples() == triples

    # Create agent instance and register agent in KG
    # - Successful agent registration within the KG is required to pick up marked up derivations
    # - Hence, the Dockerised agent is started without initial registration within the Stack and
    #   registration is done within the test to guarantee that test Blazegraph will be ready
    # - The "belated" registration of the Dockerised agent can be achieved by registering "another"
    #   agent instance with the same ONTOAGENT_SERVICE_IRI
    agent = create_example_agent(random_agent_iri=True)

    # Verify expected number of triples after derivation registration
    triples += cf.AGENT_SERVICE_TRIPLES
    triples += cf.DERIV_INPUT_TRIPLES + cf.DERIV_OUTPUT_TRIPLES
    assert sparql_client.getAmountOfTriples() == triples

    # Assert that there's currently no instance having rdf:type of the output signature in the KG
    assert not sparql_client.check_if_triple_exist(None, RDF.type.toPython(), dm.TS_FORECAST)

    # Create derivation instance for new information (incl. timestamps for pure inputs)
    derivation = derivation_client.createSyncDerivationForNewInfo(agent.agentIRI, derivation_input_set,
                                                                  cf.ONTODERIVATION_DERIVATIONWITHTIMESERIES)
    # derivation_iri = derivation.getIri()
    # print(f"Initialised successfully, created synchronous derivation instance: {derivation_iri}")
    
    # # Verify expected number of triples after derivation registration
    # triples += cf.TIME_TRIPLES_PER_PURE_INPUT * len(derivation_input_set) # timestamps for pure inputs
    # triples += cf.TIME_TRIPLES_PER_PURE_INPUT                             # timestamps for derivation instance
    # triples += len(derivation_input_set) + 2    # number of inputs + derivation type + associated agent 
    # assert sparql_client.getAmountOfTriples() == triples

    # # Query timestamp of the derivation for every 20 seconds until it's updated
    # currentTimestamp_derivation = 0
    # while currentTimestamp_derivation == 0:
    #     time.sleep(10)
    #     currentTimestamp_derivation = cf.get_timestamp(derivation_iri, sparql_client)

    # # Query the output of the derivation instance
    # derivation_outputs = cf.get_derivation_outputs(derivation_iri, sparql_client)
    # print(f"Generated derivation outputs that belongsTo the derivation instance: {', '.join(derivation_outputs)}")
    
    # # Verify that there are 2 derivation outputs (i.e. AveragePrice and Measure IRIs)
    # assert len(derivation_outputs) == 2
    # assert dm.OBE_AVERAGE_SM_PRICE in derivation_outputs
    # assert len(derivation_outputs[dm.OBE_AVERAGE_SM_PRICE]) == 1
    # assert dm.OM_MEASURE in derivation_outputs
    # assert len(derivation_outputs[dm.OM_MEASURE]) == 1
    
    # # Verify the values of the derivation output
    # avg_iri = derivation_outputs[dm.OBE_AVERAGE_SM_PRICE][0]
    # inputs, postcode, price = cf.get_avgsqmprice_details(sparql_client, avg_iri)
    # # Verify postcode
    # assert len(postcode) == 1
    # assert postcode[0] == expected_postcode
    # # Verify price
    # assert len(price) == 1
    # assert price[0] == expected_avg

    # # Verify inputs (i.e. derived from)
    # # Create deeepcopy to avoid modifying original cf.DERIVATION_INPUTS_... between tests
    # derivation_input_set_copy = copy.deepcopy(derivation_input_set)
    # for i in inputs:
    #     for j in inputs[i]:
    #         assert j in derivation_input_set_copy
    #         derivation_input_set_copy.remove(j)
    # assert len(derivation_input_set_copy) == 0

    print("All check passed.")


@pytest.mark.parametrize(
    "http_request, fail, equal, expected_result",
    [
        (cf.ERROR_REQUEST, False, True, eq),
        (cf.ERROR_REQUEST, False, False, gt),
        (cf.ERRONEOUS_ERROR_REQUEST_1, True, None, cf.ERRONEOUS_ERROR_MSG_1),
        (cf.ERRONEOUS_ERROR_REQUEST_2, True, None, cf.ERRONEOUS_ERROR_MSG_2),
        (cf.ERRONEOUS_ERROR_REQUEST_3, True, None, cf.ERRONEOUS_ERROR_MSG_3),
    ],
)
def test_evaluate_forecast(
    initialise_clients, http_request, fail, equal, expected_result
):
    """
    Test if forecast errors are evaluated as expected

    Boolean flags:
        - fail: True if the test is expected to fail
        - equal: True if time series is compared with itself
    """

    # Get required clients from fixture
    sparql_client, ts_client, _, rdb_url = initialise_clients

    # Initialise all triples in test_triples + initialise time series in RDB
    cf.initialise_triples(sparql_client)
    cf.clear_database(rdb_url)
    ts_client.init_timeseries(dataIRI=cf.IRI_TO_FORECAST_1,
                              times=cf.TIMES, values=cf.VALUES_1,
                              ts_type=DOUBLE, time_format=TIME_FORMAT)
    ts_client.init_timeseries(dataIRI=cf.IRI_TO_FORECAST_2,
                              times=cf.TIMES, values=cf.VALUES_2,
                              ts_type=DOUBLE, time_format=TIME_FORMAT)

    if not fail:
        # Retrieve list of instantiated time series IRIs
        tsIRIs = sparql_client.get_all_tsIRIs()
        if equal:
            http_request['query']['tsIRI_target'] = tsIRIs[0]
            http_request['query']['tsIRI_fc'] = tsIRIs[0]
        else:
            http_request['query']['tsIRI_target'] = tsIRIs[0]
            http_request['query']['tsIRI_fc'] = tsIRIs[1]

    # Create HTTP request to evaluate forecast errors
    headers = {'Content-Type': 'application/json'}
    url = cf.AGENT_BASE_URL + '/evaluate_errors'
    response = requests.post(url, json=http_request, headers=headers)

    if fail:
        # Verify that correct error message is returned for erroneous requests
        assert response.status_code == 500
        assert expected_result in response.text

    else:
        # Check successful execution/response
        assert response.status_code == 200
        response = response.json()
        assert expected_result(response['mape'], 0)
        assert expected_result(response['smape'], 0)
        assert expected_result(response['mse'], 0)
        assert expected_result(response['rmse'], 0)
        assert expected_result(response['max_error'], 0)

        


# def test_load_pretrained_model():
#      """
#      Test the function `load_pretrained_model` to load a pretrained model from a
#      checkpoint file or a PyTorch model file
#      NOTE: Test will fail if the model is not available at the given link
#      """
#      # Test if pretrained model is loaded correctly
#      cfg = {
#           'model_configuration_name': 'test_model',
#           'fc_model': {
#                'name': 'TFTModel_test',
#                'model_path_ckpt_link': cf.DARTS_MODEL_OBJECT,
#                'model_path_pth_link': cf.DARTS_CHECKPOINTS,
#           },
#      }
#      model = load_pretrained_model(cfg, TFTModel, force_download=True)    
#      assert model.__class__.__name__ == 'TFTModel'
#      assert model.model.input_chunk_length == 168
#      assert model.model.output_chunk_length == 24
     
#      # use previously downloaded model
#      model = load_pretrained_model(cfg, TFTModel, force_download=False)    
#      assert model.__class__.__name__ == 'TFTModel'
#      assert model.model.input_chunk_length == 168
#      assert model.model.output_chunk_length == 24


# @pytest.mark.parametrize(
#     "query_dict, expected", 
#     [
#         (cf.query1, cf.expected2),
#         (cf.query2, cf.expected2)
#     ]
# )
# def test_prophet(query_dict, expected, test_client, initialise_clients):

#     # Get SPARQL client from fixture
#     kg_client, ts_client, rdb_url = initialise_clients
#     # Create clean slate for test and initialise data as required
#     cf.initialise_prophet(kg_client, ts_client, rdb_url)

#     # Send request to flask app
#     response = test_client.post('/forecast', 
#                                 json={"headers": {'content_type': 'application/json'},
#                                 **query_dict
#                                 }).json
#     # Check successful execution/response
#     assert response['status'] == str(200)
    
#     # Check that forecast is instantiated
#     dates, values = get_ts_data(
#         response['forecast_iri'], ts_client)
#     assert len(dates) == expected['horizon']
#     assert len(values) == expected['horizon']
#     assert dates[0] == expected['forecast_start_date']
#     assert response['fc_model'] == expected['fc_model']
#     assert response['data_length'] == expected['data_length']
#     assert response['model_configuration_name'] == expected['model_configuration_name']
#     assert response['iri'] == expected['iri']
#     assert response['horizon'] == expected['horizon']
#     assert response['model_input_interval'] == expected['model_input_interval']
#     assert response['model_output_interval'] == expected['model_output_interval']
#     assert response['unit'] == expected['unit']


# @pytest.mark.parametrize(
#     "query_dict, expected_error_message", 
#     [
#         (cf.query_error1, cf.expected_error1),
#         (cf.query_error2, cf.expected_error2),
#         (cf.query_error3, cf.expected_error3),
#         (cf.query_error4, cf.expected_error4),
#         (cf.query_error5, cf.expected_error5),
#         (cf.query_error6, cf.expected_error6),
#         (cf.query_error7, cf.expected_error7)
#     ]
# )
# def test_prophet_error(query_dict, expected_error_message, test_client, initialise_clients):

#     # Get SPARQL client from fixture
#     kg_client, ts_client, rdb_url = initialise_clients
#     # Create clean slate for test and initialise data as required
#     cf.initialise_prophet(kg_client, ts_client, rdb_url)

#     res = test_client.post('/forecast',
#                            json={"headers": {'content_type': 'application/json'},
#                            **query_dict
#                            })

#     assert res.status_code == 500
#     assert expected_error_message in res.json['msg']


# @pytest.mark.parametrize(
#     "query_dict, expected", 
#     [
#         (cf.query3, cf.expected3),
#     ]
# )
# def test_tft(query_dict, expected, test_client, initialise_clients):

#     # Get SPARQL client from fixture
#     kg_client, ts_client, rdb_url = initialise_clients
#     # Create clean slate for test and initialise data as required
#     cf.initialise_tft(kg_client, ts_client, rdb_url)

#     res = test_client.post('/forecast', 
#                            json={"headers": {'content_type': 'application/json'},
#                            **query_dict
#                            }).json

#     assert res['status'] == str(200)
#     # check that forecast is instantiated
#     dates, values = get_ts_data(
#         res['forecast_iri'], ts_client)
#     assert len(dates) == expected['horizon']
#     assert len(values) == expected['horizon']
#     assert dates[0] == expected['forecast_start_date']
#     for k, v in expected['fc_model'].items():
#         assert res['fc_model'][k] == v
#     assert len(res['fc_model']['covariates_iris']) == 2
#     assert res['data_length'] == expected['data_length']
#     assert res['model_configuration_name'] == expected['model_configuration_name']
#     assert res['iri'] == expected['iri']
#     assert res['horizon'] == expected['horizon']
#     assert res['model_input_interval'] == expected['model_input_interval']
#     assert res['model_output_interval'] == expected['model_output_interval']
#     assert res['unit'] == expected['unit']


# @pytest.mark.parametrize(
#     "query_dict, expected_error_message", 
#     [
#         (cf.query_error8, cf.expected_error8),
#         (cf.query_error9, cf.expected_error9),
#         (cf.query_error10, cf.expected_error10)
#     ]
# )
# def test_tft_error(query_dict, expected_error_message, test_client, initialise_clients):

#     # Get SPARQL client from fixture
#     kg_client, ts_client, rdb_url = initialise_clients
#     # Create clean slate for test and initialise data as required
#     cf.initialise_tft(kg_client, ts_client, rdb_url)

#     res = test_client.post('/forecast', 
#                            json={"headers": {'content_type': 'application/json'},
#                            **query_dict
#                            })

#     assert res.status_code == 500
#     assert expected_error_message in res.json['msg']


# @pytest.mark.parametrize(
#     "query_dict, expected_code, expected_error", 
#     [
#         (cf.query1, 200, None),
#         (cf.query_error11, 500, cf.expected_error11),
#         (cf.query_error12, 500, cf.expected_error12),
#         # Verify that default endpoints are (still) used
#         (cf.query1, 200, None),
#     ]
# )
# def test_http_connection_config(query_dict, expected_code, expected_error, test_client, initialise_clients):

#     # Get SPARQL client from fixture
#     kg_client, ts_client, rdb_url = initialise_clients
#     # Create clean slate for test and initialise data as required
#     cf.initialise_prophet(kg_client, ts_client, rdb_url)

#     res = test_client.post('/forecast', 
#                            json={"headers": {'content_type': 'application/json'},
#                            **query_dict
#                            })
    
#     # Verify that request fails for changed RDB and KG endpoints
#     assert res.status_code == expected_code
#     if expected_code == 500:
#         # Verify expected error message
#         assert expected_error in res.json['msg']
