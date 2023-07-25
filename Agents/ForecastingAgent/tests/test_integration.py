#################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk)  #
#          Magnus Mueller (mm2692@cam.ac.uk)    #
# Date: 25 Jul 2023                             #
#################################################

# The purpose of this module is to test the Agent Flask App. HTTP requests are sent 
# to the Flask App and the response is checked.

# Each test follows the same structure:
# 1. Instantiate test data
# 2. Create agent app
# 3. Send request to forecast iri 
# 4. Check if forecast/error is correct (via agent response)

import pytest
from pathlib import Path
from rdflib import Graph

from py4jps import agentlogging

from forecastingagent.datamodel.iris import *
from forecastingagent.datamodel.data_mapping import *
from forecastingagent.utils.tools import *
from forecastingagent.utils.env_configs import *
from forecastingagent.agent.forecasting import *
from forecastingagent.datamodel.data_mapping import DOUBLE, TIME_FORMAT

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
    # Get SPARQL client from fixture
    sparql_client, ts_client, _, rdb_url = initialise_clients

    ### TRIPPLE STORE ###
    # Verify that KG is empty
    assert sparql_client.getAmountOfTriples() == 0

    # Upload example test triples
    cf.initialise_triples(sparql_client)

    # Verify instantiation of expected number of triples
    assert sparql_client.getAmountOfTriples() == cf.INITIAL_TRIPLES

    ### POSTGRESQL ###
    # Verify that Postgres database is empty
    assert cf.get_number_of_rdb_tables(rdb_url) == 0

    # Initialise and Upload time series
    ts_client.init_timeseries(dataIRI=cf.IRI_TO_FORECAST_1,
                              times=cf.TIMES, values=cf.VALUES,
                              ts_type=DOUBLE, time_format=TIME_FORMAT)

    # Verify that expected tables and triples are created (i.e. dbTable + 1 ts table)
    assert cf.get_number_of_rdb_tables(rdb_url) == 2
    assert sparql_client.getAmountOfTriples() == (cf.INITIAL_TRIPLES + cf.TS_TRIPLES)

    # Verify correct retrieval of time series data
    times, values = ts_client.retrieve_timeseries(cf.IRI_TO_FORECAST_1)
    assert times == cf.TIMES
    # Account for rounding errors
    assert pytest.approx(values, rel=1e-5) == cf.VALUES

    # Verify that dropping all tables works as expected
    cf.clear_database(rdb_url)
    assert cf.get_number_of_rdb_tables(rdb_url) == 0


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
