#################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk)  #
#          Magnus Mueller (mm2692@cam.ac.uk)    #
# Date: 08 Dec 2022                             #
#################################################

# The purpose of this module is to test the Agent Flask App. HTTP requests are sent 
# to the Flask App and the response is checked.

# Each test follows the same structure:
# 1. Instantiate test data
# 2. Create agent app
# 3. Send request to forecast iri 
# 4. Check if forecast/error is correct (via agent response)

import pytest

from py4jps import agentlogging

from forecasting.datamodel.iris import *
from forecasting.datamodel.data_mapping import *
from forecasting.utils.tools import *
from forecasting.utils.default_configs import *
from forecasting.forecasting_agent.agent import *

from . import conftest as cf

# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


def test_load_pretrained_model():
     """
     Test the function `load_pretrained_model` to load a pretrained model from a
     checkpoint file or a PyTorch model file
     NOTE: Test will fail if the model is not available at the given link
     """
     # Test if pretrained model is loaded correctly
     cfg = {
          'model_configuration_name': 'test_model',
          'fc_model': {
               'name': 'TFTModel_test',
               'model_path_ckpt_link': cf.DARTS_MODEL_OBJECT,
               'model_path_pth_link': cf.DARTS_CHECKPOINTS,
          },
     }
     model = load_pretrained_model(cfg, TFTModel, force_download=True)    
     assert model.__class__.__name__ == 'TFTModel'
     assert model.model.input_chunk_length == 168
     assert model.model.output_chunk_length == 24
     
     # use previously downloaded model
     model = load_pretrained_model(cfg, TFTModel, force_download=False)    
     assert model.__class__.__name__ == 'TFTModel'
     assert model.model.input_chunk_length == 168
     assert model.model.output_chunk_length == 24


@pytest.mark.parametrize(
    "query_dict, expected", 
    [
        (cf.query1, cf.expected2),
        (cf.query2, cf.expected2)
    ]
)
def test_prophet(query_dict, expected, test_client, initialise_clients):

    # Get SPARQL client from fixture
    kg_client, ts_client, rdb_url = initialise_clients
    # Create clean slate for test and initialise data as required
    cf.initialise_prophet(kg_client, ts_client, rdb_url)

    # Send request to flask app
    response = test_client.post('/forecast', 
                                json={"headers": {'content_type': 'application/json'},
                                **query_dict
                                }).json
    # Check successful execution/response
    assert response['status'] == str(200)
    
    # Check that forecast is instantiated
    dates, values = get_ts_data(
        response['forecast_iri'], ts_client)
    assert len(dates) == expected['horizon']
    assert len(values) == expected['horizon']
    assert dates[0] == expected['forecast_start_date']
    assert response['fc_model'] == expected['fc_model']
    assert response['data_length'] == expected['data_length']
    assert response['model_configuration_name'] == expected['model_configuration_name']
    assert response['iri'] == expected['iri']
    assert response['horizon'] == expected['horizon']
    assert response['model_input_interval'] == expected['model_input_interval']
    assert response['model_output_interval'] == expected['model_output_interval']
    assert response['unit'] == expected['unit']


@pytest.mark.parametrize(
    "query_dict, expected_error_message", 
    [
        (cf.query_error1, cf.expected_error1),
        (cf.query_error2, cf.expected_error2),
        (cf.query_error3, cf.expected_error3),
        (cf.query_error4, cf.expected_error4),
        (cf.query_error5, cf.expected_error5),
        (cf.query_error6, cf.expected_error6),
        (cf.query_error7, cf.expected_error7)
    ]
)
def test_prophet_error(query_dict, expected_error_message, test_client, initialise_clients):

    # Get SPARQL client from fixture
    kg_client, ts_client, rdb_url = initialise_clients
    # Create clean slate for test and initialise data as required
    cf.initialise_prophet(kg_client, ts_client, rdb_url)

    res = test_client.post('/forecast',
                           json={"headers": {'content_type': 'application/json'},
                           **query_dict
                           })

    assert res.status_code == 500
    assert expected_error_message in res.json['msg']


@pytest.mark.parametrize(
    "query_dict, expected", 
    [
        (cf.query3, cf.expected3),
    ]
)
def test_tft(query_dict, expected, test_client, initialise_clients):

    # Get SPARQL client from fixture
    kg_client, ts_client, rdb_url = initialise_clients
    # Create clean slate for test and initialise data as required
    cf.initialise_tft(kg_client, ts_client, rdb_url)

    res = test_client.post('/forecast', 
                           json={"headers": {'content_type': 'application/json'},
                           **query_dict
                           }).json

    assert res['status'] == str(200)
    # check that forecast is instantiated
    dates, values = get_ts_data(
        res['forecast_iri'], ts_client)
    assert len(dates) == expected['horizon']
    assert len(values) == expected['horizon']
    assert dates[0] == expected['forecast_start_date']
    for k, v in expected['fc_model'].items():
        assert res['fc_model'][k] == v
    assert len(res['fc_model']['covariates_iris']) == 2
    assert res['data_length'] == expected['data_length']
    assert res['model_configuration_name'] == expected['model_configuration_name']
    assert res['iri'] == expected['iri']
    assert res['horizon'] == expected['horizon']
    assert res['model_input_interval'] == expected['model_input_interval']
    assert res['model_output_interval'] == expected['model_output_interval']
    assert res['unit'] == expected['unit']


@pytest.mark.parametrize(
    "query_dict, expected_error_message", 
    [
        (cf.query_error8, cf.expected_error8),
        (cf.query_error9, cf.expected_error9),
        (cf.query_error10, cf.expected_error10)
    ]
)
def test_tft_error(query_dict, expected_error_message, test_client, initialise_clients):

    # Get SPARQL client from fixture
    kg_client, ts_client, rdb_url = initialise_clients
    # Create clean slate for test and initialise data as required
    cf.initialise_tft(kg_client, ts_client, rdb_url)

    res = test_client.post('/forecast', 
                           json={"headers": {'content_type': 'application/json'},
                           **query_dict
                           })

    assert res.status_code == 500
    assert expected_error_message in res.json['msg']


@pytest.mark.parametrize(
    "query_dict, expected_code, expected_error", 
    [
        (cf.query1, 200, None),
        (cf.query_error11, 500, cf.expected_error11),
        (cf.query_error12, 500, cf.expected_error12),
        # Verify that default endpoints are (still) used
        (cf.query1, 200, None),
    ]
)
def test_http_connection_config(query_dict, expected_code, expected_error, test_client, initialise_clients):

    # Get SPARQL client from fixture
    kg_client, ts_client, rdb_url = initialise_clients
    # Create clean slate for test and initialise data as required
    cf.initialise_prophet(kg_client, ts_client, rdb_url)

    res = test_client.post('/forecast', 
                           json={"headers": {'content_type': 'application/json'},
                           **query_dict
                           })
    
    # Verify that request fails for changed RDB and KG endpoints
    assert res.status_code == expected_code
    if expected_code == 500:
        # Verify expected error message
        assert expected_error in res.json['msg']
