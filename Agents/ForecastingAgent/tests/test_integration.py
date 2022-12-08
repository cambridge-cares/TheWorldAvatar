#################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk)  #
#          Magnus Mueller (mm2692@cam.ac.uk)    #
# Date: 08 Dec 2022                             #
#################################################
# purpose of this file is to test the flask app. Http requests are sent to the flask app and the response is checked.
# First requests for the Prophet model are sent.
# After that incorrect requests are sent to check if the right error messages are returned.
# Then requests for the TFT model are sent and checked.
# Finally incorrect requests for the TFT model are sent to check if the right error messages are returned.

# 1. Instantiate test data
# 2. Create app
# 3. Forecast iri with app
# 4. Check if forecast/ error is correct

import pytest

from py4jps import agentlogging

from forecasting.datamodel.iris import *
from forecasting.datamodel.data_mapping import *
from forecasting.utils.tools import *
from forecasting.utils.env_configs import *
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
    response = test_client.post('/api/forecastingAgent/forecast', 
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
        (cf.query_error6, cf.expected_error6)
    ]
)
def test_prophet_error(query_dict, expected_error_message, test_client, initialise_clients):

    res = test_client.post('/api/forecastingAgent/forecast',
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

    res = test_client.post('/api/forecastingAgent/forecast', 
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
        (cf.query_error7, cf.expected_error7),
        (cf.query_error8, cf.expected_error8)
    ]
)
def test_tft_error(query_dict, expected_error_message, test_client, initialise_clients):

    res = test_client.post('/api/forecastingAgent/forecast', 
                           json={"headers": {'content_type': 'application/json'},
                           **query_dict
                           })

    assert res.status_code == 500
    assert expected_error_message in res.json['msg']
