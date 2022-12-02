################################################
# Authors: Magnus Mueller (mm2692@cam.ac.uk)   #
# Date: 30 Nov 2022                            #
################################################
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

from forecasting.flaskapp import create_app
from forecasting.datamodel.data_mapping import *
from forecasting.datamodel.iris import *
from forecasting.utils.properties import *
from forecasting.kgutils.kgclient import KGClient
from forecasting.kgutils.tsclient import TSClient, init_ts
from forecasting.utils.tools import *
import numpy as np
import uuid

from py4jps import agentlogging
logger = agentlogging.get_logger('prod')

# create flask app
@pytest.fixture(scope='module')
def test_client():
    flask_app = create_app()
    # Create a test client using the Flask application configured for testing
    with flask_app.test_client() as testing_client:
        # Establish an application context
        with flask_app.app_context():
            yield testing_client  # this is where the testing happens!


kgClient = KGClient(QUERY_ENDPOINT, UPDATE_ENDPOINT)
tsClient = TSClient(kg_client=kgClient)
query_delete = \
    """
       DELETE WHERE {?s ?p ?o}
       """
logger.info("Deleting all data from KG namespace")
kgClient.performUpdate(query_delete)
update = ''
n = 400
#  the test data
timestamps = pd.date_range(start='2019-08-05T09:00:00Z', periods=n,
                           freq='H').strftime(TIME_FORMAT)
logger.info(f'timestamps start: {timestamps[0]}, timestamps end: {timestamps[-1]}')


# test data generated heat
test_data2 = pd.DataFrame({
    'timestamp': timestamps,
    'generatedHeat': np.random.rand(n)})

generatedHeat_iri = KB + "GeneratedHeat_" + str(uuid.uuid4())
generatedHeat_dataIRI = KB + 'Measure_' + str(uuid.uuid4())


update += get_properties_for_subj(subj=generatedHeat_dataIRI, verb_obj={
    RDF_TYPE: OM_MEASURE,
    OM_HASUNIT: OM_MEGAWATTHOUR
})
update += get_properties_for_subj(subj=generatedHeat_iri, verb_obj={
    RDF_TYPE: OHN_GENERATEDHEATAMOUNT,
    OM_HASVALUE: generatedHeat_dataIRI
})

init_ts(generatedHeat_dataIRI,
        test_data2['timestamp'], test_data2['generatedHeat'], tsClient, ts_type=DOUBLE, time_format=TIME_FORMAT_TS)

# Convert to proper format
update = add_insert_data(update)

# Execute query
kgClient.performUpdate(update)
logger.info("Test data initialized")


forecast_start_date = (pd.to_datetime(
    timestamps[-1]) + pd.Timedelta('1 hour')).strftime(TIME_FORMAT)
query1 = {"query": {
    "forecast_start_date": forecast_start_date,
    "iri": generatedHeat_iri,
    "data_length": 168,
    "horizon": 3,
    "use_model_configuration": "DEFAULT"
}}
expected1 = {'fc_model': {'train_again': True, 'name': 'prophet', 'scale_data': False, 'input_length': query1['query']['data_length']},
             'data_length': query1['query']['data_length'],
             'model_configuration_name': query1['query']['use_model_configuration'],
             'iri': query1['query']['iri'],
             'horizon': query1['query']['horizon'],
             'forecast_start_date': forecast_start_date,
             'model_input_interval': ['Thu, 15 Aug 2019 01:00:00 GMT', 'Thu, 22 Aug 2019 00:00:00 GMT'],
             'model_output_interval': ['Thu, 22 Aug 2019 01:00:00 GMT', 'Thu, 22 Aug 2019 03:00:00 GMT'],
             'unit': OM_MEGAWATTHOUR,
             }

query2 = {"query": {
    "iri": generatedHeat_iri,
    "data_length": 168,
    "horizon": 3
}}
expected2 = {'fc_model': {'train_again': True, 'name': 'prophet', 'scale_data': False, 'input_length': query2['query']['data_length']},
             'data_length': query2['query']['data_length'],
             'model_configuration_name': 'DEFAULT',
             'iri': query2['query']['iri'],
             'horizon': query2['query']['horizon'],
             'forecast_start_date': forecast_start_date,
             'model_input_interval': ['Thu, 15 Aug 2019 01:00:00 GMT', 'Thu, 22 Aug 2019 00:00:00 GMT'],
             'model_output_interval': ['Thu, 22 Aug 2019 01:00:00 GMT', 'Thu, 22 Aug 2019 03:00:00 GMT'],
             'unit': OM_MEGAWATTHOUR,
             'time_format': TIME_FORMAT_TS
             }


@pytest.mark.parametrize("query_dict, expected", [
    (query1, expected1),
    (query2, expected2)
])
def test_prophet(query_dict, expected, test_client):

    response = test_client.post('/api/forecastingAgent/forecast', json={"headers": {'content_type': 'application/json'},
                                                                        **query_dict
                                                                        }).json

    assert response['status'] == str(200)
    # check that forecast is instantiated
    dates, values = get_ts_data(
        response['forecast_iri'], tsClient)
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


# test error handling
query_error1 = {"query": {
    "iri": generatedHeat_iri
}}
expected_error1 = '"horizon" (how many steps to forecast) must be provided.'
query_error2 = {"query": {
    "horizon": 3
}}
expected_error2 = '"iri" must be provided.'

query_error3 = {}
expected_error3 = 'No JSON "query" object could be identified.'

query_error4 = {"query": {
    "iri": 'blablabal',
    "data_length": 168,
    "horizon": 3
}}
expected_error4 = 'No time series data could be retrieved for the given IRI: blablabal'
query_error5 = {"query": {
    "iri": generatedHeat_iri,
    "data_length": 168,
    "horizon": 3,
    "use_model_configuration": "blablabla"}}

expected_error5 = 'No model configuration found for the given key: blablabla'
query_error6 = {"query": {
    "iri": generatedHeat_iri,
    "data_length": 168,
    "horizon": 24,
    "forecast_start_date": "2049-08-15T01:00:00Z"}}
#expected_error6 = f'Could not get time series for {generatedHeat_dataIRI} with lowerbound'
expected_error6 = 'no values for dataIRI '


@pytest.mark.parametrize("query_dict, expected_error_message", [
    (query_error1, expected_error1),
    (query_error2, expected_error2),
    (query_error3, expected_error3),
    (query_error4, expected_error4),
    (query_error5, expected_error5),
    (query_error6, expected_error6)
])
def test_prophet_error(query_dict, expected_error_message, test_client):

    res = test_client.post('/api/forecastingAgent/forecast',
                           json={"headers": {'content_type': 'application/json'},
                                 **query_dict
                                 })
    assert res.status_code == 500
    assert expected_error_message in res.json['msg']


# test tft
update = ''
# test data heat supply
test_data = pd.DataFrame({
    'timestamp': timestamps,
    'heatDemand': np.random.rand(n),
    'airTemp': np.random.rand(n),
    'isHoliday': np.random.randint(0, 2, n),
})

# initialize the test data in the KG
# heatDemand
heatDemand_iri = KB + "HeatDemand_" + str(uuid.uuid4())
heatDemand_dataIRI = KB + 'Measure_' + str(uuid.uuid4())
update += get_properties_for_subj(subj=heatDemand_dataIRI, verb_obj={
    RDF_TYPE: OM_MEASURE,
    OM_HASUNIT: OM_MEGAWATTHOUR})
update += get_properties_for_subj(subj=heatDemand_iri, verb_obj={
    RDF_TYPE: OHN_HEATDEMAND,
    OM_HASVALUE: heatDemand_dataIRI})


init_ts(heatDemand_dataIRI,
        test_data['timestamp'], test_data['heatDemand'],  tsClient, ts_type=DOUBLE, time_format=TIME_FORMAT_TS)

# air temperature
airTemp_iri = KB + "AirTemperature_" + str(uuid.uuid4())
airTemp_dataIRI = airTemp_iri
update += get_properties_for_subj(subj=airTemp_dataIRI, verb_obj={
    RDF_TYPE: ONTOEMS_AIRTEMPERATURE,
    OM_HASVALUE: airTemp_dataIRI})

init_ts(airTemp_dataIRI, test_data['timestamp'], test_data['airTemp'],
        tsClient, ts_type=DOUBLE, time_format=TIME_FORMAT_TS)

# is holiday
isHoliday_iri = KB + "IsHoliday_" + str(uuid.uuid4())
isHoliday_dataIRI = isHoliday_iri
update += get_properties_for_subj(subj=isHoliday_dataIRI, verb_obj={
    RDF_TYPE: OHN_ISPUBLICHOLIDAY,
    OM_HASVALUE: isHoliday_dataIRI})

init_ts(isHoliday_dataIRI, test_data['timestamp'],
        test_data['isHoliday'],  tsClient, ts_type=DOUBLE, time_format=TIME_FORMAT_TS)


kgClient.performUpdate(add_insert_data(update))

forecast_start_date = (pd.to_datetime(
    timestamps[-1]) - 168 * pd.Timedelta('1 hour')).strftime(TIME_FORMAT)
query1 = {"query": {
    "forecast_start_date": forecast_start_date,
    "iri": heatDemand_iri,
    "data_length": 168,
    "horizon": 24,
    "use_model_configuration": "TFT_HEAT_SUPPLY"
}}
expected1 = {'fc_model': {'train_again': False, 'name': 'tft', 'scale_data': True, 'input_length': query1['query']['data_length'], 'model_path_ckpt_link': 'https://www.dropbox.com/s/fxt3iztbimvm47s/best.ckpt?dl=1',
                          'model_path_pth_link': 'https://www.dropbox.com/s/ntg8lgvh01x09wr/_model.pth.tar?dl=1'},
             'data_length': query1['query']['data_length'],
             'model_configuration_name': query1['query']['use_model_configuration'],
             'iri': query1['query']['iri'],
             'horizon': query1['query']['horizon'],
             'forecast_start_date': query1['query']['forecast_start_date'],
             'model_input_interval': ['Thu, 08 Aug 2019 00:00:00 GMT', 'Wed, 14 Aug 2019 23:00:00 GMT'],
             'model_output_interval': ['Thu, 15 Aug 2019 00:00:00 GMT', 'Thu, 15 Aug 2019 23:00:00 GMT'],
             'unit': OM_MEGAWATTHOUR,
             }


@pytest.mark.parametrize("query_dict, expected", [
    (query1, expected1),

])
def test_tft(query_dict, expected, test_client):

    res = test_client.post('/api/forecastingAgent/forecast', json={"headers": {'content_type': 'application/json'},
                                                                   **query_dict
                                                                   }).json
    assert res['status'] == str(200)
    # check that forecast is instantiated
    dates, values = get_ts_data(
        res['forecast_iri'], tsClient)
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


query_error1 = {"query": {
    "iri": heatDemand_iri,
    "use_model_configuration": "TFT_HEAT_SUPPLY",
    "data_length": 168,
    "horizon": 24}}
expected_error1 = 'Not enough covariates for complete future horizon. Covariates end at '

query_error2 = {"query": {
    "iri": heatDemand_iri,
    "forecast_start_date": forecast_start_date,
    "use_model_configuration": "TFT_HEAT_SUPPLY",
    "data_length": 168,
    "horizon": 3}}
expected_error2 = 'Specify a horizon bigger than the output_chunk_length of your model'


@pytest.mark.parametrize("query_dict, expected_error_message", [
    (query_error1, expected_error1),
    (query_error2, expected_error2),

])
def test_tft_error(query_dict, expected_error_message, test_client):

    res = test_client.post('/api/forecastingAgent/forecast', json={"headers": {'content_type': 'application/json'},
                                                                   **query_dict
                                                                   })
    assert res.status_code == 500
    assert expected_error_message in res.json['msg']
