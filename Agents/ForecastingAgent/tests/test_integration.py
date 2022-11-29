################################################
# Authors: Magnus Mueller (mm2692@cam.ac.uk)   #
# Date: 30 Nov 2022                            #
################################################

#import copy
import pytest
#from math import nan
from forecasting.errorhandling.exceptions import InvalidInput

from forecasting.flaskapp import create_app
from forecasting.datamodel.data_mapping import *
from forecasting.datamodel.iris import *
from forecasting.utils.properties import *
from forecasting.errorhandling.exceptions import KGException
from forecasting.kgutils.kgclient import KGClient
#from pytest_mock import MockerFixture

from forecasting.kgutils.tsclient import TSClient
from forecasting.utils.tools import *
from forecasting.utils.useful_queries import *
#from forecasting.forecasting_agent.agent import *
import numpy as np
import uuid

# print(DB_URL)
# 1. Instantiate test data
# 2. Create app
# 3. Forecast iri with app
# 4. Check if forecast is correct

TIME_FORMAT_TS = "YYYY-MM-DDThh:mm:ssZ"


def init_ts(iri, dates, values, tsClient):
    # call client
    with tsClient.connect() as conn:
        tsClient.tsclient.initTimeSeries([iri], [DOUBLE], TIME_FORMAT_TS, conn)
        ts = TSClient.create_timeseries(
            dates.to_list(), [iri], [values.to_list()])
        tsClient.tsclient.addTimeSeriesData(ts, conn)


kgClient = KGClient(QUERY_ENDPOINT, UPDATE_ENDPOINT)
tsClient = TSClient(kg_client=kgClient, rdb_url=DB_URL,
                    rdb_user=DB_USER, rdb_password=DB_PASSWORD)
query_delete = \
       """
       DELETE WHERE {?s ?p ?o}
       """
kgClient.performUpdate(query_delete)
update = ''
n = 400
#  the test data
timestamps = pd.date_range(start='2019-08-05T09:00:00Z', periods=n,
                           freq='H').strftime(TIME_FORMAT)
print(f'timestamps start: {timestamps[0]}, timestamps end: {timestamps[-1]}')



# test data generated heat
test_data2 = pd.DataFrame({
    'timestamp': timestamps,
    'generatedHeat': np.random.rand(n)})

generatedHeat_iri = KB + "GeneratedHeat_" + str(uuid.uuid4())
generatedHeat_ts_iri = KB + 'Measure_' + str(uuid.uuid4())

update += get_properties_for_subj(subj=generatedHeat_ts_iri, verb_obj={
    RDF_TYPE: OM_MEASURE,
    OM_HASUNIT: OM_MEGAWATTHOUR
})
update += get_properties_for_subj(subj=generatedHeat_iri, verb_obj={
    RDF_TYPE: OHN_GENERATEDHEATAMOUNT,
    OM_HASVALUE: generatedHeat_ts_iri
})

init_ts(generatedHeat_ts_iri,
        test_data2['timestamp'], test_data2['generatedHeat'], tsClient)

# Convert to proper format
update = add_insert_data(update)

# Execute query
kgClient.performUpdate(update)
print("Test data initialized")

# create flask app
@pytest.fixture(scope='module')
def test_client():
    flask_app = create_app()
    # Create a test client using the Flask application configured for testing
    with flask_app.test_client() as testing_client:
        # Establish an application context
        with flask_app.app_context():
            yield testing_client  # this is where the testing happens!

forecast_start_date = (pd.to_datetime(timestamps[-1]) + pd.Timedelta('1 hour')).strftime(TIME_FORMAT)
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
    assert len(dates) ==  expected['horizon']
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
query1_error = {"query": {
     "iri": generatedHeat_iri
}}
expected_errror1 = '"horizon" (how many steps to forecast) must be provided.'
query2_error ={ "query": {
     "horizon": 3
}}
expected_errror2 = '"iri" must be provided.'

query3_error = {}
expected_errror3 = 'No JSON "query" object could be identified.'

query4 = {"query": {
        "iri": 'blablabal',
        "data_length": 168,
        "horizon": 3
}}
expected4 = 'No data found for the given IRI.'
query5 = {"query": {
     "iri": generatedHeat_iri,
     "data_length": 168,
     "horizon": 3,
     "use_model_configuration": "blablabla"}}
expected5 = 'No model configuration found for the given name.'
query6 = {"query": {
     "iri": generatedHeat_iri,
     "data_length": 168,
     "horizon": 3,
     "forecasting_start_date": "3999-08-15T01:00:00"}}
expected6 = 'The given forecasting start date is in the future.'

@pytest.mark.parametrize("query_dict, expected_error_message", "error", [
         (query1_error, expected_errror1, InvalidInput),
         (query2_error, expected_errror2, InvalidInput),
         (query3_error, expected_errror3, InvalidInput),
         (query4, expected4, KeyError),
         (query5, expected5, KeyError),
         (query6, expected6, KeyError)
])
def test_prophet_error(query_dict, expected_error_message, error, test_client):

     with pytest.raises(error) as e:
          test_client.post('/api/forecastingAgent/forecast', json={"headers": {'content_type': 'application/json'},
               **query_dict
          })
          assert e == expected_error_message

