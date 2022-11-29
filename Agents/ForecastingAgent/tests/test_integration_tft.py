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
heatDemand_ts_iri = KB + 'Measure_' + str(uuid.uuid4())
update += get_properties_for_subj(subj=heatDemand_ts_iri, verb_obj={
    RDF_TYPE: OM_MEASURE,
    OM_HASUNIT: OM_MEGAWATTHOUR})
update += get_properties_for_subj(subj=heatDemand_iri, verb_obj={
    RDF_TYPE: OHN_HEATDEMAND,
    OM_HASVALUE: heatDemand_ts_iri})


init_ts(heatDemand_ts_iri,
        test_data['timestamp'], test_data['heatDemand'], tsClient)

# air temperature
airTemp_iri = KB + "AirTemperature_" + str(uuid.uuid4())
airTemp_ts_iri = airTemp_iri
update += get_properties_for_subj(subj=airTemp_ts_iri, verb_obj={
    RDF_TYPE: ONTOEMS_AIRTEMPERATURE,
    OM_HASVALUE: airTemp_ts_iri})

init_ts(airTemp_ts_iri, test_data['timestamp'], test_data['airTemp'], tsClient)

# is holiday
isHoliday_iri = KB + "IsHoliday_" + str(uuid.uuid4())
isHoliday_ts_iri = isHoliday_iri
update += get_properties_for_subj(subj=isHoliday_ts_iri, verb_obj={
    RDF_TYPE: OHN_ISPUBLICHOLIDAY,
    OM_HASVALUE: isHoliday_ts_iri})

init_ts(isHoliday_ts_iri, test_data['timestamp'],
        test_data['isHoliday'], tsClient)



forecast_start_date = (pd.to_datetime(timestamps[-1]) - 168 *  pd.Timedelta('1 hour')).strftime(TIME_FORMAT)
query1 = {"query": {
        "forecast_start_date": forecast_start_date,
        "iri": heatDemand_iri,
        "data_length": 168,
        "horizon": 3,
        "use_model_configuration": "TFT_HEAT_SUPPLY"
}}
expected1 = {'fc_model': {'train_again': False, 'name': 'prophet', 'scale_data': True, 'input_length': query1['query']['data_length']},
                'data_length': query1['query']['data_length'],
                'model_configuration_name': query1['query']['use_model_configuration'],
                'iri': query1['query']['iri'],
                'horizon': query1['query']['horizon'],
                'forecast_start_date': query1['query']['forecast_start_date'],
                'model_input_interval': ['Thu, 15 Aug 2019 01:00:00 GMT', 'Thu, 22 Aug 2019 00:00:00 GMT'], 
                'model_output_interval': ['Thu, 22 Aug 2019 01:00:00 GMT', 'Thu, 22 Aug 2019 03:00:00 GMT'], 
                'unit': OM_MEGAWATTHOUR, 
                }

@pytest.mark.parametrize("query_dict, expected", [
         (query1, expected1),
    
])
def test_tft(query_dict, expected, test_client):

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
    

