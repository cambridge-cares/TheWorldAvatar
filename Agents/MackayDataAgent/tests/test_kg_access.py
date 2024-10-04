#All tests releated to KG access module
import pytest
from data_classes.ts_data_classes import TimeSeriesInstance, TimeSeriesMeta, parse_incomplete_time, parse_time_to_format, KgAccessInfo,ForecastMeta
from kg_access.tsclient_wrapper import TSClient,create_postgres_db_if_not_exists
from kg_access.forecast_client import ForcastAgentClient
from utils import conf_utils
import os
from utils.conf_utils import load_conf
from testconf import *

def test_tsclient_wrapper_update_timeseries_if_new():
    #Create TEST table and regsiter test TS
    clear_database()
    create_postgres_db_if_not_exists(TS_CONFIG_DICT['db.url'], TS_CONFIG_DICT['db.user'],TS_CONFIG_DICT['db.password'])
    tmp_file = './temp.properties'
    conf_utils.write_java_properties_conf(TS_CONFIG_DICT, tmp_file)
    ts = TSClient(tmp_file)
    ts.register_timeseries(TEST_TS_META)
    #Update with no previous record
    updated = ts.update_timeseries_if_new(TEST_TS)
    retrieved_times,retrieved_values = ts.get_timeseries(TEST_INSTANCE_IRI)
    assert retrieved_times == [ parse_time_to_format(parse_incomplete_time(str(t))) for t in TEST_TIMES]
    assert retrieved_values == TEST_VALUES
    assert updated
    #Update if TS is same as record => will not update
    updated = ts.update_timeseries_if_new(TEST_TS)
    assert not updated
    updated = ts.update_timeseries_if_new(UPDATE_TS)
    #Update if TS is different from record=> delete all old records and instantiate with new TS
    retrieved_times,retrieved_values = ts.get_timeseries(TEST_INSTANCE_IRI)
    assert updated
    assert retrieved_times == [ parse_time_to_format(parse_incomplete_time(str(t))) for t in TEST_UPDATE_TIMES]
    assert retrieved_values == TEST_UPDATE_VALUES
    #Clean up test env
    os.remove(tmp_file)
    ts.delete_timeseries(TEST_INSTANCE_IRI)
    clear_database()


def test_update_forcast_meta():
    clear_database()
    AGENT_CONF = load_conf(CONFIG_FILE)
    agent = ForcastAgentClient(AGENT_CONF['forecast_agent']['agent_url'],AGENT_CONF['forecast_agent']['agent_iri'], KgAccessInfo(endpoint=TS_CONFIG_DICT['sparql.query.endpoint']),BASE_IRI)
    #Check insert
    agent._insert_forecast_meta(TEST_FORECAST_META_1)
    re = agent.get_forecast_meta(TEST_FORECAST_META_1.iri, TEST_FORECAST_META_1.name)
    assert re == {'duration': '30', 'start': '1640995200', 'model': 'Prophet', 'end': '1651363200', 'unit_frequency': 'http://www.w3.org/2006/time#unitDay', 'frequency': '30'}
    #Check delete
    agent._delete_forecast_meta(TEST_FORECAST_META_1.iri, TEST_FORECAST_META_1.name)
    re = agent.get_forecast_meta(TEST_FORECAST_META_1.iri, TEST_FORECAST_META_1.name)
    assert not re # Meta not exists anymore
    #Check update
    TEST_FORECAST_META_1.duration = 29 # Change Meta
    agent.update_forcast_meta(TEST_FORECAST_META_1)
    re = agent.get_forecast_meta(TEST_FORECAST_META_1.iri, TEST_FORECAST_META_1.name)
    assert  re == {'duration': '29', 'start': '1640995200', 'model': 'Prophet', 'end': '1651363200', 'unit_frequency': 'http://www.w3.org/2006/time#unitDay', 'frequency': '30'}
    # clean up test env
    agent._delete_forecast_meta(TEST_FORECAST_META_1.iri, TEST_FORECAST_META_1.name)
    clear_database()
