from datetime import datetime, timezone

import pytest

from PVLibAgent.error_handling.exceptions import TSException
from PVLibAgent.data_instantiation.timeseries_instantiation import timeseries_instantiation
from PVLibAgent.kg_utils.kgClient import KGClient
from PVLibAgent.kg_utils.tsClientForQuery import TSClientForQuery
from PVLibAgent.kg_utils.tsClientForUpdate import TSClientForUpdate
from PVLibAgent.kg_utils.utils import DATACLASS, TIME_FORMAT, QUERY_ENDPOINT, UPDATE_ENDPOINT, DB_UPDATE_URL, \
    DB_UPDATE_USER, DB_UPDATE_PASSWORD, DB_QUERY_PASSWORD, DB_QUERY_URL, DB_QUERY_USER
from PVLibAgent.kg_utils.utils import create_sparql_prefix



class TestDataInstantiation:
    def tearDown(self):
        kg_client = KGClient(query_endpoint=UPDATE_ENDPOINT, update_endpoint=UPDATE_ENDPOINT)
        ts_client = TSClientForUpdate(kg_client=kg_client, rdb_url=DB_UPDATE_URL, rdb_user=DB_UPDATE_USER,
                                      rdb_password=DB_UPDATE_PASSWORD)
        with ts_client.connect() as conn:
            try:
                (ts_client.tsclient.deleteAll(conn))
            except Exception as ex:
                raise TSException("Unable to delete timeseries.") from ex

    def utcformat(self, dt, timespec):
            """convert datetime to string in UTC format (YYYY-mm-ddTHH:MM:SS.mmmZ)"""
            iso_str = dt.astimezone(timezone.utc).isoformat('T', timespec)
            return iso_str.replace('+00:00', 'Z')

    def test_add_timeseries_data_fails(self):
        # test adding timeseries data without initialising timeseries first
        with pytest.raises(TSException) as excinfo:
            now = datetime.now(tz=timezone.utc)
            time = self.utcformat(now, 'seconds')
            test_timeseries = TSClientForUpdate.create_timeseries([time], ['http://test'], [[float(10)]])
            timeseries_instantiation.add_timeseries_data(test_timeseries, QUERY_ENDPOINT, UPDATE_ENDPOINT, DB_UPDATE_URL, DB_UPDATE_USER, DB_UPDATE_PASSWORD)
        # Check correct exception message
        assert 'Adding of timeseries data to knowledge graph was not successful.' in str(excinfo.value)
        self.tearDown()

    def test_init_timeseries_data_successful(self):
        # init_timeseries_data will only fail if there are connection errors
        timeseries_instantiation.init_timeseries(['http://test'], [DATACLASS], TIME_FORMAT, QUERY_ENDPOINT, UPDATE_ENDPOINT, DB_UPDATE_URL, DB_UPDATE_USER, DB_UPDATE_PASSWORD)
        kg_client = KGClient(QUERY_ENDPOINT, UPDATE_ENDPOINT)
        query = create_sparql_prefix('rdf') + \
                create_sparql_prefix('ts') + \
                '''SELECT ?b WHERE { ?a ts:hasTimeSeries ?b }'''

        response = kg_client.performQuery(query)

        for d in response:
            val = d["b"]
        assert str(val).__contains__('https://www.theworldavatar.com/kg/ontotimeseries/Timeseries')
        self.tearDown()

    def test_add_timeseries_data_successful(self):
        timeseries_instantiation.init_timeseries(['http://test'], [DATACLASS], TIME_FORMAT, QUERY_ENDPOINT, UPDATE_ENDPOINT, DB_UPDATE_URL, DB_UPDATE_USER, DB_UPDATE_PASSWORD)
        now = datetime.now(tz=timezone.utc)
        time = self.utcformat(now, 'seconds')
        test_timeseries = TSClientForUpdate.create_timeseries([time], ['http://test'], [[float(10)]])
        timeseries_instantiation.add_timeseries_data(test_timeseries, QUERY_ENDPOINT, UPDATE_ENDPOINT, DB_UPDATE_URL, DB_UPDATE_USER, DB_UPDATE_PASSWORD)
        kg_client = KGClient(query_endpoint=QUERY_ENDPOINT, update_endpoint=UPDATE_ENDPOINT)
        ts_client = TSClientForQuery(kg_client=kg_client, rdb_url=DB_QUERY_URL, rdb_user=DB_QUERY_USER,
                                     rdb_password=DB_QUERY_PASSWORD)
        try:
            with ts_client.connect() as conn:
                value = (ts_client.tsclient.getLatestData('http://test', conn))
        except Exception as ex:
            raise TSException("Unable to get latest data from knowledge graph!") from ex

        timestamp = [d.toString() for d in value.getTimes()]
        assert str(timestamp).__contains__(time)
        data = [v for v in value.getValues('http://test')]
        assert str(data).__contains__('10')
        self.tearDown()

    def test_check_data_has_timeseries_true(self):
        timeseries_instantiation.init_timeseries(['http://test'], [DATACLASS], TIME_FORMAT, QUERY_ENDPOINT, UPDATE_ENDPOINT, DB_UPDATE_URL, DB_UPDATE_USER, DB_UPDATE_PASSWORD)
        boolean = timeseries_instantiation.check_data_has_timeseries(['http://test'], QUERY_ENDPOINT, UPDATE_ENDPOINT, DB_UPDATE_URL, DB_UPDATE_USER, DB_UPDATE_PASSWORD)
        assert boolean == True
        self.tearDown()

    def test_check_data_has_timeseries_false(self):
        boolean = timeseries_instantiation.check_data_has_timeseries(['http://test'], QUERY_ENDPOINT, UPDATE_ENDPOINT, DB_UPDATE_URL, DB_UPDATE_USER, DB_UPDATE_PASSWORD)
        assert boolean == False
        self.tearDown()

