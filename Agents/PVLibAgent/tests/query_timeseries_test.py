from datetime import datetime, timezone

import pytest

from PVLibAgent.error_handling.exceptions import TSException
from PVLibAgent.data_instantiation.timeseries_instantiation import timeseries_instantiation
from PVLibAgent.kg_utils.kgClient import KGClient
from PVLibAgent.kg_utils.tsClientForQuery import TSClientForQuery
from PVLibAgent.kg_utils.tsClientForUpdate import TSClientForUpdate
from PVLibAgent.kg_utils.utils import DATACLASS, TIME_FORMAT, QUERY_ENDPOINT, UPDATE_ENDPOINT, DB_UPDATE_URL, \
    DB_UPDATE_USER, DB_UPDATE_PASSWORD, DB_QUERY_PASSWORD, DB_QUERY_URL, DB_QUERY_USER
from PVLibAgent.data_retrieval.query_timeseries import query_latest_timeseries



class TestQueryTimeseries:
    def tearDown(self):
        kg_client = KGClient(query_endpoint=QUERY_ENDPOINT, update_endpoint=UPDATE_ENDPOINT)
        ts_client = TSClientForQuery(kg_client=kg_client, rdb_url=DB_QUERY_URL, rdb_user=DB_QUERY_USER,
                                     rdb_password=DB_QUERY_PASSWORD)
        with ts_client.connect() as conn:
            try:
                (ts_client.tsclient.deleteAll(conn))
            except Exception as ex:
                raise TSException("Unable to delete timeseries.") from ex

    def utcformat(self, dt, timespec):
            """convert datetime to string in UTC format (YYYY-mm-ddTHH:MM:SS.mmmZ)"""
            iso_str = dt.astimezone(timezone.utc).isoformat('T', timespec)
            return iso_str.replace('+00:00', 'Z')

    def test_query_latest_timeseries_fails(self):
        with pytest.raises(TSException) as excinfo:
            query_latest_timeseries('http://test', QUERY_ENDPOINT, UPDATE_ENDPOINT, DB_UPDATE_URL, DB_UPDATE_USER, DB_UPDATE_PASSWORD)
        assert 'Unable to get latest data from knowledge graph!' in str(excinfo.value)
        self.tearDown()

    def test_query_latest_timeseries_successful(self):
        timeseries_instantiation.init_timeseries(['http://test'], [DATACLASS], TIME_FORMAT, QUERY_ENDPOINT, UPDATE_ENDPOINT, DB_UPDATE_URL, DB_UPDATE_USER, DB_UPDATE_PASSWORD)
        now = datetime.now(tz=timezone.utc)
        time = self.utcformat(now, 'seconds')
        test_timeseries = TSClientForUpdate.create_timeseries([time], ['http://test'], [[float(10)]])
        timeseries_instantiation.add_timeseries_data(test_timeseries, QUERY_ENDPOINT, UPDATE_ENDPOINT, DB_UPDATE_URL, DB_UPDATE_USER, DB_UPDATE_PASSWORD)
        timeseries = query_latest_timeseries('http://test', QUERY_ENDPOINT, UPDATE_ENDPOINT, DB_UPDATE_URL, DB_UPDATE_USER, DB_UPDATE_PASSWORD)
        timestamp = [d.toString() for d in timeseries.getTimes()]
        assert str(timestamp).__contains__(time)
        data = [v for v in timeseries.getValues('http://test')]
        assert str(data).__contains__('10')
        self.tearDown()



