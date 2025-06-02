from agent.utils.baselib_gateway import baselib_view
from agent.utils.stack_configs import BLAZEGRAPH_URL
import uuid

PREFIX = 'https://www.theworldavatar.com/kg/'
TRIP = PREFIX + 'Trip'
VISIT = PREFIX + 'Visit'
TIMESERIES_NAMESPACE = 'https://www.theworldavatar.com/kg/ontotimeseries/'
HAS_TIME_SERIES = TIMESERIES_NAMESPACE + 'hasTimeSeries'
TIMESERIES_TYPE = TIMESERIES_NAMESPACE + 'TimeSeries'
HAS_TIME_CLASS = TIMESERIES_NAMESPACE + 'hasTimeClass'


class KgClient():
    def __init__(self):
        self.remote_store_client = baselib_view.RemoteStoreClient(
            BLAZEGRAPH_URL, BLAZEGRAPH_URL)

    def get_trip_and_visit(self, point_iri: str):
        query = f"""
        SELECT ?trip ?visit
        WHERE {{
            <{point_iri}> <{HAS_TIME_SERIES}> ?time_series.
            ?trip <{HAS_TIME_SERIES}> ?time_series; a <{TRIP}>.
            ?visit <{HAS_TIME_SERIES}> ?time_series; a <{VISIT}>.
        }}
        """
        query_results = self.remote_store_client.executeQuery(query)

        trip = None
        visit = None

        if not query_results.isEmpty():
            trip = query_results.getJSONObject(0).getString('trip')
            visit = query_results.getJSONObject(0).getString('visit')

        return trip, visit

    def get_time_series_iri(self, point_iri: str):
        query = f"""
        SELECT ?time_series
        WHERE {{
            <{point_iri}> <{HAS_TIME_SERIES}> ?time_series.
        }}
        """
        query_results = self.remote_store_client.executeQuery(query)
        return query_results.getJSONObject(0).getString('time_series')

    def instantiate_trip_and_visit(self):
        trip = PREFIX + 'trip/' + str(uuid.uuid4())
        visit = PREFIX + 'visit/' + str(uuid.uuid4())
        query = f"""
        INSERT {{<{trip}> a <{TRIP}>. <{visit}> a <{VISIT}>}}
        WHERE {{}}
        """
        self.remote_store_client.executeUpdate(query)
        return trip, visit

    def get_java_time_class(self, point_iri: str):
        query = f"""
        SELECT ?time_class
        WHERE {{
            <{point_iri}> <{HAS_TIME_SERIES}>/<{HAS_TIME_CLASS}> ?time_class.
        }}
        """
        query_results = self.remote_store_client.executeQuery(query)
        return query_results.getJSONObject(0).getString('time_class')
