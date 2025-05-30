from twa import agentlogging
from contextlib import contextmanager
from agent.utils.baselib_gateway import baselib_view
from agent.utils.stack_configs import BLAZEGRAPH_URL, RDB_USER, RDB_URL, RDB_PASSWORD

logger = agentlogging.get_logger('dev')


class TimeSeriesException(Exception):
    """Raise in case of exception when using the TimeSeriesClient."""


class TimeSeriesClient:

    def __init__(self, point_iri):
        try:
            remote_store_client = baselib_view.RemoteStoreClient(
                BLAZEGRAPH_URL, BLAZEGRAPH_URL)

            self.tsclient = baselib_view.TimeSeriesClientFactory.getInstance(
                remote_store_client, [point_iri])

            self.rdb_remote_client = baselib_view.RemoteRDBStoreClient(
                RDB_URL, RDB_USER, RDB_PASSWORD)
        except Exception as ex:
            logger.error("Unable to initialise TimeSeriesClient.")
            raise TimeSeriesException(
                "Unable to initialise TimeSeriesClient.") from ex

    @contextmanager
    def connect(self):
        """
        Create context manager for RDB connection using getConnection method of Java
        TimeSeries client (i.e. to ensure connection is closed after use)
        """
        conn = None
        try:
            conn = self.rdb_remote_client.getConnection()
            yield conn
        finally:
            if conn is not None:
                conn.close()

    @staticmethod
    def create_time_series(times: list, data_iri_list: list, values: list):
        """
        Create Java TimeSeries object

        Arguments:
            times (list): List of time stamps
            dataIRIs (list): List of dataIRIs
            values (list): List of list of values per dataIRI     
        """
        try:
            timeseries = baselib_view.TimeSeries(times, data_iri_list, values)
        except Exception as ex:
            logger.error("Unable to create TimeSeries object.")
            raise TimeSeriesException("Unable to create timeseries.") from ex

        return timeseries

    def add_time_series(self, time_series):
        with self.connect() as conn:
            self.tsclient.addTimeSeriesData(time_series, conn)
        logger.info('Uploaded time series data')

    def get_time_series(self, data_iri_list: list, lowerbound=None, upperbound=None):
        with self.connect() as conn:
            time_series = self.tsclient.getTimeSeriesWithinBounds(
                data_iri_list, lowerbound, upperbound, conn)
        return time_series

    def add_columns(self, time_series_iri, data_iri: list, class_list: list):
        with self.connect() as conn:
            self.tsclient.addColumnsToExistingTimeSeries(
                time_series_iri, data_iri, class_list, None, conn)
        logger.info('Added new columns')
