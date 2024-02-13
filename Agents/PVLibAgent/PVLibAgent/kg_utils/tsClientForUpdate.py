# The purpose of this module is to provide functionality to use
# the TimeSeriesClient from the JPS_BASE_LIB

from contextlib import contextmanager


from .jpsSingletons import jpsBaseLibGW
from .utils import TIME_FORMAT
from .utils import TIMECLASS
from PVLibAgent.error_handling.exceptions import TSException
import logging

class TSClientForUpdate:
    logging.basicConfig(level=logging.DEBUG)
    # Create ONE JVM module view on class level and import all required java classes
    jpsBaseLibView = jpsBaseLibGW.createModuleView()
    jpsBaseLibGW.importPackages(jpsBaseLibView, "uk.ac.cam.cares.jps.base.query.*")
    jpsBaseLibGW.importPackages(jpsBaseLibView, "uk.ac.cam.cares.jps.base.timeseries.*")

    def __init__(self, kg_client, rdb_url, rdb_user, rdb_password, timeclass=TIMECLASS):
        """
        Initialise TimeSeriesClient (default properties taken from environment variables)
        Arguments:
            kg_client (KGClient): KGClient object (as per `kgclient.py`)
            timeclass: Java time class objects supported by PostgreSQL
                       (see: https://www.jooq.org/javadoc/dev/org.jooq/org/jooq/impl/SQLDataType.html)
            rdb_url (str): URL of relational database
            rdb_user (str): Username for relational database
            rdb_password (str): Password for relational database
        """

        # 1) Create an instance of a RemoteStoreClient (to retrieve RDB connection)
        try:
            self.connection = TSClientForUpdate.jpsBaseLibView.RemoteRDBStoreClient(rdb_url, rdb_user, rdb_password)
        except Exception as ex:
            logging.error("Unable to initialise TS Remote Store client.")
            raise TSException("Unable to initialise TS Remote Store client.") from ex

        # 2) Initiliase TimeSeriesClient
        try:
            self.tsclient = TSClientForUpdate.jpsBaseLibView.TimeSeriesClient(kg_client.kg_client, timeclass)
        except Exception as ex:
            logging.error("Unable to initialise TS client.")
            raise TSException("Unable to initialise TS client.") from ex


    @contextmanager
    def connect(self):
        """
        Create context manager for RDB connection using getConnection method of Java
        TimeSeries client (i.e. to ensure connection is closed after use)
        """
        conn = None
        try:
            conn = self.connection.getConnection()
            yield conn
        finally:
            if conn is not None:
                conn.close()

    @staticmethod
    def create_timeseries(times: list, dataIRIs: list, values: list):
        """
        Create Java TimeSeries object (i.e. to attach via TSClient)

        Arguments:
            times (list): List of time stamps
            dataIRIs (list): List of dataIRIs
            values (list): List of list of values per dataIRI
        """
        try:
            timeseries = TSClientForUpdate.jpsBaseLibView.TimeSeries(times, dataIRIs, values)

        except Exception as ex:
            logging.error("Unable to create timeseries.")
            raise TSException("Unable to create timeseries.") from ex

        return timeseries










