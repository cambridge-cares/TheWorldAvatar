# The purpose of this module is to provide functionality to use 
# the TimeSeriesClient from the JPS_BASE_LIB

from contextlib import contextmanager

from iris import *

from pyderivationagent.kg_operations.gateway import jpsBaseLibGW
from configs import DB_URL, DB_USER, DB_PASSWORD

# Dates from HM Land Registry are reported in xsd:gYearMonth, i.e. ISO 8601 YYYY-MM
# However, YearMonth not supported by TimeSeriesClient RDB implementation
# --> Used xsd:date, i.e. ISO 8601 YYYY-MM-DD, during instantiation (via PropertySalesInstantiationAgent)
TIME_FORMAT_KG = 'YYYY-MM-DD'   # used by TimeSeriesClient to create KG markup
TIME_FORMAT_RDB = '%Y-%m-%d'    # used by pandas for parsing & RDB to store date


class TSClient:

    # Create ONE JVM module view on class level and import all required java classes
    jpsBaseLibView = jpsBaseLibGW.createModuleView()
    jpsBaseLibGW.importPackages(jpsBaseLibView, "uk.ac.cam.cares.jps.base.query.*")
    jpsBaseLibGW.importPackages(jpsBaseLibView, "uk.ac.cam.cares.jps.base.timeseries.*")
    # Create time and data class for time entries (LocalDate, Double)
    # NOTE: Define inside class to avoid necessity for another JVM module view
    # PostgreSQL supported data types: https://www.jooq.org/javadoc/dev/org.jooq/org/jooq/impl/SQLDataType.html
    LocalDate = jpsBaseLibView.java.time.LocalDate
    TIMECLASS = LocalDate.now().getClass()
    DATACLASS = jpsBaseLibView.java.lang.Double.TYPE


    def __init__(self, kg_client, timeclass=TIMECLASS, rdb_url=DB_URL, 
                 rdb_user=DB_USER, rdb_password=DB_PASSWORD):
        """
        Initialise TimeSeriesClient (default properties taken from environment variables)

        Arguments:
            kg_client (KGClient): KGClient object (as per `kgclient.py` (i.e. PySparqlClient))
            timeclass: Java time class objects supported by PostgreSQL
                       (see: https://www.jooq.org/javadoc/dev/org.jooq/org/jooq/impl/SQLDataType.html)
            rdb_url (str): URL of relational database
            rdb_user (str): Username for relational database
            rdb_password (str): Password for relational database
        """

        # 1) Create an instance of a RemoteStoreClient (to retrieve RDB connection)
        try:
            self.connection = TSClient.jpsBaseLibView.RemoteRDBStoreClient(rdb_url, rdb_user, rdb_password)
        except Exception as ex:
            print("Unable to initialise TS Remote Store client.")
            raise ex

        # 2) Initiliase TimeSeriesClient
        try:
            self.tsclient = TSClient.jpsBaseLibView.TimeSeriesClient(kg_client.kg_client, timeclass)
        except Exception as ex:
            print("Unable to initialise TS client.")
            raise ex


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
            timeseries = TSClient.jpsBaseLibView.TimeSeries(times, dataIRIs, values)
        except Exception as ex:
            print("Unable to create timeseries.")
            raise ex
        
        return timeseries
