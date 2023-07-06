################################################
# Authors: Feroz Farazi (msff2@cam.ac.uk)      #    
# Date: 02 July 2023                           #
################################################

# The purpose of this module is to provide functionality to use
# the TimeSeriesClient from the JPS_BASE_LIB

from contextlib import contextmanager

from py4jps import agentlogging
from agent.errorhandling.exceptions import TSException
from agent.kgutils.javagateway import jpsBaseLibGW
from agent.utils.timeseries import TIMECLASS, DATACLASS, TIME_FORMAT
from agent.utils.stack_configs import DB_URL, DB_USER, DB_PASSWORD

# Initialises logger
logger = agentlogging.get_logger("prod")


class TSClient:

    # Creates ONE JVM module view on class level and import all required java classes
    jpsBaseLibView = jpsBaseLibGW.createModuleView()
    jpsBaseLibGW.importPackages(jpsBaseLibView, "uk.ac.cam.cares.jps.base.query.*")
    jpsBaseLibGW.importPackages(jpsBaseLibView, "uk.ac.cam.cares.jps.base.timeseries.*")

    def __init__(self, kg_client, timeclass=TIMECLASS, rdb_url=DB_URL, 
                 rdb_user=DB_USER, rdb_password=DB_PASSWORD):
        """
        Initialises TimeSeriesClient (default properties taken from environment variables)

        Arguments:
            kg_client (KGClient): KGClient object (as per `kgclient.py`)
            timeclass: Java time class objects supported by PostgreSQL
                       (see: https://www.jooq.org/javadoc/dev/org.jooq/org/jooq/impl/SQLDataType.html)
            rdb_url (str): URL of relational database
            rdb_user (str): Username for relational database
            rdb_password (str): Password for relational database
        """

        # 1) Creates an instance of a RemoteStoreClient (to retrieve RDB connection)
        try:
            self.connection = TSClient.jpsBaseLibView.RemoteRDBStoreClient(rdb_url, rdb_user, rdb_password)
        except Exception as ex:
            logger.error("Unable to initialise TS Remote Store client.")
            raise TSException("Unable to initialise TS Remote Store client.") from ex

        # 2) Initiliases TimeSeriesClient
        try:
            self.tsclient = TSClient.jpsBaseLibView.TimeSeriesClient(kg_client.kg_client, timeclass)
        except Exception as ex:
            logger.error("Unable to initialise TS client.")
            raise TSException("Unable to initialise TS client.") from ex


    @contextmanager
    def connect(self):
        """
        Creates context manager for RDB connection using getConnection method of Java
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
        Creates Java TimeSeries object (i.e. to attach via TSClient)
        
        Arguments:
            times (list): List of time stamps
            dataIRIs (list): List of dataIRIs
            values (list): List of list of values per dataIRI     
        """
        try:
            timeseries = TSClient.jpsBaseLibView.TimeSeries(times, dataIRIs, values)
        except Exception as ex:
            logger.error("Unable to create timeseries.")
            raise TSException("Unable to create timeseries.") from ex
        
        return timeseries
    
    def add_timeseries(self, times, dataIRIs, values):
        try:
            # Creates a time series
            ts = TSClient.create_timeseries(times, dataIRIs, values)
            with self.connect as conn:
                # Initialises time series in Blazegraph and PostgreSQL
                self.tsclient.initTimeSeries(dataIRIs, [DATACLASS]*len(dataIRIs), TIME_FORMAT, conn)
                # Adds time series data
                self.tsclient.addTimeSeriesData(ts, conn)
        except Exception as ex:
            logger.error('Failed to create time series')
            raise TSException('Failed to create time series')

