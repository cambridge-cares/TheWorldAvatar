################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk)        #    
# Date: 17 May 2023                            #
################################################

# The purpose of this module is to provide functionality to use
# the TimeSeriesClient from the JPS_BASE_LIB

from contextlib import contextmanager

from py4jps import agentlogging
from utiliycostcalculationagent.errorhandling.exceptions import TSException
from utiliycostcalculationagent.kg_operations.javagateway import jpsBaseLibGW
from utiliycostcalculationagent.datamodel.data import TIMECLASS
from utiliycostcalculationagent.utils.stack_configs import DB_URL, DB_USER, DB_PASSWORD
from utiliycostcalculationagent.datamodel.data import DATACLASS, TIME_FORMAT

# Initialise logger instance (ensure consistent logger level with `entrypoint.py`)
logger = agentlogging.get_logger('prod')


class TSClient:

    # Create ONE JVM module view on class level and import all required java classes
    jpsBaseLibView = jpsBaseLibGW.createModuleView()
    jpsBaseLibGW.importPackages(jpsBaseLibView, "uk.ac.cam.cares.jps.base.query.*")
    jpsBaseLibGW.importPackages(jpsBaseLibView, "uk.ac.cam.cares.jps.base.timeseries.*")

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
            logger.error("Unable to initialise TS Remote Store client.")
            raise TSException("Unable to initialise TS Remote Store client.") from ex

        # 2) Initiliase TimeSeriesClient
        try:
            self.tsclient = TSClient.jpsBaseLibView.TimeSeriesClient(kg_client.kg_client, timeclass)
        except Exception as ex:
            logger.error("Unable to initialise TS client.")
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
            timeseries = TSClient.jpsBaseLibView.TimeSeries(times, dataIRIs, values)
            
        except Exception as ex:
            logger.error("Unable to create timeseries.")
            print(ex)
            raise TSException("Unable to create timeseries.") from ex
            
        
        return timeseries


    def add_timeseries(self, dates, dataIRIs, values):
        try:
            # Create time series from test data                        
            ts = TSClient.create_timeseries(dates, dataIRIs, values)
            
            with self.connect() as conn:
                # Initialise time series in Blazegraph and PostgreSQL
                self.tsclient.initTimeSeries(dataIRIs, [DATACLASS]*len(dataIRIs), TIME_FORMAT, conn)
                # Add test time series data
                self.tsclient.addTimeSeriesData(ts, conn)

        except Exception as ex:
            logger.error('Error wrapping data time series')
            raise TSException('Error wrapping data time series') from ex


    def delete_timeseries(self, dataIRI:str):
        try:
            with self.connect() as conn:
                # Initialise time series in Blazegraph and PostgreSQL
                self.tsclient.deleteIndividualTimeSeries(dataIRI, conn)

        except Exception as ex:
            logger.error('Error deleting data time series')
            raise TSException('Error deleting data time series') from ex
        
    def retrieve_data(self, dataIRI:str):
        try:
            with self.connect() as conn:
                ts = self.tsclient.getTimeSeries([dataIRI], conn)
        except Exception as ex:
            logger.error('Error when retrieving time series')
            raise TSException('Error when retrieving time series') from ex
        
        dates = [d.toString() for d in ts.getTimes()]
        values = [v for v in ts.getValues(dataIRI)]
        result_dict = {k: v for k, v in zip(dates, values)}

        return result_dict