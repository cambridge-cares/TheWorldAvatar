################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 17 Oct 2022                            #
################################################

# The purpose of this module is to provide functionality to use
# the TimeSeriesClient from the JPS_BASE_LIB

import agentlogging
from agent.errorhandling.exceptions import TSException
from agent.kgutils.javagateway import jpsBaseLibGW
from agent.kgutils.kgclient import KGClient
from agent.datamodel.data_mapping import TIMECLASS
from agent.utils.stack_configs import QUERY_ENDPOINT, UPDATE_ENDPOINT, \
                                             DB_URL, DB_USER, DB_PASSWORD

# Initialise logger
logger = agentlogging.get_logger("prod")


class TSClient:

    def __init__(self, kg_client, timeclass=TIMECLASS, rdb_url=DB_URL, 
                 rdb_user=DB_USER, rdb_password=DB_PASSWORD):
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

        # Create a JVM module view and use it to import the required java classes
        jpsBaseLibView = jpsBaseLibGW.createModuleView()
        jpsBaseLibGW.importPackages(jpsBaseLibView, "uk.ac.cam.cares.jps.base.query.*")
        jpsBaseLibGW.importPackages(jpsBaseLibView, "uk.ac.cam.cares.jps.base.timeseries.*")

        # 1) Create an instance of a RemoteStoreClient (to retrieve RDB connection)
        try:
            connection = jpsBaseLibView.RemoteRDBStoreClient(rdb_url, rdb_user, rdb_password)
            self.conn = connection.getConnection()
        except Exception as ex:
            logger.error("Unable to initialise TS client RDB connection.")
            raise TSException("Unable to initialise TS client RDB connection.") from ex

        # 2) Initiliase TimeSeriesClient
        try:
            self.tsclient = jpsBaseLibView.TimeSeriesClient(kg_client.kg_client, timeclass)
        except Exception as ex:
            logger.error("Unable to initialise TS client.")
            raise TSException("Unable to initialise TS client.") from ex


    @staticmethod
    def create_timeseries(times: list, dataIRIs: list, values: list):
        # Create Java TimeSeries object (i.e. to attach via TSClient)

        # Create a JVM module view and use it to import the required java classes
        jpsBaseLibView = jpsBaseLibGW.createModuleView()
        jpsBaseLibGW.importPackages(jpsBaseLibView, "uk.ac.cam.cares.jps.base.timeseries.*")

        try:
            timeseries = jpsBaseLibView.TimeSeries(times, dataIRIs, values)
        except Exception as ex:
            logger.error("Unable to create timeseries.")
            raise TSException("Unable to create timeseries.") from ex
        
        return timeseries
