################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 17 Oct 2022                            #
################################################

# The purpose of this module is to provide functionality to use
# the TimeSeriesClient from the JPS_BASE_LIB

#import agentlogging
from avgsmpriceagent.errorhandling.exceptions import TSException
from avgsmpriceagent.kg_operations.javagateway import jpsBaseLibGW
from avgsmpriceagent.kg_operations.kgclient import KGClient
from avgsmpriceagent.datamodel.data_mapping import TIMECLASS
from avgsmpriceagent.utils.stack_configs import QUERY_ENDPOINT, UPDATE_ENDPOINT, \
                                             DB_URL, DB_USER, DB_PASSWORD

# Initialise logger
#logger = agentlogging.get_logger("prod")


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
            #logger.error("Unable to initialise TS client RDB connection.")
            raise TSException("Unable to initialise TS client RDB connection.") from ex

        # 2) Initiliase TimeSeriesClient
        try:
            #TODO: fix this
            # An error occurred while calling None.uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient. Trace:
            # py4j.Py4JException: Constructor uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient([class uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient, class java.lang.Class])
            self.tsclient = jpsBaseLibView.TimeSeriesClient(kg_client.kg_client, timeclass)
        except Exception as ex:
            #logger.error("Unable to initialise TS client.")
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
            #logger.error("Unable to create timeseries.")
            raise TSException("Unable to create timeseries.") from ex
        
        return timeseries


# TODO: to be removed
# Test connection (for local development)
if __name__ == '__main__':

    # Create a JVM module view and retrieve Java class for time entries (Instant)
    jpsBaseLibView = jpsBaseLibGW.createModuleView()
    DOUBLE = jpsBaseLibView.java.lang.Double.TYPE
    TIME_FORMAT_TS =  "YYYY-MM-DDThh:mm:ssZ"

    kgclient = KGClient(QUERY_ENDPOINT, UPDATE_ENDPOINT)
    ts_client = TSClient(kg_client=kgclient)
    # init
    data_IRI = 'http://dataIRI_1'
    ts_client.tsclient.initTimeSeries([data_IRI], [DOUBLE], TIME_FORMAT_TS,
                                        ts_client.conn)
    # add data
    time_list = ["2022-10-18", "2022-10-19"]
    value_list = [1.0, 2.0]
    ts = TSClient.create_timeseries(time_list, [data_IRI], [value_list])
    ts_client.tsclient.addTimeSeriesData(ts, ts_client.conn)
