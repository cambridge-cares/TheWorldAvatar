################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 08 Apr 2022                            #
################################################

# The purpose of this module is to provide functionality to use
# the TimeSeriesClient from the JPS_BASE_LIB

#import agentlogging
from epcdata.errorhandling.exceptions import TSException
from epcdata.kgutils.javagateway import jpsBaseLibGW
from epcdata.kgutils.kgclient import KGClient
from epcdata.utils.stack_configs import QUERY_ENDPOINT, UPDATE_ENDPOINT, \
                                          DB_URL, DB_USER, DB_PASSWORD

# Initialise logger
#logger = agentlogging.get_logger("prod")


class TSClient:

    @staticmethod
    def tsclient_with_default_settings():
        # Initialise TimeSeriesClient with default properties from environment variables

        # Create a JVM module view and use it to import the required java classes
        jpsBaseLibView = jpsBaseLibGW.createModuleView()
        jpsBaseLibGW.importPackages(jpsBaseLibView, "uk.ac.cam.cares.jps.base.query.*")
        jpsBaseLibGW.importPackages(jpsBaseLibView, "uk.ac.cam.cares.jps.base.timeseries.*")

        # Retrieve Java classes for time entries (Instant)
        Instant = jpsBaseLibView.java.time.Instant
        instant_class = Instant.now().getClass()

        # Initialise KG client required for TSClient constructor
        kg_client = KGClient(QUERY_ENDPOINT, UPDATE_ENDPOINT)

        try:
            ts_client = jpsBaseLibView.TimeSeriesClient(kg_client.kg_client, instant_class, 
                                                        DB_URL, DB_USER, DB_PASSWORD)
        except Exception as ex:
            #logger.error("Unable to initialise TS client.")
            raise TSException("Unable to initialise TS client.") from ex
        
        return ts_client


    @staticmethod
    def create_timeseries(times: list, dataIRIs: list, values: list):
        # Create Java TimeSeries object (i.e. to attach via TSClient)

        # Create a JVM module view and use it to import the required java classes
        jpsBaseLibView = jpsBaseLibGW.createModuleView()
        jpsBaseLibGW.importPackages(jpsBaseLibView, "uk.ac.cam.cares.jps.base.query.*")
        jpsBaseLibGW.importPackages(jpsBaseLibView, "uk.ac.cam.cares.jps.base.timeseries.*")

        try:
            timeseries = jpsBaseLibView.TimeSeries(times, dataIRIs, values)
        except Exception as ex:
            #logger.error("Unable to create timeseries.")
            raise TSException("Unable to create timeseries.") from ex
        
        return timeseries
