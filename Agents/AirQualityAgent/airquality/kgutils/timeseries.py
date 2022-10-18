################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 08 Apr 2022                            #
################################################

# The purpose of this module is to provide functionality to use
# the TimeSeriesClient from the JPS_BASE_LIB

import os
from pathlib import Path

#import agentlogging
from airquality.errorhandling.exceptions import TSException
from airquality.kgutils.javagateway import jpsBaseLibGW

# Initialise logger
#logger = agentlogging.get_logger("prod")


class TSClient:

    @staticmethod
    def tsclient_with_default_settings():
        # Initialise TimeSeriesClient with default properties from properties file

        # Create a JVM module view and use it to import the required java classes
        jpsBaseLibView = jpsBaseLibGW.createModuleView()
        jpsBaseLibGW.importPackages(jpsBaseLibView, "uk.ac.cam.cares.jps.base.query.*")
        jpsBaseLibGW.importPackages(jpsBaseLibView, "uk.ac.cam.cares.jps.base.timeseries.*")

        # Retrieve Java classes for time entries (Instant)
        Instant = jpsBaseLibView.java.time.Instant
        instant_class = Instant.now().getClass()

        # Define path to properties file
        fp = os.path.join(Path(__file__).parent.parent.parent, "resources", "airquality.properties" )

        try:
            ts_client = jpsBaseLibView.TimeSeriesClient(instant_class, fp)
        except:
            #logger.error("Unable to initialise TS client")
            raise TSException("Unable to initialise TS client")
        
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
        except:
            #logger.error("Unable to create timeseries")
            raise TSException("Unable to create timeseries")
        
        return timeseries
