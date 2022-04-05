# The purpose of this module is to provide functionality to use
# the TimeSeriesClient from the JPS_BASE_LIB

import os
import json
from pathlib import Path

# import agentlogging
from metoffice.errorhandling.exceptions import TSException
from metoffice.kgutils.javagateway import jpsBaseLibGW


# # Initialise logger
# logger = agentlogging.get_logger("dev")


class TSClient:

    def __init__(self) -> None:
        # Initialise TSClient with default settings

        # Create a JVM module view and use it to import the required java classes
        self.jpsBaseLibView = jpsBaseLibGW.createModuleView()
        jpsBaseLibGW.importPackages(self.jpsBaseLibView, "uk.ac.cam.cares.jps.base.query.*")
        jpsBaseLibGW.importPackages(self.jpsBaseLibView, "uk.ac.cam.cares.jps.base.timeseries.*")

        # Retrieve Java classes for time entries (Instant)
        Instant = self.jpsBaseLibView.java.time.Instant
        instant_class = Instant.now().getClass()

        # Define path to properties file
        fp = os.path.join(Path(__file__).parent.parent.parent, "resources", "metoffice.properties" )

        try:
            self.ts_client = self.jpsBaseLibView.TimeSeriesClient(instant_class, fp)
        except:
            #logger.error("Unable to initialise TS client.")
            raise TSException("Unable to initialise TS client.")
