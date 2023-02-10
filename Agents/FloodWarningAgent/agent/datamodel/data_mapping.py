###############################################
# Authors: Markus Hofmeister (mh807cam.ac.uk) #    
# Date: 10 Feb 2023                           #
###############################################

# The purpose of this module is to provide mappings between retrieved 
# data from the Environment Agency Real Time flood-monitoring API
# (https://environment.data.gov.uk/flood-monitoring/doc/reference#flood-warnings)
# and corresponding concepts and units as defined in OntoFlood

from agent.utils.javagateway import jpsBaseLibGW


# Create Java classes for all time series data
jpsBaseLibView = jpsBaseLibGW.createModuleView()
# Time entries (Instant)
Instant = jpsBaseLibView.java.time.Instant
TIMECLASS = Instant.now().getClass()
# Data class (i.e. all data as double)
DOUBLE = jpsBaseLibView.java.lang.Double.TYPE
STRING = jpsBaseLibView.java.lang.String.TYPE


# Times are reported in ISO 8601 dateTime (UTC)
TIME_FORMAT = '%Y-%m-%dT%H:%M:%SZ'

