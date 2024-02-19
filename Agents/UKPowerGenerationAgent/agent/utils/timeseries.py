###############################################
# Authors: Feroz Farazi (msff2@cam.ac.uk)     #    
# Date: 02 July 2023                          #
###############################################

# The purpose of this module is to provide access to instances of
# time series classes

from math import nan

from agent.datamodel.iris import *
from agent.kgutils.javagateway import jpsBaseLibGW


# Create Java classes for all time series data
jpsBaseLibView = jpsBaseLibGW.createModuleView()
# Time entries (Instant)
Instant = jpsBaseLibView.java.time.Instant
TIMECLASS = Instant.now().getClass()
# Data class (i.e. all data as double)
DATACLASS = jpsBaseLibView.java.lang.Double.TYPE

# Times are reported in ISO 8601 dateTime (UTC)
TIME_FORMAT = '%Y-%m-%dT%H:%M:%SZ'
