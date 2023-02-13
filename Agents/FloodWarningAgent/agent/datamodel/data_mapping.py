###############################################
# Authors: Markus Hofmeister (mh807cam.ac.uk) #    
# Date: 10 Feb 2023                           #
###############################################

# The purpose of this module is to provide mappings between retrieved 
# data from the Environment Agency Real Time flood-monitoring API
# (https://environment.data.gov.uk/flood-monitoring/doc/reference#flood-warnings)
# and corresponding concepts and units as defined in OntoFlood

from .iris import *

from agent.utils.javagateway import jpsBaseLibGW


# Create Java classes for all time series data
jpsBaseLibView = jpsBaseLibGW.createModuleView()
# Time entries (Instant)
Instant = jpsBaseLibView.java.time.Instant
TIMECLASS = Instant.now().getClass()
# Data class (i.e. all data as double)
DOUBLE = jpsBaseLibView.java.lang.Double.TYPE

# Times are reported in ISO 8601 dateTime (UTC)
# NOTE: Potentially to be verified
TIME_FORMAT = '%Y-%m-%dT%H:%M:%S'
BLAZEGRAPH_TIME_FORMAT = '%Y-%m-%dT%H:%M:%S.%fZ'

#
# Waterbodies associated with flood areas
#
# API data mapping
WATERBODIES_API = {
    'river': 'river',
    'brook': 'river',
    'beck': 'river',
    'stream': 'river',
    # frequent river names
    'thames': 'river',
    'ouse': 'river',
    'trent': 'river',
    'severn': 'river',
    'lake': 'lake',
    'canal': 'canal',
    'sea': 'sea',
    'channel': 'sea',
    'harbour': 'sea',
    } 
# IRI mapping
WATERBODIES_IRIS = {
    'waterbody': ENVO_WATER_BODY,
    'river': ENVO_RIVER,
    'lake': ENVO_LAKE,
    'canal': ENVO_CANAL,
    'sea': ENVO_SEA
    }

#
# Severity of flood warnings
#
SEVERITY_IRIS= {
    'severe flood warning': FLOOD_SEVERE_FLOOD_WARNING,
    'flood warning': FLOOD_FLOOD_WARNING,
    'flood alert': FLOOD_FLOOD_ALERT,
    'warning no longer in force': FLOOD_WARNING_NO_LONGER_IN_FORCE,
    }
