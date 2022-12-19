###############################################
# Authors: Markus Hofmeister (mh807cam.ac.uk) #    
# Date: 04 Apr 2022                           #
###############################################

# The purpose of this module is to provide a mapping between retrieved 
# observation variables from Met Office (via metoffer) to corresponding
# concepts and units as defined in OntoEMS

from math import nan

from agent.datamodel.iris import *
#from agent.datamodel.observation_types import *
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

'''
# Mapping of observation variables to OntoEMS concepts
READINGS_MAPPING = {
    'Temperature': AIR_TEMPERATURE,
    'Pressure': ATMOSPHERIC_PRESSURE,
    'Dew Point': DEW_POINT,
    'Feels Like Temperature': FEELS_LIKE_TEMPERATURE,
    'Precipitation Probability': PRECIPITATION_PROBABILITY,  
    'Screen Relative Humidity': RELATIVE_HUMIDITY,
    'Max UV Index': UV_INDEX,
    'Visibility': VISIBILITY,
    # Wind direction is measured relative to true north (not magnetic north) 
    # and is reported from where the wind is blowing
    'Wind Direction': WIND_DIRECTION,
    'Wind Speed': WIND_SPEED,
    'Wind Gust': WIND_GUST
}

# Mapping of units to OM units and symbols
UNITS_MAPPING = {
     # original OM symbol for °C: '&#x00B0;C'
     AIR_TEMPERATURE: (OM_DEGREE_C, '&#x00B0;C'),     
     FEELS_LIKE_TEMPERATURE: (OM_DEGREE_C, '&#x00B0;C'),
     DEW_POINT: (OM_DEGREE_C, '&#x00B0;C'),
     ATMOSPHERIC_PRESSURE: (OM_HECTO_PASCAL, 'hPa'),
     PRECIPITATION_PROBABILITY: (OM_PERCENT, '%'),
     RELATIVE_HUMIDITY: (OM_PERCENT, '%'),
     VISIBILITY: (OM_METRE, 'm'),
     WIND_SPEED: (OM_MPH, 'mi/h'),
     WIND_GUST: (OM_MPH, 'mi/h'),
     # original OM symbol for °: '&#x00B0;'
     WIND_DIRECTION: (OM_DEGREE, '&#x00B0;'),
     UV_INDEX: (OM_UNITLESS, '1')
}

# Mapping of 16 wind direction readings to angles wrt true north
COMPASS = {
    'N':    0.0,
    'NNE':  22.5,
    'NE':   45.0,
    'ENE':  67.5,
    'E':    90.0,
    'ESE':  112.5,
    'SE':   135.0,
    'SSE':  157.5,
    'S':    180.0,
    'SSW':  202.5,
    'SW':   225.0,
    'WSW':  247.5,
    'W':    270.0,
    'WNW':  292.5,
    'NW':   315.0,
    'NNW':  337.5,
}

# Visibility is provided as a mix of values and textual codes
VISIBILITY = {
    'UN': nan,       # Unknown
    'VP': 1000.0,    # Very poor - Less than 1 km
    'PO': 4000.0,    # Poor - Between 1-4 km
    'MO': 10000.0,   # Moderate - Between 4-10 km
    'GO': 20000,     # Good - Between 10-20 km
    'VG': 40000.0,   # Very good - Between 20-40 km
    'EX': 60000.0    # Excellent - More than 40 km
}
'''