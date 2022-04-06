###############################################
# Authors: Markus Hofmeister (mh807cam.ac.uk) #    
# Date: 04 Apr 2022                           #
###############################################

# The purpose of this module is to provide a mapping between retrieved 
# observation variables from Met Office (via metoffer) to corresponding
# concepts and units as defined in OntoEMS

from metoffice.kgutils.javagateway import jpsBaseLibGW


# Mapping of observation variables to OntoEMS concepts
READINGS_MAPPING = {
    'Temperature': 'AirTemperature',
    'Pressure': 'AtmosphericPressure',
    'Dew Point': 'DewPoint',
    'Feels Like Temperature': 'FeelsLikeTemperature',
    'Precipitation Probability': 'PrecipitationProbability',  
    'Screen Relative Humidity': 'RelativeHumidity',
    'Max UV Index':'UVIndex',
    'Visibility': 'Visibility',
    # Wind direction is measured relative to true north (not magnetic north) 
    # and is reported from where the wind is blowing
    'Wind Direction': 'WindDirection',
    'Wind Speed': 'WindSpeed',
    'Wind Gust': 'WindGust'
}

# Mapping of units to OM units and symbols
UNITS_MAPPING = {
    'AirTemperature': ('om:degreeCelsius', '&#x00B0;C'), # °C
    'FeelsLikeTemperature': ('om:degreeCelsius', '&#x00B0;C'), # °C
    'DewPoint': ('om:degreeCelsius', '&#x00B0;C'), # °C
    'AtmosphericPressure': ('om:hectopascal', 'hPa'),
    'PrecipitationProbability': ('om:percent', '%'),
    'RelativeHumidity': ('om:percent', '%'),
    'Visibility': ('om:metre', 'm'),
    'WindSpeed': ('om:mile-StatutePerHour', 'mi/h'),
    'WindGust': ('om:mile-StatutePerHour', 'mi/h'),
    'WindDirection': ('om:degree', '&#x00B0;'),   # °
    'UVIndex': ('om:one', '1')
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

# Times are reported in ISO 8601 dateTime (UTC)
TIME_FORMAT = '%Y-%m-%dT%H:%M:%SZ'

# Create data class for all time series data (i.e. all data as double)
jpsBaseLibView = jpsBaseLibGW.createModuleView()
DATACLASS = jpsBaseLibView.java.lang.Double.TYPE
