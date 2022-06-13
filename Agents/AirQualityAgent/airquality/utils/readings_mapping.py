################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 04 Apr 2022                            #
################################################

# The purpose of this module is to provide a mapping between retrieved 
# observation variables from UK-AIR Sensor Observation Service to corresponding
# concepts and units as defined in OntoEMS

from math import nan

from airquality.datamodel.observation_types import *
from airquality.datamodel.iris import *
from airquality.kgutils.javagateway import jpsBaseLibGW


# Mapping of observation variables to OntoEMS concepts
READINGS_MAPPING = {
    'Nitrogen dioxide in air': EMS_NO2_CONCENTRATION,
    'Nitrogen oxides in air': EMS_NOX_CONCENTRATION,
    'Nitrogen monoxide in air': EMS_NO_CONCENTRATION,
    'PM10 in aerosol': EMS_PM10_CONCENTRATION,
    'Particulate matter under 2.5 micro m (aerosol)': EMS_PM2_5_CONCENTRATION,
    'Sulphur dioxide in air': EMS_SO2_CONCENTRATION,
    'Ozone in air': EMS_O3_CONCENTRATION,
}

# Mapping of units to OM units and symbols
UNITS_MAPPING = {
    'mg.m-3': (OM_MILLIG_M3, 'mg/m3'),
    'ug.m-3': (OM_MICROG_M3, 'μg/m3'), 
    'ng.m-3': (OM_NANOG_M3, 'ng/m3'),
}

# Times are reported in ISO 8601 dateTime (UTC)
TIME_FORMAT = '%Y-%m-%dT%H:%M:%SZ'

# Create data class for all time series data (i.e. all data as double)
jpsBaseLibView = jpsBaseLibGW.createModuleView()
DATACLASS = jpsBaseLibView.java.lang.Double.TYPE
