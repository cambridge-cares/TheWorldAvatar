################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 03 Apr 2023                            #
################################################

# The purpose of this module is to provide a mapping between retrieved 
# observation variables from UK-AIR Sensor Observation Service to corresponding
# concepts and units as defined in OntoEMS

from agent.datamodel.iris import *
from agent.datamodel.observation_types import *
from agent.kgutils.javagateway import jpsBaseLibGW

# Create Java classes for all time series data
jpsBaseLibView = jpsBaseLibGW.createModuleView()
# Time entries (Instant)
Instant = jpsBaseLibView.java.time.Instant
TIMECLASS = Instant.now().getClass()

# Mapping of observation variables to OntoEMS concepts
READINGS_MAPPING = {
    'nitrogen dioxide (air)': (NO2_CONCENTRATION, EMS_NO2_CONCENTRATION),
    'nitrogen oxides (air)': (NOX_CONCENTRATION, EMS_NOX_CONCENTRATION),
    'nitrogen monoxide (air)': (NO_CONCENTRATION, EMS_NO_CONCENTRATION),
    'sulphur dioxide (air)': (SO2_CONCENTRATION, EMS_SO2_CONCENTRATION),
    'ozone (air)': (O3_CONCENTRATION, EMS_O3_CONCENTRATION),
    # Multiple (ambiguous) particulate matter descriptions provided by API
    'volatile pm2.5': (PM2_5_CONCENTRATION, EMS_PM2_5_CONCENTRATION),
    'particulate matter under 2.5 micro m (aerosol)': (PM2_5_CONCENTRATION, EMS_PM2_5_CONCENTRATION),
    'particulate matter less than 2.5 micro m (aerosol)': (PM2_5_CONCENTRATION, EMS_PM2_5_CONCENTRATION),
    'pm10 in aerosol': (PM10_CONCENTRATION, EMS_PM10_CONCENTRATION),
    'volatile pm10': (PM10_CONCENTRATION, EMS_PM10_CONCENTRATION),    
    'particulate matter under 10 micro m (aerosol)': (PM10_CONCENTRATION, EMS_PM10_CONCENTRATION),
    'particulate matter less than 10 micro m (aerosol)': (PM10_CONCENTRATION, EMS_PM10_CONCENTRATION),
}

# Mapping of units to OM units and symbols
UNITS_MAPPING = {
    'mg.m-3': OM_MILLIG_M3,
    'ug.m-3': OM_MICROG_M3, 
    'ng.m-3': OM_NANOG_M3
}

# Times are reported in ISO 8601 dateTime (UTC)
TIME_FORMAT = '%Y-%m-%dT%H:%M:%SZ'

# # Create data class for all time series data (i.e. all data as double)
# jpsBaseLibView = jpsBaseLibGW.createModuleView()
DATACLASS = jpsBaseLibView.java.lang.Double.TYPE
