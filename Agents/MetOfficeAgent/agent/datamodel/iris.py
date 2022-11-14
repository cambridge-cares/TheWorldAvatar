from agent.datamodel.observation_types import *

# URLs to ontology .owl files
TBOX_URL = 'http://www.theworldavatar.com/ontology/ontoems/OntoEMS.owl'
ABOX_URL = 'http://www.theworldavatar.com/kb/ontoems/OntoEMS.owl'

###--- Common Base URLs ---###
# External ontologies
RDF = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#'
RDFS = 'http://www.w3.org/2000/01/rdf-schema#'
XSD = 'http://www.w3.org/2001/XMLSchema#'
GEO = 'http://www.opengis.net/ont/geosparql#'
OM = 'http://www.ontology-of-units-of-measure.org/resource/om-2/'
OWL = 'http://www.w3.org/2002/07/owl#'
M3L = 'http://purl.org/iot/vocab/m3-lite#'
WEATHER = 'https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#'
SIO = 'http://semanticscience.org/resource/'
# CoMo / CARES ontologies
EMS = 'https://www.theworldavatar.com/kg/ontoems/'
TS = 'https://www.theworldavatar.com/kg/ontotimeseries/'
# Knowledge base
KB = 'https://www.theworldavatar.com/kg/ontoems/'


###--- IRIs for OntoEMS ---###
EMS_REPORTING_STATION = EMS + 'ReportingStation'
EMS_DATA_SOURCE = EMS + 'dataSource'
EMS_HAS_IDENTIFIER = EMS + 'hasIdentifier'
EMS_HAS_OBSERVATION_LOCATION = EMS + 'hasObservationLocation'
EMS_HAS_OBSERVATION_ELEVATION = EMS + 'hasObservationElevation'
EMS_REPORTS = EMS + 'reports'
EMS_FORECAST = EMS + 'Forecast'
EMS_HAS_FORECASTED_VALUE = EMS + 'hasForecastedValue'
EMS_CREATED_ON = EMS + 'createdOn'
EMS_WATER_LEVEL = EMS + WATER_LEVEL
EMS_RAINFALL = EMS + RAINFALL
EMS_WATER_FLOW = EMS + WATER_FLOW
EMS_AIR_TEMPERATURE = EMS + AIR_TEMPERATURE
EMS_WIND_SPEED = EMS + WIND_SPEED
EMS_WIND_GUST = EMS + WIND_GUST
EMS_WIND_DIRECTION = EMS + WIND_DIRECTION
EMS_FEELS_LIKE_TEMPERATURE = EMS + FEELS_LIKE_TEMPERATURE
EMS_DEW_POINT = EMS + DEW_POINT
EMS_ATMOSPHERIC_PRESSURE = EMS + ATMOSPHERIC_PRESSURE
EMS_RELATIVE_HUMIDITY = EMS + RELATIVE_HUMIDITY
EMS_UV_INDEX = EMS + UV_INDEX
EMS_VISIBILITY = EMS + VISIBILITY
EMS_PRECIPITATION_PROBABILITY = EMS + PRECIPITATION_PROBABILITY
EMS_AIR_POLLUTANT_CONCENTRATION = EMS + AIR_POLLUTANT_CONCENTRATION
EMS_DIRECT_HORIZONTAL_IRRADIANCE = EMS + DIRECT_HORIZONTAL_IRRADIANCE
EMS_DIFFUSE_HORIZONTAL_IRRADIANCE = EMS + DIFFUSE_HORIZONTAL_IRRADIANCE
EMS_GLOBAL_HORIZONTAL_IRRADIANCE = EMS + GLOBAL_HORIZONTAL_IRRADIANCE
# OntoTimeSeries
TS_TIMESERIES = TS + 'TimeSeries'
TS_HAS_TIMESERIES = TS + 'hasTimeSeries'
TS_HAS_TIME_UNIT = TS +'hasTimeUnit'
TS_HAS_RDB = TS + 'hasRDB'
# Ontology of units of measure
OM_QUANTITY = OM + 'Quantity'
OM_MEASURE = OM + 'Measure'
OM_UNIT = OM + 'Unit'
OM_HAS_VALUE = OM + 'hasValue'
OM_HAS_UNIT = OM + 'hasUnit'
OM_SYMBOL = OM + 'symbol'
OM_DEGREE_C = OM + 'degreeCelsius'
OM_HECTO_PASCAL = OM + 'hectopascal'
OM_PERCENT = OM + 'percent'
OM_METRE = OM + 'metre'
OM_MPH = OM + 'mile-StatutePerHour'
OM_DEGREE = OM + 'degree'
OM_UNITLESS = OM + 'one'
# Data types
RDF_TYPE = RDF + 'type'
RDFS_COMMENT = RDFS + 'comment'
RDFS_LABEL = RDFS + 'label'
XSD_STRING = XSD + 'string'
XSD_FLOAT = XSD + 'float'
XSD_DATETIME = XSD + 'dateTime'
# GeoSPARQL
GEO_FEATURE = GEO + 'Feature'
GEO_HAS_GEOMETRY = GEO + 'hasGeometry'
GEO_ASWKT = GEO + 'asWKT'
# OWL
OWL_VERSION = OWL + 'versionInfo'
OWL_SAMEAS = OWL + 'sameAs'
