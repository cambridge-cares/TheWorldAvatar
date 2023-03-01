from agent.datamodel.observation_types import *
###--- Ontology ---#
TBOX = 'https://raw.githubusercontent.com/cambridge-cares/TheWorldAvatar/main/JPS_Ontology/ontology/ontoems/OntoEMS.owl'
ABOX = 'https://raw.githubusercontent.com/cambridge-cares/TheWorldAvatar/main/JPS_Ontology/ontology/ontoems/OntoEMS%20ABox.owl'

###--- Common Base URLs ---###
# External ontologies
RDF = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#'
RDFS = 'http://www.w3.org/2000/01/rdf-schema#'
XSD = 'http://www.w3.org/2001/XMLSchema#'
GEO = 'http://www.bigdata.com/rdf/geospatial#'
GEOLIT = 'http://www.bigdata.com/rdf/geospatial/literals/v1#'
OM = 'http://www.ontology-of-units-of-measure.org/resource/om-2/'
OWL = 'http://www.w3.org/2002/07/owl#'
M3L = 'http://purl.org/iot/vocab/m3-lite#'
WEATHER = 'https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#'
SIO = 'http://semanticscience.org/resource/'
# CoMo / CARES ontologies
EMS = 'https://www.theworldavatar.com/kg/ontoems/'
TS = 'https://www.theworldavatar.com/kg/ontotimeseries/'
UOM = 'https://www.theworldavatar.com/kg/ontouom/'
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
EMS_AIR_TEMPERATURE = EMS + AIR_TEMPERATURE
EMS_WIND_SPEED = EMS + WIND_SPEED
EMS_WIND_GUST = EMS + WIND_GUST
EMS_WIND_DIRECTION = EMS + WIND_DIRECTION
EMS_FEELS_LIKE_TEMPERATURE = EMS + FEELS_LIKE_TEMPERATURE
EMS_DEW_POINT = EMS + DEW_POINT
EMS_ATMOSPHERIC_PRESSURE = EMS + ATMOSPHERIC_PRESSURE
EMS_RELATIVE_HUMIDITY = EMS + RELATIVE_HUMIDITY
EMS_AIR_POLLUTANT_CONCENTRATION = EMS + AIR_POLLUTANT_CONCENTRATION
EMS_SO2_CONCENTRATION = EMS + SO2_CONCENTRATION
EMS_NOX_CONCENTRATION = EMS + NOX_CONCENTRATION
EMS_NO_CONCENTRATION = EMS + NO_CONCENTRATION
EMS_NO2_CONCENTRATION = EMS + NO2_CONCENTRATION
EMS_PM_CONCENTRATION = EMS + PM_CONCENTRATION
EMS_PM2_5_CONCENTRATION = EMS +PM2_5_CONCENTRATION
EMS_PM10_CONCENTRATION = EMS + PM10_CONCENTRATION
EMS_O3_CONCENTRATION = EMS + O3_CONCENTRATION

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
OM_MPH = OM + 'mile-StatutePerHour'
OM_DEGREE = OM + 'degree'
OM_MILLIG_M3 = UOM + 'milligramPerCubicmetre'
# OM extension
OM_MICROG_M3 = UOM + 'microgramPerCubicMetre'
OM_NANOG_M3 = UOM + 'nanogramPerCubicMetre'

# Data types
RDF_TYPE = RDF + 'type'
RDFS_COMMENT = RDFS + 'comment'
RDFS_LABEL = RDFS + 'label'
XSD_STRING = XSD + 'string'
XSD_FLOAT = XSD + 'float'
XSD_DATETIME = XSD + 'dateTime'
GEOLIT_LAT_LON = GEOLIT + 'lat-lon'

# Miscellaneous
SAMEAS = OWL + 'sameAs'
OWL_VERSION = OWL + 'versionInfo'