# Provide Constants/IRIs, which can be used to query and update the KG

# Namespaces
# External ontologies
OM = "http://www.ontology-of-units-of-measure.org/resource/om-2/"
OWL = "http://www.w3.org/2002/07/owl#"
RDF = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
RDFS = "http://www.w3.org/2000/01/rdf-schema#"
XSD = "http://www.w3.org/2001/XMLSchema#"
TIME = "http://www.w3.org/2006/time#"
# CoMo / CARES ontologies
ONTOCHEMPLANT = "http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#"
ONTOPOWSYS = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#"
ONTOCAPE = "http://www.theworldavatar.com/ontology/ontocape/"
METAMODEL = "http://www.theworldavatar.com/ontology/meta_model/"
ONTOEMS = "https://www.theworldavatar.com/kg/ontoems/"
TS      = "https://www.theworldavatar.com/kg/ontotimeseries/"
OHN     = "https://www.theworldavatar.com/kg/ontoheatnetwork/"
# Knowledge base
KB = "https://www.theworldavatar.com/kg/pms_dh/"


### TimeSeries ###
TS_TIMESERIES = TS + "TimeSeries"
TS_FORECAST = TS + "Forecast"
TS_FORECASTINGMODEL = TS + "ForecastingModel"
TS_HASTIMESERIES = TS + "hasTimeSeries"
TS_HASFORECAST = TS + "hasForecast"
TS_HASURL = TS + "hasURL"
TS_HASRDB = TS + "hasRDB"
TS_HASINPUTTIMEINTERVAL = TS + "hasInputTimeInterval"
TS_HASOUTPUTTIMEINTERVAL = TS + "hasOutputTimeInterval"
TS_HASTIMEUNIT = TS + "hasTimeUnit"
TS_HASCOVARIATE = TS + "hasCovariate"
TS_HASFORECASTINGMODEL = TS + "hasForecastingModel"
TS_HASTRAININGTIMESERIES = TS + "hasTrainingTimeSeries"
TS_FREQUENCY = TS + 'Frequency'

### Time ###
TIME_INTERVAL = TIME + 'Interval'
TIME_HASBEGINNING = TIME + 'hasBeginning'
TIME_HASEND = TIME + 'hasEnd'
TIME_INSTANT = TIME + 'Instant'
TIME_INTIMEPOSITION = TIME + 'inTimePosition'
TIME_TIMEPOSITION = TIME + 'TimePosition'
TIME_HASTRS = TIME + 'hasTRS'
UNIX_TIME = "http://dbpedia.org/resource/Unix_time"
TIME_NUMERICPOSITION = TIME + 'numericPosition'
TIME_HASTIME = TIME + 'hasTime'
TIME_DURATION = TIME + 'Duration'
TIME_NUMERICDURATION = TIME + 'numericDuration'
TIME_UNIT_TYPE = TIME + 'unitType'
TIME_UNIT_DAY = TIME + 'unitDay'
TIME_UNIT_HOUR = TIME + 'unitHour'
TIME_UNIT_MINUTE = TIME + 'unitMinute'
TIME_UNIT_SECOND = TIME + 'unitSecond'

### OM ###
OM_HASVALUE = OM + "hasValue"
OM_HASUNIT = OM + "hasUnit"

### OWL, RDF, RDFS ###
OWL_THING = OWL + "Thing"
RDF_TYPE = RDF + 'type'
RDFS_LABEL = RDFS + 'label'

# Data types
XSD_STRING = XSD + 'string'
XSD_INTEGER = XSD + 'decimal'
XSD_INTEGER = XSD + 'integer'
#TODO: Verify which date types are still needed ultimately
XSD_DATE = XSD + "date"
XSD_DATETIME = XSD + 'dateTime'
XSD_DATETIMESTAMP = XSD + "dateTimeStamp"

### ONTOEMS ###
#TODO: verify if still needed after removing method to get covariates
ONTOEMS_AIRTEMPERATURE = ONTOEMS + "AirTemperature"

### OM ###
OM_EURO = OM + 'euro'
OM_MEGAWATT = OM + 'megawatt'

### OHN ###
OHN_ISPUBLICHOLIDAY = OHN + "isPublicHoliday"
