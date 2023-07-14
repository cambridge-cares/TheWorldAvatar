# Provide Constants/IRIs, which can be used to query and update the KG

# Namespaces
# External ontologies
ONTOCAPE = "http://www.theworldavatar.com/ontology/ontocape/"
METAMODEL = "http://www.theworldavatar.com/ontology/meta_model/"
OM = "http://www.ontology-of-units-of-measure.org/resource/om-2/"
ONTOPOWSYS = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#"
ONTOCHEMPLANT = "http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#"
RDF = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
RDFS = "http://www.w3.org/2000/01/rdf-schema#"
XSD = "http://www.w3.org/2001/XMLSchema#"
TIME = "http://www.w3.org/2006/time#"
# CoMo / CARES ontologies
OHN     = "https://www.theworldavatar.com/kg/ontoheatnetwork/"
ONTOEMS = "https://www.theworldavatar.com/kg/ontoems/"
TS      = "https://www.theworldavatar.com/kg/ontotimeseries/"
# Knowledge base
KB = "https://www.theworldavatar.com/kg/pms_dh/"


### TS ###
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

### Time  ###
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

### OM ###
OM_HASVALUE = OM + "hasValue"
OM_HASUNIT = OM + "hasUnit"

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
