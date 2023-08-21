# Provide Constants/IRIs, which can be used to query and update the KG

# Namespaces
# External ontologies
OM =   'http://www.ontology-of-units-of-measure.org/resource/om-2/'
OWL =  'http://www.w3.org/2002/07/owl#'
RDF =  'http://www.w3.org/1999/02/22-rdf-syntax-ns#'
RDFS = 'http://www.w3.org/2000/01/rdf-schema#'
XSD =  'http://www.w3.org/2001/XMLSchema#'
TIME = 'http://www.w3.org/2006/time#'
# CoMo / CARES ontologies
ONTOCHEMPLANT = 'http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#'
ONTOPOWSYS =    'http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#'
ONTOCAPE =      'http://www.theworldavatar.com/ontology/ontocape/'
ONTODERIV =     'https://www.theworldavatar.com/kg/ontoderivation/'
TS =            'https://www.theworldavatar.com/kg/ontotimeseries/'
OD =            'https://www.theworldavatar.com/kg/ontodispersion/'
OHN =           'https://www.theworldavatar.com/kg/ontoheatnetwork/'
# Knowledge base
KB =            'https://www.theworldavatar.com/kg/pirmasens/'


### OntoHeatNetwrok ###
OHN_PROVIDED_HEAT_AMOUNT = OHN + 'ProvidedHeatAmount'
OHN_CONSUMED_GAS_AMOUNT = OHN + 'ConsumedGasAmount'

### OntoDispersion ###
OD_SIMULATION_TIME = OD + 'SimulationTime'
OD_STATIC_POINT_SOURCE = OD + 'StaticPointSource'
OD_EMISSION = OD + 'Emission'
OD_HAS_POLLUTANT_ID = OD + 'hasPollutantID'
OD_NO2 = OD + 'NO2'
OD_NOX = OD + 'NOx'
OD_PM2_5 = OD + 'PM2.5'
OD_PM10 = OD + 'PM10'
OD_EMITS = OD + 'emits'

### TimeSeries ###
TS_TIMESERIES = TS + 'TimeSeries'
TS_FORECAST = TS + 'Forecast'
TS_HASTIMESERIES = TS + 'hasTimeSeries'
TS_HASFORECAST = TS + 'hasForecast'
TS_HASURL = TS + 'hasURL'
TS_HASRDB = TS + 'hasRDB'

### Time ###
TIME_INSTANT = TIME + 'Instant'
TIME_INTIMEPOSITION = TIME + 'inTimePosition'
TIME_TIMEPOSITION = TIME + 'TimePosition'
TIME_HASTRS = TIME + 'hasTRS'
UNIX_TIME = 'http://dbpedia.org/resource/Unix_time'
TIME_NUMERICPOSITION = TIME + 'numericPosition'
TIME_HASTIME = TIME + 'hasTime'

### OM ###
OM_HAS_QUANTITY = OM + 'hasQuantity'
OM_TEMPERATURE = OM + 'Temperature'
OM_DENSITY = OM + 'Density'
OM_MASSFLOW = OM + 'MassFlow'
OM_HASVALUE = OM + 'hasValue'
OM_MEASURE = OM + 'Measure'
OM_HAS_NUMERICAL_VALUE = OM + 'hasNumericalValue'
OM_HASUNIT = OM + 'hasUnit'
OM_MEGAWATTHOUR = OM + 'megawattHour'
OM_KELVIN = OM + 'kelvin'
OM_KG_PER_M3 = OM + 'kilogramPerCubicmetre'
OM_KG_PER_S = OM + 'kilogramPerSecond-Time'

### OWL, RDF, RDFS ###
OWL_THING = OWL + 'Thing'
RDF_TYPE = RDF + 'type'
RDFS_LABEL = RDFS + 'label'

# Data types
XSD_STRING = XSD + 'string'
XSD_FLOAT = XSD + 'float'
XSD_DECIMAL = XSD + 'decimal'
XSD_INTEGER = XSD + 'integer'
