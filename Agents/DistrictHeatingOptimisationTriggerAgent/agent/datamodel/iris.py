# Provide Constants/IRIs for KG queries/updates

from pyderivationagent.data_model.iris import ONTODERIVATION_ISDERIVEDFROM,\
                                              ONTODERIVATION_BELONGSTO, \
                                              ONTODERIVATION_DERIVATION, \
                                              ONTODERIVATION_DERIVATIONWITHTIMESERIES

# Namespaces
# External ontologies
RDF = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
XSD = "http://www.w3.org/2001/XMLSchema#"
TIME = "http://www.w3.org/2006/time#"
OM = "http://www.ontology-of-units-of-measure.org/resource/om-2/"
# CoMo / CARES ontologies
OD =  'https://www.theworldavatar.com/kg/ontodispersion/'
TS =  'https://www.theworldavatar.com/kg/ontotimeseries/'
OHN = 'https://www.theworldavatar.com/kg/ontoheatnetwork/'
EMS = "https://www.theworldavatar.com/kg/ontoems/"
KB = 'https://www.theworldavatar.com/kg/pms_dh/'
# Derivation markup
DERIVATION_INSTANCE_BASE_URL = 'https://www.theworldavatar.com/kg/derivation/'

# Forecasting related instances #
# NOTE: to be aligned with fcmodels.ttl
fc_model_heat_demand = KB + 'ForecastingModel_TFT_heat_demand'
fc_model_grid_temperature = KB + 'ForecastingModel_Prophet'
# Static point source instances #
# NOTE: to be aligned with static_point_sources.ttl
point_source_mu = KB + 'StaticPointSource_MunicipalUtility'
point_source_efw = KB + 'StaticPointSource_EfWPlant'

### Concept/Data types ###
RDF_TYPE = RDF + 'type'
XSD_DECIMAL = XSD + 'decimal'

### OntoDispersion ###
OD_SIMULATION_TIME = OD + 'SimulationTime'

# OntoTimeSeries
TS_FREQUENCY = TS + 'Frequency'
TS_FORECASTINGMODEL = TS + "ForecastingModel"
TS_HASCOVARIATE = TS + "hasCovariate"
TS_FORECAST = TS + 'Forecast'

### Time ontology ###
TIME_INTERVAL = TIME + 'Interval'
TIME_HASBEGINNING = TIME + 'hasBeginning'
TIME_HASEND = TIME + 'hasEnd'
TIME_INSTANT = TIME + 'Instant'
TIME_INTIMEPOSITION = TIME + 'inTimePosition'
TIME_TIMEPOSITION = TIME + 'TimePosition'
TIME_HASTRS = TIME + 'hasTRS'
TIME_NUMERICPOSITION = TIME + 'numericPosition'
UNIX_TIME = "http://dbpedia.org/resource/Unix_time"
TIME_DURATION = TIME + 'Duration'
TIME_NUMERICDURATION = TIME + 'numericDuration'
TIME_UNIT_TYPE = TIME + 'unitType'
TIME_UNIT_DAY = TIME + 'unitDay'
TIME_UNIT_HOUR = TIME + 'unitHour'
TIME_UNIT_MINUTE = TIME + 'unitMinute'
TIME_UNIT_SECOND = TIME + 'unitSecond'

### ONTOEMS ###
EMS_AIRTEMPERATURE = EMS + "AirTemperature"
EMS_REPORTS = EMS + 'reports'

### OHN ###
OHN_HEAT_DEMAND = OHN + "HeatDemand"
OHN_INCINERATIONPLANT = OHN + "IncinerationPlant"
OHN_MUNICIPAL_UTILITY = OHN + "MunicipalUtility"
OHN_HAS_DOWNSTREAM_GRIDCONNECTION = OHN + "hasDownstreamGridConnection"
OHN_HAS_UPSTREAM_GRIDCONNECTION = OHN + "hasUpstreamGridConnection"
OHN_GRIDCONNECTION = OHN + "GridConnection"
OHN_HAS_OBSERVABLE_PROPERTY = OHN + "hasObservableProperty"
OHN_ISPUBLICHOLIDAY = OHN + "isPublicHoliday"
OHN_PROVIDED_HEAT_AMOUNT = OHN + "ProvidedHeatAmount"
OHN_CONSUMED_GAS_AMOUNT = OHN + "ConsumedGasAmount" 

### OM ###
OM_TEMPERATURE = OM + "Temperature"
OM_HAS_VALUE = OM + "hasValue"