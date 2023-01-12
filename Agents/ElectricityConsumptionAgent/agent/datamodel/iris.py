################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk) #
# Date: 30/11 2022                            #
################################################

# The purpose of this module is to provide IRIS for
# required SPARQL queries (Mainly used in 'querytemplates.py' module)

###--- --- --- --- --- URLs to ontology .owl files --- --- --- --- ---###

GAS = "http://www.theworldavatar.com/ontology/ontogasgrid/ontogasgrid.owl#"
COMP = "http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#"
COMPA = "http://www.theworldavatar.com/kb/ontogasgrid/offtakes_abox/"
CLIMB =  "http://www.theworldavatar.com/ontology/ontogasgrid/ontoclimate.owl#"
OFP = "http://www.theworldavatar.com/ontology/ontofuelpoverty/ontofuelpoverty.owl#"
OFPT = "http://www.theworldavatar.com/kb/ontofuelpoverty/abox/"
CLIMA =  "http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/"
###--- --- --- --- --- Common Base URLs --- --- --- --- ---###

# External ontologies
RDF = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
RDFS = "http://www.w3.org/2000/01/rdf-schema#"
XSD = "http://www.w3.org/2001/XMLSchema#"
GEO = "http://www.opengis.net/ont/geosparql#"
OM = "http://www.ontology-of-units-of-measure.org/resource/om-2/"
OWL = "http://www.w3.org/2002/07/owl#"

# ONS related
ONS_ID = "http://statistics.data.gov.uk/id/statistical-geography/"
ONS_DEF = "http://statistics.data.gov.uk/def/statistical-geography#"
ONS_DEF_STAT= ONS_DEF + "Statistical-Geography"
# CoMo / CARES ontologies
TS = "https://www.theworldavatar.com/kg/ontotimeseries/"

###--- --- --- --- --- URLs to ontologies --- --- --- --- ---###

# OntoGasgrid
COMP_HASCONSUMED = COMP + 'hasConsumed'
COMP_HASUSED = COMP + 'hasUsed'
COMP_ELEC = COMP + "Electricity"
COMP_HAS_STARTUTC = COMP + "hasStartUTC"
COMP_HAS_ENDUTC = COMP + "hasEndUTC"
COMP_OFFTAKENGAS = COMP + "OfftakenGas"

GAS_ELEC = GAS + "ElecMeters"
GAS_GASMETER = GAS + "GasMeters"
GAS_HAS_ELECMETERS = GAS + "hasElecMeters"
GAS_HAS_CONSUM_ELECMETERS = GAS + "hasConsumingElecMeters"
GAS_HAVE_GASMETERS = GAS + "hasGasMeters"
GAS_HAVE_CONSUM_GASMETERS = GAS +"hasConsumingGasMeters"
GAS_HAVE_NONCONSUM_GASMETERS = GAS +"hasNonConsumingGasMeters"

# Ontofuelpoverty
OFP_HASHOUSEHOLD = OFP + 'hasHouseholds'
OFP_FUELPOOR = OFP + 'fuelpoorhouseholds '
OFP_NUMBEROFHOUSEHOLD = OFP + 'numberofhouseholds'
OFP_VALIDFROM = OFP + "validFrom"
OFP_VALIDTO = OFP + "validTo"

# OntoClimate
CLIMB_HASMEASURE = CLIMB + "hasClimateMeasurement"
CLIMB_HASVAR = CLIMB + "hasClimateVariable"
CLIMB_CLIMATEMEASUREMENT = CLIMB + "ClimateMeasurement"
CLIMB_CLIMBVARIABLE = CLIMB + "ClimateVariable"

# OntoTimeSeries
TS_TIMESERIES = TS + "TimeSeries"
TS_HAS_TIMESERIES = TS + "hasTimeSeries"
TS_HAS_TIME_UNIT = TS + "hasTimeUnit"
TS_HAS_RDB = TS + "hasRDB"

# Ontology of units of measure
OM_QUANTITY = OM + "Quantity"
OM_ENERGY = OM + "Energy"
OM_MEASURE = OM + "Measure"
OM_UNIT = OM + "Unit"
OM_TEMPERATURE = OM + "Temperature"

OM_HAS_VALUE = OM + "hasValue"
OM_HAS_NUMERICALVALUE = OM +"hasNumericalValue"
OM_HAS_UNIT = OM + "hasUnit"
OM_HAS_PHENO = OM + "hasPhenomenon"

OM_SYMBOL = OM + "symbol"
OM_DEGREE_C = OM + "degreeCelsius"
OM_HECTO_PASCAL = OM + "hectopascal"
OM_PERCENT = OM + "percent"
OM_METRE = OM + "metre"
OM_MPH = OM + "mile-StatutePerHour"
OM_DEGREE = OM + "degree"
OM_UNITLESS = OM + "one"
OM_KW = OM + "kilowattHour"

# Data types
RDF_TYPE = RDF + "type"
RDFS_COMMENT = RDFS + "comment"
RDFS_LABEL = RDFS + "label"
XSD_STRING = XSD + "string"
XSD_FLOAT = XSD + "float"
XSD_DATETIME = XSD + "dateTime"

# GeoSPARQL
GEO_FEATURE = GEO + "Feature"
GEO_GEOMETRY = GEO + 'Geometry'
GEO_HAS_GEOMETRY = GEO + "hasGeometry"
GEO_ASWKT = GEO + "asWKT"

# OWL
OWL_VERSION = OWL + "versionInfo"
OWL_SAMEAS = OWL + "sameAs"
