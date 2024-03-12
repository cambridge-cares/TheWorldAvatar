################################################
# Authors: Jiying Chen (jc2341)                #
# Date: 07 March 2024                          #
################################################

# The purpose of this module is to provide IRIS for required SPARQL queries 

###--- --- --- --- --- URLs to ontology .owl files --- --- --- --- ---###



######--- --- --- --- --- URLs to ontologies --- --- --- --- ---######


ontodevice = "https://www.theworldavatar.com/kg/ontodevice.owl"
fhr = "http://www.theworldavatar.com/ontology/OntoFHRS"
ts = "https://www.theworldavatar.com/kg/ontotimeseries/"
ABOX_URL_ontodevice = 'https://www.theworldavatar.com/kg/ontodevice'

# External ontologies
RDF = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
RDFS = "http://www.w3.org/2000/01/rdf-schema#"
XSD = "http://www.w3.org/2001/XMLSchema#"
GEO = "http://www.opengis.net/ont/geosparql#"
OM = "http://www.ontology-of-units-of-measure.org/resource/om-2/"
OWL = "http://www.w3.org/2002/07/owl#"

# OntoTimeSeries
TS_TIMESERIES = ts + "TimeSeries"
TS_HAS_TIMESERIES = ts + "hasTimeSeries"
TS_HAS_TIME_UNIT = ts + "hasTimeUnit"
TS_HAS_RDB = ts + "hasRDB"

# Ontology of units of measure
OM_QUANTITY = OM + "Quantity"
OM_ENERGY = OM + "Energy"
OM_MEASURE = OM + "Measure"
OM_UNIT = OM + "Unit"
OM_HAS_VALUE = OM + "hasValue"
OM_HAS_NUMERICALVALUE = OM +"hasNumericalValue"
OM_HAS_UNIT = OM + "hasUnit"
OM_DEGREE = OM + "degree"

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