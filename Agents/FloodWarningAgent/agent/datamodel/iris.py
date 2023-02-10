# URLs to ontology .owl files
TBOX_URL = 'https://github.com/cambridge-cares/TheWorldAvatar/raw/main/JPS_Ontology/ontology/ontoflood/OntoFlood TBox.owl'
ABOX_URL = 'https://github.com/cambridge-cares/TheWorldAvatar/raw/main/JPS_Ontology/ontology/ontoflood/OntoFlood ABox.owl'

###--- Common Base URLs ---###
# External ontologies
RDF = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#'
RDFS = 'http://www.w3.org/2000/01/rdf-schema#'
XSD = 'http://www.w3.org/2001/XMLSchema#'
GEO = 'http://www.opengis.net/ont/geosparql#'
OM = 'http://www.ontology-of-units-of-measure.org/resource/om-2/'
OWL = 'http://www.w3.org/2002/07/owl#'
# CoMo / CARES ontologies
FLOOD = 'https://www.theworldavatar.com/kg/ontoflood/'
BUILT = 'https://www.theworldavatar.com/kg/ontobuiltenv/'
TS = 'https://www.theworldavatar.com/kg/ontotimeseries/'
# Knowledge base
KB = 'https://www.theworldavatar.com/kg/ontoflood/'


###--- IRIs for OntoFlood ---###

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


# OM / UOM unit symbols
OM_GBP = OM + 'poundSterling'
# NOTE: There are reported issues with encoding of special characters, i.e. Blazegraph
#       claiming to use utf-8 encoding while actually using iso-8859-1
#       --> PoundSterling displayed wrongly in GUI but correctly retrieved within code
# Details: https://github.com/blazegraph/database/issues/224
GBP_SYMBOL = '£'