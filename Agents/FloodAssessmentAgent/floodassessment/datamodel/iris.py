###--- Common Base URLs ---###
# External ontologies
RDF = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#'
RDFS = 'http://www.w3.org/2000/01/rdf-schema#'
DABGEO = 'http://www.purl.org/oema/infrastructure/'
OM = 'http://www.ontology-of-units-of-measure.org/resource/om-2/'
XSD = 'http://www.w3.org/2001/XMLSchema#'
# Environment Agency
RT = 'http://environment.data.gov.uk/flood-monitoring/def/core/'
# CoMo / CARES ontologies
OBE = 'https://www.theworldavatar.com/kg/ontobuiltenv/'
FLOOD = 'https://www.theworldavatar.com/kg/ontoflood/'
UOM = 'https://www.theworldavatar.com/kg/ontouom/'
# Knowledge base
KB = 'https://www.theworldavatar.com/kg/ontoflood/'

###--- IRIs for OntoBuiltEnv ---###
OBE_PROPERTY = OBE + 'Property'
OBE_BUILDING = DABGEO + 'Building'
#OBE_FLAT = OBE + 'Flat'
OBE_HAS_MARKET_VALUE = OBE + 'hasMarketValue'

###--- IRIs for OntoFlood ---###
FLOOD_ALERT_WARNING = RT + 'FloodAlertOrWarning'
FLOOD_HAS_SEVERITY = FLOOD + 'hasSeverity'
FLOOD_SEVERITY = FLOOD + 'Severity'
FLOOD_WARNS_ABOUT = FLOOD + 'warnsAbout'
FLOOD_RESULTS_IN = FLOOD + 'resultsIn'
FLOOD_IMPACT = FLOOD + 'Impact'
FLOOD_HAS_CLASSIFICATION = FLOOD + 'hasClassification'
FLOOD_HAS_MONETARY_VALUE = FLOOD + 'hasMonetaryValue'
FLOOD_HAS_TOTAL_MONETARY_VALUE = FLOOD + 'hasTotalMonetaryValue'
FLOOD_AFFECTS = FLOOD + 'affects'
FLOOD_POPULATION = FLOOD + 'Population'
FLOOD_BUILDINGS = FLOOD + 'Buildings'
FLOOD_HAS_TOTAL_COUNT = FLOOD + 'hasTotalCount'
# Flood concepts from ENVO (https://ontobee.org/ontology/ENVO)
FLOOD_FLOOD = 'http://purl.obolibrary.org/obo/ENVO_01000710'
FLOOD_COASTAL_FLOOD = 'http://purl.obolibrary.org/obo/ENVO_01000711'
FLOOD_RIVERINE_FLOOD = 'http://purl.obolibrary.org/obo/ENVO_01000712'
FLOOD_FLASH_FLOOD = 'http://purl.obolibrary.org/obo/ENVO_01000713'
# Severities from OntoFlood ABox
FLOOD_SEVERITY_SEVERE = FLOOD + 'SevereFloodWarning_ca5e5580-7ab8-4e1c-9087-8cbc893d5c5b'       # Severity level 1
FLOOD_SEVERITY_WARNING = FLOOD + 'FloodWarning_ca5e5580-7ab8-4e1c-9087-8cbc893d5c5b'            # Severity level 2
FLOOD_SEVERITY_ALERT = FLOOD + 'FloodAlert_ca5e5580-7ab8-4e1c-9087-8cbc893d5c5b'                # Severity level 3
FLOOD_SEVERITY_INACTIVE = FLOOD + 'InactiveFloodWarning_ca5e5580-7ab8-4e1c-9087-8cbc893d5c5b'   # Severity level 4

###--- External IRIs ---###
# Ontology of units of measure
OM_AMOUNT_MONEY = OM + 'AmountOfMoney'
OM_HAS_VALUE = OM + 'hasValue'
OM_MEASURE = OM + 'Measure'
OM_NUM_VALUE = OM + 'hasNumericalValue'
OM_HAS_UNIT = OM + 'hasUnit'
OM_UNIT = OM + 'Unit'
OM_SYMBOL = OM + 'symbol'
OM_GBP = OM + 'poundSterling'

# OM / UOM unit symbols
# NOTE: There are reported issues with encoding of special characters, i.e. Blazegraph
#       claiming to use utf-8 encoding while actually using iso-8859-1
#       --> PoundSterling displayed wrongly in GUI but corrected when retrieved in code
# Details: https://github.com/blazegraph/database/issues/224
GBP_SYMBOL = '£'

# Data types
XSD_FLOAT = XSD + 'float'
XSD_STRING = XSD + 'string'
XSD_INTEGER = XSD + 'integer'

# Object properties
RDF_TYPE = RDF + 'type'
RDFS_SUBCLASS = RDFS + 'subClassOf'

# Data properties
RDFS_LABEL = RDFS + 'label'
RDFS_COMMENT = RDFS + 'comment'