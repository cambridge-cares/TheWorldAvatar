##############################################################################
# Authors: Feroz Farazi (msff2@cam.ac.uk), John Atherton (ja685@cam.ac.uk)   #
# Date: 02 July 2023                                                         #
##############################################################################

###--- Defines common base URLs ---###
RDF = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#'
RDFS = 'http://www.w3.org/2000/01/rdf-schema#'
XSD = 'http://www.w3.org/2001/XMLSchema#'
OM = 'http://www.ontology-of-units-of-measure.org/resource/om-2/'
OWL = 'http://www.w3.org/2002/07/owl#'
ONTO_ENERGY_SYSTEM = 'http://www.theworldavatar.com/ontology/ontoenergysystem/OntoEnergySystem.owl#'
ONTO_ENERGY_SYSTEM_KB = 'https://www.theworldavatar.com/kg/ontoenergysystem/'
ONTO_POW_SYS = 'http://www.theworldavatar.com/ontology/ontopowsys/electrical_system.owl#'
ONTO_EIP = 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#'

# CoMo / CARES ontologies
TS = 'https://www.theworldavatar.com/kg/ontotimeseries/'

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

# Data types
RDF_TYPE = RDF + 'type'
RDFS_COMMENT = RDFS + 'comment'
RDFS_LABEL = RDFS + 'label'
XSD_STRING = XSD + 'string'
XSD_FLOAT = XSD + 'float'
XSD_DATETIME = XSD + 'dateTime'

# OWL
OWL_VERSION = OWL + 'versionInfo'
OWL_SAMEAS = OWL + 'sameAs'
