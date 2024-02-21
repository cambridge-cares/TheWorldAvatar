###--- TheWorldAvatar Base Prefix ---###
# To be used by attaching specific namespace and class name to it
# e.g. https://www.theworldavatar.com/kg/namespace/ClassName
TWA_BASE_PREFIX = 'https://www.theworldavatar.com/kg/'

###--- Common Base URL ---###
RDFS_BASE_URL = 'http://www.w3.org/2000/01/rdf-schema#'
RDF_BASE_URL = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#'
XSD_BASE_URL = 'http://www.w3.org/2001/XMLSchema#'
OWL_BASE_URL = 'http://www.w3.org/2002/07/owl#'
UNITS_OF_MEASURE = 'http://www.ontology-of-units-of-measure.org/resource/om-2/'
W3C_TIME = 'http://www.w3.org/2006/time#'
DBPEDIA = 'https://dbpedia.org/ontology/'
SAREF = 'https://saref.etsi.org/core/'

###--- IRI for Geo-Spatial Data ---###
GEOSPATIAL_LAT_LON_TIME = 'http://www.bigdata.com/rdf/geospatial/literals/v1#lat-lon-time'

###--- Common PREFIX for SPARQL query ---###
PREFIX_RDFS = 'PREFIX rdfs:   <' + RDFS_BASE_URL + '> '
PREFIX_RDF = 'PREFIX rdf:    <' + RDF_BASE_URL + '> '
PREFIX_XSD = 'PREFIX xsd:    <' + XSD_BASE_URL + '> '
PREFIX_OWL = 'PREFIX owl:    <' + OWL_BASE_URL + '> '
PREFIX_OM = 'PREFIX om:     <' + UNITS_OF_MEASURE + '> '

OWL = 'http://www.w3.org/2002/07/owl#'
OWL_VERSION = OWL + 'versionInfo'

ONTODOE = TWA_BASE_PREFIX + 'ontodoe/'
ONTOREACTION = TWA_BASE_PREFIX + 'ontoreaction/'
ONTOGOAL = TWA_BASE_PREFIX + 'ontogoal/'
ONTOKIN = 'http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#'
ONTOSPECIES = 'http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#'
ONTOLAB = TWA_BASE_PREFIX + 'ontolab/'
ONTOVAPOURTEC = TWA_BASE_PREFIX + 'ontovapourtec/'
ONTOHPLC = TWA_BASE_PREFIX + 'ontohplc/'
ONTOAGENT = 'http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#'
ONTODERIVATION = TWA_BASE_PREFIX + 'ontoderivation/'
