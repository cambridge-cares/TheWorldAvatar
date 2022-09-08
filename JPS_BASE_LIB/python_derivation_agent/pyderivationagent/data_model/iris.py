###--- Common Base URL ---###
RDFS_BASE_URL = 'http://www.w3.org/2000/01/rdf-schema#'
RDF_BASE_URL = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#'
XSD_BASE_URL = 'http://www.w3.org/2001/XMLSchema#'
OWL_BASE_URL = 'http://www.w3.org/2002/07/owl#'
UNITS_OF_MEASURE = 'http://www.ontology-of-units-of-measure.org/resource/om-2/'
W3C_TIME = 'http://www.w3.org/2006/time#'
DBPEDIA = 'https://dbpedia.org/ontology/'
SAREF = 'https://saref.etsi.org/core/'
ONTOUOM = 'http://theworldavatar.com/resource/ontouom/'
ONTOAGENT = 'http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#'
ONTODERIVATION = 'https://raw.githubusercontent.com/cambridge-cares/TheWorldAvatar/main/JPS_Ontology/ontology/ontoderivation/OntoDerivation.owl#'

###--- Common PREFIX for SPARQL query ---###
PREFIX_RDFS = 'PREFIX rdfs:   <' + RDFS_BASE_URL + '> '
PREFIX_RDF = 'PREFIX rdf:    <' + RDF_BASE_URL + '> '
PREFIX_XSD = 'PREFIX xsd:    <' + XSD_BASE_URL + '> '
PREFIX_OWL = 'PREFIX owl:    <' + OWL_BASE_URL + '> '
PREFIX_OM = 'PREFIX om:     <' + UNITS_OF_MEASURE + '> '

###--- Common IRI for units of measure ---###
OM_MEASURE = UNITS_OF_MEASURE + 'Measure'
OM_HASPHENOMENON = UNITS_OF_MEASURE + 'hasPhenomenon'
OM_HASVALUE = UNITS_OF_MEASURE + 'hasValue'
OM_HASNUMERICALVALUE = UNITS_OF_MEASURE + 'hasNumericalValue'
OM_HASUNIT = UNITS_OF_MEASURE + 'hasUnit'

###--- Common IRI for Time ---###
TIME_HASTIME = W3C_TIME + 'hasTime'
TIME_NUMERICPOSITION = W3C_TIME + 'numericPosition'
TIME_HASTRS = W3C_TIME + 'hasTRS'
TIME_INTIMEPOSITION = W3C_TIME + 'inTimePosition'

###--- Common IRI for OntoAgent ---###
ONTOAGENT_HASOPERATION = ONTOAGENT + 'hasOperation'
ONTOAGENT_HASINPUT = ONTOAGENT + 'hasInput'
ONTOAGENT_HASMANDATORYPART = ONTOAGENT + 'hasMandatoryPart'
ONTOAGENT_HASTYPE = ONTOAGENT + 'hasType'
ONTOAGENT_HASNAME = ONTOAGENT + 'hasName'
ONTOAGENT_SERVICE = ONTOAGENT + "Service"
ONTOAGENT_OPERATION = ONTOAGENT + "Operation"
ONTOAGENT_MESSAGECONTENT = ONTOAGENT + "MessageContent"
ONTOAGENT_MESSAGEPART = ONTOAGENT + "MessagePart"
ONTOAGENT_HASHTTPURL = ONTOAGENT + "hasHttpUrl"
ONTOAGENT_HASOUTPUT = ONTOAGENT + "hasOutput"

###--- Common IRI for OntoDerivation ---###
ONTODERIVATION_ISDERIVEDFROM = ONTODERIVATION + 'isDerivedFrom'
ONTODERIVATION_DERIVATION = ONTODERIVATION + 'Derivation'
ONTODERIVATION_DERIVATIONWITHTIMESERIES = ONTODERIVATION + 'DerivationWithTimeSeries'
ONTODERIVATION_DERIVATIONASYN = ONTODERIVATION + 'DerivationAsyn'
ONTODERIVATION_BELONGSTO = ONTODERIVATION + 'belongsTo'
ONTODERIVATION_ISDERIVEDUSING = ONTODERIVATION + 'isDerivedUsing'
ONTODERIVATION_HASSTATUS = ONTODERIVATION + 'hasStatus'
ONTODERIVATION_REQUESTED = ONTODERIVATION + 'Requested'
ONTODERIVATION_INPROGRESS = ONTODERIVATION + 'InProgress'
ONTODERIVATION_FINISHED = ONTODERIVATION + 'Finished'
ONTODERIVATION_HASNEWDERIVEDIRI = ONTODERIVATION + 'hasNewDerivedIRI'
ONTODERIVATION_RETRIEVEDINPUTSAT = ONTODERIVATION + 'retrievedInputsAt'
