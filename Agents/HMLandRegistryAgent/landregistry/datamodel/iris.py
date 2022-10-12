###--- Common Base URLs ---###
# External ontologies
DABGEO = 'http://www.purl.org/oema/infrastructure/'
GEO = 'http://www.opengis.net/ont/geosparql#'
ICONTACT = 'http://ontology.eil.utoronto.ca/icontact.owl#'
LRPPI = 'http://landregistry.data.gov.uk/def/ppi/'
OM = 'http://www.ontology-of-units-of-measure.org/resource/om-2/'
ONS1 = 'http://statistics.data.gov.uk/def/statistical-geography#'
ONS2 = 'http://statistics.data.gov.uk/def/hierarchy/best-fit#'
OWL = 'http://www.w3.org/2002/07/owl#'
RDF = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#'
RDFS = 'http://www.w3.org/2000/01/rdf-schema#'
TIME = 'https://www.w3.org/2006/time#'
XSD = 'http://www.w3.org/2001/XMLSchema#'
# CoMo / CARES ontologies
OBE = 'https://www.theworldavatar.com/kg/ontobuiltenv/'
TS = 'https://www.theworldavatar.com/kg/ontotimeseries/'
UOM = 'http://theworldavatar.com/resource/ontouom/'
OCGML = 'http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#'
OSID = 'http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoOSID.owl#'
# Knowledge base
KB = 'https://www.theworldavatar.com/kg/ontobuiltenv/'

###--- IRIs for OntoBuiltEnv TBox ---###
# http://www.theworldavatar.com/ontology/ontobuiltenv/OntoBuiltEnv.owl
OBE_PROPERTY = OBE + 'Property'
OBE_BUILDING = DABGEO + 'Building'
OBE_FLAT = OBE + 'Flat'
OBE_IS_IN = OBE + 'isIn'
OBE_HAS_ADDRESS = OBE + 'hasAddress'
OBE_HAS_POSTALCODE = OBE + 'hasPostalCode'
OBE_POSTALCODE = OBE + 'PostalCode'
OBE_HAS_PROPERTYNUMBER = OBE + 'hasPropertyNumber'
OBE_HAS_ADMIN_DISTRICT = OBE + 'hasAdministrativeDistrict'
OBE_ADMIN_DISTRICT = OBE + 'AdministrativeDistrict'
OBE_LOCATEDIN = OBE + 'locatedIn'
OBE_HAS_IDENTIFIER = OBE + 'hasIdentifier'
OBE_HAS_LATEST_TRANSACTION = OBE + 'hasLatestTransactionRecord'
# LRPPI
LRPPI_TRANSACTION = LRPPI + 'TransactionRecord'
LRPPI_PRICE = LRPPI + 'pricePaid'
LRPPI_DATE = LRPPI + 'transactionDate'


###--- External IRIs ---###

# Concepts
RDF_TYPE = RDF + 'type'
RDFS_COMMENT = RDFS + 'comment'
RDFS_LABEL = RDFS + 'label'

# Data types
XSD_STRING = XSD + 'string'
XSD_FLOAT = XSD + 'float'
XSD_INTEGER = XSD + 'integer'
XSD_DATE = XSD + 'date'
XSD_DATETIMESTAMP = XSD + 'dateTimeStamp'

# OntoTimeSeries
TS_TIMESERIES = TS + 'TimeSeries'
TS_HAS_TIMESERIES = TS + 'hasTimeSeries'
TS_HAS_TIME_UNIT = TS +'hasTimeUnit'
TS_HAS_RDB = TS + 'hasRDB'

# ICONTACT ontology
ICONTACT_ADDRESS = ICONTACT + 'Address'
ICONTACT_HAS_STREET = ICONTACT + 'hasStreet'

# Office for National Statistics (ONS)
ONS_GEOGRAPGY = ONS1 + 'Statistical-Geography'
ONS_NAME = ONS1 + 'officialname'
ONS_BEST_LOCAL_AUTHORITY = ONS2 + 'localauthoritydistrict'
