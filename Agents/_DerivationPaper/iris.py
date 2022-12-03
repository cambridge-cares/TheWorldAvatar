# Specify frequently used IRIs
RDF = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#'
RDFS = 'http://www.w3.org/2000/01/rdf-schema#'
XSD = 'http://www.w3.org/2001/XMLSchema#'
# UK API namespaces
OM = 'http://www.ontology-of-units-of-measure.org/resource/om-2/'
UKHPI = 'http://landregistry.data.gov.uk/def/ukhpi/'
RT = 'http://environment.data.gov.uk/flood-monitoring/def/core/'
DABGEO = 'http://www.purl.org/oema/infrastructure/'
# CoMo/CARES namespaces
OBE = 'https://www.theworldavatar.com/kg/ontobuiltenv/'
OF = 'https://www.theworldavatar.com/kg/ontoflood/'
KB = 'https://www.theworldavatar.com/kg/ontobuiltenv/'

RDF_TYPE = RDF + 'type'
RDFS_LABEL = RDFS + 'label'
XSD_STRING = XSD + 'string'

UKHPI_REFREGION = UKHPI + 'refRegion'
UKHPI_INDEX = UKHPI + 'housePriceIndex'
UKHPI_REF_MONTH = UKHPI + 'refMonth'
RT_FLOOD_ALERT_WARNING = RT + 'FloodAlertOrWarning'

# TransactionRecord
PPI = 'http://landregistry.data.gov.uk/def/ppi/'
PPI_TRANSACTIONRECORD = PPI + 'TransactionRecord'

# OM_AMOUNT_MONEY
OM_AMOUNT_MONEY = OM + 'AmountOfMoney'
OM_HAS_VALUE = OM + 'hasValue'
OM_HAS_NUMERICAL_VALUE = OM + 'hasNumericalValue'

# From OntoBuiltEnv TBox
OBE_POSTALCODE = OBE + 'PostalCode'
OBE_PROPERTY = OBE + 'Property'
OBE_BUILDING = DABGEO + 'Building'
OBE_HASPOSTALCODE = OBE + 'hasPostalCode'
OBE_HASADMINISTRATIVEDISTRICT = OBE + 'hasAdministrativeDistrict'
OBE_HASADDRESS = OBE + 'hasAddress'
OBE_HASLATESTTRANSACTIONRECORD = OBE + 'hasLatestTransactionRecord'
OBE_AVERAGE_SM_PRICE = OBE + 'AveragePricePerSqm'
OBE_REPRESENTATIVE_FOR = OBE + 'representativeFor'
OBE_PROPERTY_PRICE_INDEX = OBE + 'PropertyPriceIndex'
OBE_HASTOTALFLOORAREA = OBE + 'hasTotalFloorArea'
OBE_HASMARKETVALUE = OBE + 'hasMarketValue'
OBE_HASWGS84LATITUDELONGITUDE = OBE + 'hasWGS84LatitudeLongitude'

# From OntoFlood TBox
OF_HAS_SEVERITY = OF + 'hasSeverity'
# From OntoFlood ABox
OF_FLOOD_WARNING = OF + 'FloodWarning_ca5e5580-7ab8-4e1c-9087-8cbc893d5c5b'

# HARDCODED INSTANCE IRIS
# (i.e. IRIs of previously instantiated instances to avoid unnecessary querying)
district_iri = 'https://www.theworldavatar.com/kg/ontobuiltenv/AdministrativeDistrict_b240d043-4d79-4660-8005-3074ecb84176'
# Specify IRIs to be instantiated (there will only be one for each concept; 
# hence, hardcoding avoids necessity to query for IRI later on)
ppi_iri = KB + 'PropertyPriceIndex_KingsLynn'
flood_warning_iri = KB + 'FloodWarning_KingsLynn'
