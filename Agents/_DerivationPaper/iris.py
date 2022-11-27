# Specify frequently used IRIs
RDF = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#'
RDFS = 'http://www.w3.org/2000/01/rdf-schema#'
XSD = 'http://www.w3.org/2001/XMLSchema#'
UKHPI = 'http://landregistry.data.gov.uk/def/ukhpi/'
OBE = 'https://www.theworldavatar.com/kg/ontobuiltenv/'
KB = 'https://www.theworldavatar.com/kg/ontobuiltenv/'

RDF_TYPE = RDF + 'type'
RDFS_LABEL = RDFS + 'label'
XSD_STRING = XSD + 'string'

UKHPI_REFREGION = UKHPI + 'refRegion'
UKHPI_INDEX = UKHPI + 'housePriceIndex'
UKHPI_REF_MONTH = UKHPI + 'refMonth'

OBE_REPRESENTATIVE_FOR = OBE + 'representativeFor'
OBE_PROPERTY_PRICE_INDEX = OBE + 'PropertyPriceIndex'

# HARDCODED INSTANCE IRIS
# (i.e. IRIs of previously instantiated instances to avoid unnecessary querying)
district_iri = 'https://www.theworldavatar.com/kg/ontobuiltenv/AdministrativeDistrict_b240d043-4d79-4660-8005-3074ecb84176'
# Specify IRI for property price index to be instantiated (there will be only one for
# entire district; hence, hardcoding avoids necessity to query for IRI later on)
ppi_iri = KB + 'PropertyPriceIndex_KingsLynn4'