###--- Common Base URLs ---###
# External ontologies
RDF = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#'
RDFS = 'http://www.w3.org/2000/01/rdf-schema#'
DABGEO = 'http://www.purl.org/oema/infrastructure/'
OM = 'http://www.ontology-of-units-of-measure.org/resource/om-2/'
XSD = 'http://www.w3.org/2001/XMLSchema#'
# Office for National Statistics (ONS)
ONS1 = 'http://statistics.data.gov.uk/def/spatialrelations/'
ONS2 = 'http://statistics.data.gov.uk/def/postcode/'
# HM Land Registry
LRPPI = 'http://landregistry.data.gov.uk/def/ppi/'
UKHPI = 'http://landregistry.data.gov.uk/def/ukhpi/'
# CoMo / CARES ontologies
OBE = 'https://www.theworldavatar.com/kg/ontobuiltenv/'
UOM = 'https://www.theworldavatar.com/kg/ontouom/'
# Knowledge base
KB = 'https://www.theworldavatar.com/kg/ontobuiltenv/'

###--- IRIs for OntoBuiltEnv TBox ---###
# http://www.theworldavatar.com/ontology/ontobuiltenv/OntoBuiltEnv.owl
OBE_ADMIN_DISTRICT = OBE + 'AdministrativeDistrict'
OBE_AVERAGE_SM_PRICE = OBE + 'AveragePricePerSqm'
OBE_BUILDING = DABGEO + 'Building'
OBE_FLAT = OBE + 'Flat'
OBE_HAS_ADDRESS = OBE + 'hasAddress'
OBE_HAS_ADMIN_DISTRICT = OBE + 'hasAdministrativeDistrict'
OBE_HAS_IDENTIFIER = OBE + 'hasIdentifier'
OBE_HAS_LATEST_TRANSACTION = OBE + 'hasLatestTransactionRecord'
OBE_HAS_POSTALCODE = OBE + 'hasPostalCode'
OBE_HAS_TOTAL_FLOOR_AREA = OBE + 'hasTotalFloorArea'
OBE_HAS_UNIT_NAME = OBE + 'hasUnitName'
OBE_IS_IN = OBE + 'isIn'
OBE_IS_PRESUMED_MATCH_OF = OBE + 'isPresumedMatchOf'
OBE_LOCATEDIN = OBE + 'locatedIn'
OBE_POSTALCODE = OBE + 'PostalCode'
OBE_PROPERTY = OBE + 'Property'
OBE_PROPERTY_PRICE_INDEX = OBE + 'PropertyPriceIndex'
OBE_REPRESENTATIVE_FOR = OBE + 'representativeFor'
# LRPPI: https://landregistry.data.gov.uk/app/root/doc/ppd
LRPPI_DATE = LRPPI + 'transactionDate'
LRPPI_PRICE = LRPPI + 'pricePaid'
LRPPI_PROPERTY_ADDRESS = LRPPI + 'propertyAddress'
LRPPI_PROPERTY_TYPE = LRPPI + 'propertyType'
LRPPI_TRANSACTION_RECORD = LRPPI + 'TransactionRecord'
LRPPI_TX_CATEGORY = LRPPI + 'transactionCategory'

###--- External IRIs ---###

# Ontology of units of measure
OM_AMOUNT_MONEY = OM + 'AmountOfMoney'
OM_AREA = OM + 'Area'
OM_HAS_UNIT = OM + 'hasUnit'
OM_HAS_VALUE = OM + 'hasValue'
OM_M2 = OM + 'squareMetre'
OM_MEASURE = OM + 'Measure'
OM_NUM_VALUE = OM + 'hasNumericalValue'
OM_SYMBOL = OM + 'symbol'
OM_UNIT = OM + 'Unit'
UOM_GBP_M2 =  UOM + 'pound_sterling_per_sqm'

# ONS
ONS_EASTING = ONS1 + 'epsg-27700/easting'
ONS_NORTHING = ONS1 + 'epsg-27700/northing'
ONS_POSTCODE = ONS2 + 'unit'
ONS_WITHIN_SOSA = ONS1 + 'within#superoutputarealowerlayer'

# UKHPI
UKHPI_INDEX = UKHPI + 'housePriceIndex'
UKHPI_REFREGION = UKHPI + 'refRegion'
UKHPI_REF_MONTH = UKHPI + 'refMonth'

# Concepts
RDFS_COMMENT = RDFS + 'comment'
RDFS_LABEL = RDFS + 'label'
RDFS_SEEALSO = RDFS + 'seeAlso'
RDFS_SUBCLASS = RDFS + 'subClassOf'
RDF_TYPE = RDF + 'type'

# Data types
XSD_DATE = XSD + 'date'
XSD_DATETIMESTAMP = XSD + 'dateTimeStamp'
XSD_FLOAT = XSD + 'float'
XSD_INTEGER = XSD + 'integer'
XSD_STRING = XSD + 'string'
