###--- TBox and ABox URL ---###
TBOX_URL = 'https://raw.githubusercontent.com/cambridge-cares/TheWorldAvatar/main/JPS_Ontology/ontology/ontobuiltenv/OntoBuiltEnv.owl'
# alternative: 'http://www.theworldavatar.com/ontology/ontobuiltenv/OntoBuiltEnv.owl'

###--- Derivation Markup ---###
DERIVATION_INSTANCE_BASE_URL = 'https://www.theworldavatar.com/kg/derivation/'


###--- Common Base URLs ---###
# External ontologies
DABGEO = 'http://www.purl.org/oema/infrastructure/'
GEO = 'http://www.opengis.net/ont/geosparql#'
ICONTACT = 'http://ontology.eil.utoronto.ca/icontact.owl#'
OM = 'http://www.ontology-of-units-of-measure.org/resource/om-2/'
OWL = 'http://www.w3.org/2002/07/owl#'
RDF = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#'
RDFS = 'http://www.w3.org/2000/01/rdf-schema#'
SKOS = 'http://www.w3.org/2004/02/skos/core#'
TIME = 'https://www.w3.org/2006/time#'
XSD = 'http://www.w3.org/2001/XMLSchema#'
# Office for National Statistics
ONS1 = 'http://statistics.data.gov.uk/def/statistical-geography#'
ONS2 = 'http://statistics.data.gov.uk/def/hierarchy/best-fit#'
# HM Land Registry
LRPPI = 'http://landregistry.data.gov.uk/def/ppi/'
LRCOMMON = 'http://landregistry.data.gov.uk/def/common/'
UKHPI = 'http://landregistry.data.gov.uk/def/ukhpi/'
# CoMo / CARES ontologies
OBE = 'https://www.theworldavatar.com/kg/ontobuiltenv/'
TS = 'https://www.theworldavatar.com/kg/ontotimeseries/'
UOM = 'https://www.theworldavatar.com/kg/ontouom/'
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
OBE_HAS_UNIT_NAME = OBE + 'hasUnitName'
OBE_POSTALCODE = OBE + 'PostalCode'
OBE_IS_PRESUMED_MATCH_OF = OBE + 'isPresumedMatchOf'
OBE_HAS_ADMIN_DISTRICT = OBE + 'hasAdministrativeDistrict'
OBE_ADMIN_DISTRICT = OBE + 'AdministrativeDistrict'
OBE_LOCATEDIN = OBE + 'locatedIn'
OBE_HAS_IDENTIFIER = OBE + 'hasIdentifier'
OBE_HAS_LATEST_TRANSACTION = OBE + 'hasLatestTransactionRecord'
OBE_REPRESENTATIVE_FOR = OBE + 'representativeFor'
OBE_PROPERTY_PRICE_INDEX = OBE + 'PropertyPriceIndex'
OBE_HAS_OCGML_REPRESENTATION = OBE + 'hasOntoCityGMLRepresentation'
OBE_HAS_GROUND_ELEVATION = OBE + 'hasGroundElevation'
OBE_AVERAGE_SM_PRICE = OBE + 'AveragePricePerSqm'
OBE_HAS_TOTAL_FLOORAREA = OBE + 'hasTotalFloorArea'
OBE_HASMARKETVALUE = OBE + 'hasMarketValue'
# LRPPI: https://landregistry.data.gov.uk/app/root/doc/ppd
LRPPI_TRANSACTION_RECORD = LRPPI + 'TransactionRecord'
LRPPI_PROPERTY_ADDRESS = LRPPI + 'propertyAddress'
LRPPI_PRICE = LRPPI + 'pricePaid'
LRPPI_DATE = LRPPI + 'transactionDate'
LRPPI_PROPERTY_TYPE = LRPPI + 'propertyType'
LRPPI_TX_CATEGORY = LRPPI + 'transactionCategory'
SKOS_LABEL = SKOS + 'prefLabel'
LRCOMMON_PAON = LRCOMMON + 'paon'
LRCOMMON_SAON = LRCOMMON + 'saon'
LRCOMMON_STREET = LRCOMMON + 'street'
LRCOMMON_TOWN = LRCOMMON + 'town'
LRCOMMON_POSTCODE = LRCOMMON + 'postcode'
LRCOMMON_DISTRICT = LRCOMMON + 'district'
LRCOMMON_COUNTY = LRCOMMON + 'county'
UKHPI_REFREGION = UKHPI + 'refRegion'
UKHPI_INDEX = UKHPI + 'housePriceIndex'
UKHPI_REF_MONTH = UKHPI + 'refMonth'

###--- External IRIs ---###

# Concepts
RDF_TYPE = RDF + 'type'
RDFS_SUBCLASS = RDFS + 'subClassOf'
RDFS_COMMENT = RDFS + 'comment'
RDFS_LABEL = RDFS + 'label'
RDFS_SEEALSO = RDFS + 'seeAlso'
OWL_SAME_AS = OWL + 'sameAs'

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
ICONTACT_HAS_STREET_NUMBER = ICONTACT + 'hasStreetNumber'
ICONTACT_HAS_BUILDING = ICONTACT + 'hasBuilding'

# Office for National Statistics (ONS)
ONS_GEOGRAPGY = ONS1 + 'Statistical-Geography'
ONS_NAME = ONS1 + 'officialname'
ONS_BEST_LOCAL_AUTHORITY = ONS2 + 'localauthoritydistrict'

# OWL
OWL_VERSION = OWL + 'versionInfo'

# Ontology of units of measure
OM_AREA = OM + 'Area'
OM_HEIGHT = OM + 'Height'
OM_AMOUNT_MONEY = OM + 'AmountOfMoney'
OM_HAS_VALUE = OM + 'hasValue'
OM_MEASURE = OM + 'Measure'
OM_NUM_VALUE = OM + 'hasNumericalValue'
OM_HAS_UNIT = OM + 'hasUnit'
OM_UNIT = OM + 'Unit'
OM_SYMBOL = OM + 'symbol'
OM_M = OM + 'metre'
OM_M2 = OM + 'squareMetre'
OM_GBP = OM + 'poundSterling'
UOM_GBP_M2 =  UOM + 'pound_sterling_per_sqm'

###--- OntoCityGml TBox ---###
OCGML_BLDG_HEIGHT = OCGML + 'measuredHeigh'     # typo intended (part of OCGML ontology)
OCGML_BLDG_HEIGHT_UNIT = OCGML + 'measuredHeightUnit'
OCGML_FOOTPRINT = OCGML + 'lod0FootprintId'
OCGML_GEOM_TYPE = OCGML + 'GeometryType'
OCGML_ROOT_ID = OCGML + 'rootId'
OCGML_SRSNAME = OCGML + 'srsname'