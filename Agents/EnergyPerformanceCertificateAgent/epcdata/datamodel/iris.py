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
OBE_HAS_WGS84_LATLON = OBE + 'hasWGS84LatitudeLongitude'
OBE_HAS_IDENTIFIER = OBE + 'hasIdentifier'
OBE_HAS_ENERGYRATING = OBE + 'hasEnergyRating'
OBE_HAS_NUMBER_ROOMS = OBE + 'hasNumberOfHabitableRooms'
OBE_HAS_USAGE = OBE + 'hasUsageCategory'
OBE_HAS_PROPERTY_TYPE = OBE + 'hasPropertyType'
OBE_HAS_BUILT_FORM = OBE + 'hasBuiltForm'
OBE_HAS_CONSTRUCTION_DATE = OBE + 'hasConstructionDate'
OBE_HAS_TOTAL_FLOOR_AREA = OBE + 'hasTotalFloorArea'
OBE_HAS_GROUND_ELEVATION = OBE + 'hasGroundElevation'
OBE_HAS_CONSTRUCTION_COMPONENT = OBE + 'hasConstructionComponent'
OBE_WALL = OBE + 'Wall'
OBE_ROOF = OBE + 'Roof'
OBE_WINDOWS = OBE + 'Windows'
OBE_FLOOR = OBE + 'Floor'
OBE_HAS_LATEST_EPC = OBE + 'hasLatestEPC'
OBE_HAS_OCGML_REPRESENTATION = OBE + 'hasOntoCityGMLRepresentation'
OBE_HAS_LATEST_TRANSACTION = OBE + 'hasLatestTransactionRecord'
# LRPPI: https://landregistry.data.gov.uk/app/root/doc/ppd
LRPPI_TRANSACTION_RECORD = LRPPI + 'TransactionRecord'
LRPPI_PRICE = LRPPI + 'pricePaid'
LRPPI_DATE = LRPPI + 'transactionDate'

###--- IRIs for OntoBuiltEnv ABox ---###
# http://www.theworldavatar.com/kb/ontobuiltenv/OntoBuiltEnv.owl
# Built form
OBE_DETACHED = OBE + 'Detached_b5997912-1035-4284-ad40-cde44e438485'
OBE_SEMI_DETACHED = OBE + 'Semi-Detached_b5997912-1035-4284-ad40-cde44e438485'
OBE_TERRACED = OBE + 'Terraced_b5997912-1035-4284-ad40-cde44e438485'
# Property types
OBE_HOUSE = OBE + 'House_b5997912-1035-4284-ad40-cde44e438485'
OBE_BUNGALOW = OBE + 'Bungalow_b5997912-1035-4284-ad40-cde44e438485'
OBE_PARKHOME = OBE + 'ParkHome_b5997912-1035-4284-ad40-cde44e438485'
OBE_MAISONETTE = OBE + 'Maisonette_b5997912-1035-4284-ad40-cde44e438485'
# Property usage category
OBE_DOMESTIC = OBE + 'Domestic_b5997912-1035-4284-ad40-cde44e438485'
OBE_NON_DOMESTIC = OBE + 'Non-Domestic_b5997912-1035-4284-ad40-cde44e438485'
OBE_SINGLERESIDENTIAL = OBE + 'SingleResidential_b5997912-1035-4284-ad40-cde44e438485'
OBE_MULTIRESIDENTIAL = OBE + 'MultiResidential_b5997912-1035-4284-ad40-cde44e438485'
OBE_EMERGENCYSERVICE = OBE + 'EmergencyService_b5997912-1035-4284-ad40-cde44e438485'
OBE_MEDICALCARE = OBE + 'MedicalCare_b5997912-1035-4284-ad40-cde44e438485'
OBE_EDUCATION = OBE + 'Education_b5997912-1035-4284-ad40-cde44e438485'
OBE_OFFICE = OBE + 'Office_b5997912-1035-4284-ad40-cde44e438485'
OBE_RETAILESTABLISHMENT = OBE + 'RetailEstablishment_b5997912-1035-4284-ad40-cde44e438485'
OBE_RELIGIOUSFACILITY = OBE + 'ReligiousFacility_b5997912-1035-4284-ad40-cde44e438485'
OBE_INDUSTRIALFACILITY = OBE + 'IndustrialFacility_b5997912-1035-4284-ad40-cde44e438485'
OBE_EATINGESTABLISHMENT = OBE + 'EatingEstablishment_b5997912-1035-4284-ad40-cde44e438485'
OBE_DRINKINGESTABLISHMENT = OBE + 'DrinkingEstablishment_b5997912-1035-4284-ad40-cde44e438485'
OBE_HOTEL = OBE + 'Hotel_b5997912-1035-4284-ad40-cde44e438485'
OBE_SPORTSFACILITY = OBE + 'SportsFacility_b5997912-1035-4284-ad40-cde44e438485'
OBE_CULTURALFACILITY = OBE + 'CulturalFacility_b5997912-1035-4284-ad40-cde44e438485'
OBE_TRANSPORTFACILITY = OBE + 'TransportFacility_b5997912-1035-4284-ad40-cde44e438485'
OBE_FIRESTATION = OBE + 'FireStation_b5997912-1035-4284-ad40-cde44e438485'
OBE_POLICESTATION = OBE + 'PoliceStation_b5997912-1035-4284-ad40-cde44e438485'
OBE_HOSPITAL = OBE + 'Hospital_b5997912-1035-4284-ad40-cde44e438485'
OBE_CLINIC = OBE + 'Clinic_b5997912-1035-4284-ad40-cde44e438485'
OBE_SCHOOL = OBE + 'School_b5997912-1035-4284-ad40-cde44e438485'
OBE_UNIVERSITY = OBE + 'University_b5997912-1035-4284-ad40-cde44e438485'

###--- OntoCityGml TBox ---###
OCGML_BLDG_HEIGHT = OCGML + 'measuredHeigh'     # typo intended (part of OCGML ontology)
OCGML_BLDG_HEIGHT_UNIT = OCGML + 'measuredHeightUnit'
OCGML_FOOTPRINT = OCGML + 'lod0FootprintId'
OCGML_GEOM_TYPE = OCGML + 'GeometryType'
OCGML_ROOT_ID = OCGML + 'rootId'
OCGML_SRSNAME = OCGML + 'srsname'
# OntoCityGml / OSID
OSID_INTERSECTS_FEATURE = OSID + 'intersectsFeature'
OSID_HAS_VALUE = OSID + 'hasValue'


###--- External IRIs ---###

# Concepts
RDF_TYPE = RDF + 'type'
RDFS_COMMENT = RDFS + 'comment'
RDFS_LABEL = RDFS + 'label'
OWL_VERSION = OWL + 'versionInfo'
OWL_SAMEAS = OWL + 'sameAs'

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

# Time ontology
TIME_INTERVAL = TIME + 'Interval'
TIME_HAS_BEGINNING = TIME + 'hasBeginning'
TIME_HAS_END = TIME + 'hasEnd'
TIME_INSTANT = TIME + 'Instant'
TIME_IN_DATETIME_STAMP = TIME + 'inXSDDateTimeStamp'

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

# GeoSPARQL
GEO_FEATURE = GEO + 'Feature'
GEO_HAS_GEOMETRY = GEO + 'hasGeometry'
GEO_ASWKT = GEO + 'asWKT'

# ICONTACT ontology
ICONTACT_ADDRESS = ICONTACT + 'Address'
ICONTACT_HAS_STREET = ICONTACT + 'hasStreet'
ICONTACT_HAS_STREET_NUMBER = ICONTACT + 'hasStreetNumber'
ICONTACT_HAS_BUILDING = ICONTACT + 'hasBuilding'

# Office for National Statistics (ONS)
ONS_GEOGRAPGY = ONS1 + 'Statistical-Geography'
ONS_NAME = ONS1 + 'officialname'
ONS_BEST_LOCAL_AUTHORITY = ONS2 + 'localauthoritydistrict'
