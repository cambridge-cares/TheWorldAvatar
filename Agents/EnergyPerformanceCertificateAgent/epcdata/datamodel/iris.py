from epcdata.datamodel.usage_types import *

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
TS = 'https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_Ontology/ontology/ontotimeseries/OntoTimeSeries.owl#'
UOM = 'http://theworldavatar.com/resource/ontouom/'
# Knowledge base
KB = 'https://www.theworldavatar.com/kg/ontoems/'

###--- IRIs for OntoBuiltEnv TBox ---###
# http://www.theworldavatar.com/ontology/ontobuiltenv/OntoBuiltEnv.owl
OBE_ADMIN_DISTRICT = OBE + 'AdministrativeDistrict'
OBE_POSTALCODE = OBE + 'PostalCode'

###--- IRIs for OntoBuiltEnv ABox ---###
# http://www.theworldavatar.com/kb/ontobuiltenv/OntoBuiltEnv.owl

OBE_DETACHED = OBE + 'Detached_b5997912-1035-4284-ad40-cde44e438485'
OBE_SEMI_DETACHED = OBE + 'Semi-Detached_b5997912-1035-4284-ad40-cde44e438485'
OBE_TERRACED = OBE + 'Terraced_b5997912-1035-4284-ad40-cde44e438485'
OBE_HOUSE = OBE + 'House_b5997912-1035-4284-ad40-cde44e438485'
OBE_BUNGALOW = OBE + 'Bungalow_b5997912-1035-4284-ad40-cde44e438485'
OBE_PARKHOME = OBE + 'ParkHome_b5997912-1035-4284-ad40-cde44e438485'
OBE_MAISONETTE = OBE + 'Maisonette_b5997912-1035-4284-ad40-cde44e438485'
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
OM_SYMBOL = OM + 'symbol'

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

# GeoSPARQL
GEO_FEATURE = GEO + 'Feature'
GEO_HAS_GEOMETRY = GEO + 'hasGeometry'
GEO_ASWKT = GEO + 'asWKT'

# Office for National Statistics (ONS)
ONS_GEOGRAPGY = ONS1 + 'Statistical-Geography'
ONS_NAME = ONS1 + 'officialname'
ONS_BEST_LOCAL_AUTHORITY = ONS2 + 'localauthoritydistrict'
