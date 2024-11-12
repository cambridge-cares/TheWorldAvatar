# URLs to ontology .owl files
TBOX_URL = 'https://github.com/cambridge-cares/TheWorldAvatar/raw/main/JPS_Ontology/ontology/ontoflood/OntoFlood TBox.owl'
ABOX_URL = 'https://github.com/cambridge-cares/TheWorldAvatar/raw/main/JPS_Ontology/ontology/ontoflood/OntoFlood ABox.owl'

###--- Derivation Markup ---###
DERIVATION_INSTANCE_BASE_URL = 'https://www.theworldavatar.com/kg/derivation/'

###--- Common Base URLs ---###
# External ontologies
RDF = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#'
RDFS = 'http://www.w3.org/2000/01/rdf-schema#'
XSD = 'http://www.w3.org/2001/XMLSchema#'
GEO = 'http://www.opengis.net/ont/geosparql#'
OM = 'http://www.ontology-of-units-of-measure.org/resource/om-2/'
OWL = 'http://www.w3.org/2002/07/owl#'
TIME = 'http://www.w3.org/2006/time#'
RT = 'http://environment.data.gov.uk/flood-monitoring/def/core/'
ENVO = 'http://purl.obolibrary.org/obo/'
SOPH = 'http://sweetontology.net/phen/'
SOPHHY = 'http://sweetontology.net/phenHydro/'
# Office for National Statistics (ONS) Linked Data
ONS1 = 'http://statistics.data.gov.uk/def/statistical-geography#'
ONS2 = 'http://statistics.data.gov.uk/def/geography/collection/'
ONS3 = 'http://publishmydata.com/def/ontology/foi/'
# CoMo / CARES ontologies
FLOOD = 'https://www.theworldavatar.com/kg/ontoflood/'
OBE = 'https://www.theworldavatar.com/kg/ontobuiltenv/'
TS = 'https://www.theworldavatar.com/kg/ontotimeseries/'
DERIV = 'https://www.theworldavatar.com/kg/ontoderivation/'
# Knowledge base
KB = 'https://www.theworldavatar.com/kg/ontoflood/'

###--- IRIs for OntoFlood ---###
# TBox
FLOOD_ADMINISTRATIVE_DISTRICT = FLOOD + 'AdministrativeDistrict'        
FLOOD_AFFECTS = FLOOD + 'affects'
FLOOD_AREAL_DISTRIBUTION = FLOOD + 'ArealDistribution'
FLOOD_AREAL_EXTENT_POLYGON = FLOOD + 'ArealExtentPolygon'
FLOOD_AREAL_INFRASTRUCTURE = FLOOD + 'ArealInfrastructure'
FLOOD_ATTACHED_WATERBODY = FLOOD + 'attachedWaterBody'
FLOOD_BUILDING = FLOOD + 'Building'
FLOOD_COASTAL_WATER = FLOOD + 'CoastalWater'
FLOOD_COMMERCIAL_AREA = FLOOD + 'CommercialArea'
FLOOD_ENVIRONMENTAL_COMPONENT = FLOOD + 'EnvironmentalComponent'        
FLOOD_FLOOD_ALERT_OR_WARNING_HISTORY = FLOOD + 'FloodAlertOrWarningHistory'
FLOOD_FLOOD_DEPTH = FLOOD + 'FloodDepth'
FLOOD_FLOOD_FORECAST = FLOOD + 'FloodForecast'
FLOOD_FLOOD_SOURCE = FLOOD + 'FloodSource'
FLOOD_GROUNDWATER = FLOOD + 'GroundWater'
FLOOD_HAS_ADMINISTRATIVE_DISTRICT = FLOOD + 'hasAdministrativeDistrict'  
FLOOD_HAS_ALERT_OR_WARNING_HISTORY = FLOOD + 'hasAlertOrWarningHistory'    
FLOOD_HAS_AREA_IDENTIFIER = FLOOD + 'hasAreaIdentifier'
FLOOD_HAS_AREAL_EXTENT = FLOOD + 'hasArealExtent'
FLOOD_HAS_CLASSIFICATION = FLOOD + 'hasClassification'
FLOOD_HAS_EFFECTIVE_DATE = FLOOD + 'hasEffectiveDate'
FLOOD_HAS_FLOOD_SOURCE = FLOOD + 'hasFloodSource'
FLOOD_HAS_GEOSPATIAL_DISTRIBUTION = FLOOD + 'hasGeospatialDistribution'  
FLOOD_HAS_IMPACT_LEVEL = FLOOD + 'hasImpactLevel'
FLOOD_HAS_INTENSITY = FLOOD + 'hasIntensity'
FLOOD_HAS_LIKELIHOOD = FLOOD + 'hasLikelihood'
FLOOD_HAS_LIKELIHOOD_SCORE = FLOOD + 'hasLikelihoodScore'
FLOOD_HAS_LOCATION = FLOOD + 'hasLocation'
FLOOD_HAS_MONETARY_VALUE = FLOOD + 'hasMonetaryValue'
FLOOD_HAS_POTENTIAL_IMPACT = FLOOD + 'hasPotentialImpact'
FLOOD_HAS_RISK_LEVEL = FLOOD + 'hasRiskLevel'
FLOOD_HAS_SEVERITY = FLOOD + 'hasSeverity'
FLOOD_HAS_SEVERITY_LEVEL = FLOOD + 'hasSeverityLevel'
FLOOD_HAS_TIME_INTERVAL = FLOOD + 'hasTimeInterval'
FLOOD_HAS_TOTAL_AFFECTED_AREA = FLOOD + 'hasTotalAffectedArea'
FLOOD_HAS_TOTAL_COUNT = FLOOD + 'hasTotalCount'
FLOOD_HAS_TOTAL_MONETARY_VALUE = FLOOD + 'hasTotalMonetaryValue'
FLOOD_HAS_WGS84_LATITUDE_LONGITUDE = FLOOD + 'hasWGS84LatitudeLongitude'  
FLOOD_IMPACT = FLOOD + 'Impact'
FLOOD_INDUSTRIAL_AREA = FLOOD + 'IndustrialArea'
FLOOD_INFRASTRUCTURE_COMPONENT = FLOOD + 'InfrastructureComponent'      
FLOOD_LIKELIHOOD = FLOOD + 'Likelihood'
FLOOD_LOCATION = FLOOD + 'Location'
FLOOD_MOBILITY_NETWORK = FLOOD + 'MobilityNetwork'
FLOOD_NETWORK_INFRASTRUCTURE = FLOOD + 'NetworkInfrastructure'
FLOOD_POPULATION = FLOOD + 'Population'
FLOOD_POTENTIAL_IMPACT = FLOOD + 'PotentialImpact'
FLOOD_PREDICTS = FLOOD + 'predicts'
FLOOD_PROVISIONING_NETWORK = FLOOD + 'ProvisioningNetwork'
FLOOD_RESIDENTIAL_AREA = FLOOD + 'ResidentialArea'
FLOOD_RESULTS_IN = FLOOD + 'resultsIn'
FLOOD_RISK_LEVEL = FLOOD + 'RiskLevel'
FLOOD_RIVER_WATER = FLOOD + 'RiverWater'
FLOOD_SEVERITY = FLOOD + 'Severity'
FLOOD_SURFACE_WATER = FLOOD + 'SurfaceWater'
FLOOD_VEHICLE = FLOOD + 'Vehicle'
FLOOD_WARNS_ABOUT = FLOOD + 'warnsAbout'
FLOOD_WATER_VELOCITY = FLOOD + 'WaterVelocity'
# RT (Environment Agency)
RT_CURRENT_WARNING = RT + 'currentWarning'
RT_FLOOD_ALERT_AREA = RT + 'FloodAlertArea'
RT_FLOOD_ALERT_OR_WARNING = RT + 'FloodAlertOrWarning'
RT_FLOOD_AREA = RT + 'FloodArea'
RT_FLOOD_WARNING_AREA = RT + 'FloodWarningArea'
RT_MESSAGE = RT + 'message'
RT_TIME_MESSAGE_CHANGED = RT + 'timeMessageChanged'
RT_TIME_RAISED = RT + 'timeRaised'
RT_TIME_SEVERITY_CHANGED = RT + 'timeSeverityChanged'
# ABox
FLOOD_VERY_LOW_LIKELIHOOD = FLOOD + 'VeryLowLikelihood_ca5e5580-7ab8-4e1c-9087-8cbc893d5c5b'
FLOOD_LOW_LIKELIHOOD = FLOOD + 'LowLikelihood_ca5e5580-7ab8-4e1c-9087-8cbc893d5c5b'
FLOOD_MEDIUM_LIKELIHOOD = FLOOD + 'MediumLikelihood_ca5e5580-7ab8-4e1c-9087-8cbc893d5c5b'
FLOOD_HIGH_LIKELIHOOD = FLOOD + 'HighLikelihood_ca5e5580-7ab8-4e1c-9087-8cbc893d5c5b'
FLOOD_SEVERE_IMPACT = FLOOD + 'SevereImpact_ca5e5580-7ab8-4e1c-9087-8cbc893d5c5b'
FLOOD_SIGNIFICANT_IMPACT = FLOOD + 'SignificantImpact_ca5e5580-7ab8-4e1c-9087-8cbc893d5c5b'
FLOOD_MINOR_IMPACT = FLOOD + 'MinorImpact_ca5e5580-7ab8-4e1c-9087-8cbc893d5c5b'
FLOOD_MINIMAL_IMPACT = FLOOD + 'MinimalImpact_ca5e5580-7ab8-4e1c-9087-8cbc893d5c5b'
FLOOD_SEVERE_FLOOD_WARNING = FLOOD + 'SevereFloodWarning_ca5e5580-7ab8-4e1c-9087-8cbc893d5c5b'
FLOOD_FLOOD_WARNING = FLOOD + 'FloodWarning_ca5e5580-7ab8-4e1c-9087-8cbc893d5c5b'
FLOOD_FLOOD_ALERT = FLOOD + 'FloodAlert_ca5e5580-7ab8-4e1c-9087-8cbc893d5c5b'
FLOOD_WARNING_NO_LONGER_IN_FORCE = FLOOD + 'InactiveFloodWarning_ca5e5580-7ab8-4e1c-9087-8cbc893d5c5b'

# OntoBuiltEnv
OBE_HASMARKETVALUE = OBE + 'hasMarketValue'

# OntoTimeSeries
TS_HAS_RDB = TS + 'hasRDB'
TS_HAS_TIME_SERIES = TS + 'hasTimeSeries'
TS_HAS_TIME_UNIT = TS + 'hasTimeUnit'
TS_TIMESERIES = TS + 'TimeSeries'

# Ontoderivation
DERIV_BELONGS_TO = DERIV + 'belongsTo'
DERIV_DERIVED_FROM = DERIV + 'isDerivedFrom'
DERIV_DERIVED_USING = DERIV + 'isDerivedUsing' 
DERIV_HAS_STATUS = DERIV + 'hasStatus' 
DERIV_RETRIEVED_INPUTS_AT = DERIV + 'retrievedInputsAt'

# Time ontology
TIME_HAS_BEGINNING = TIME + 'hasBeginning'
TIME_HAS_END = TIME + 'hasEnd'
TIME_HAS_XSDDURATION = TIME + 'hasXSDDuration'
TIME_INSTANT = TIME + 'Instant'
TIME_INTERVAL = TIME + 'Interval'
TIME_IN_XSDDATETIMESTAMP = TIME + 'inXSDDateTimeStamp'

# Ontology of units of measure
OM_AMOUNTOFMONEY = OM + 'AmountOfMoney'
OM_AREA = OM + 'Area'
OM_DEPTH = OM + 'Depth'
OM_HAS_NUMERICALVALUE = OM + 'hasNumericalValue'
OM_HAS_UNIT = OM + 'hasUnit'
OM_HAS_VALUE = OM + 'hasValue'
OM_MEASURE = OM + 'Measure'
OM_QUANTITY = OM + 'Quantity'
OM_SYMBOL = OM + 'symbol'
OM_UNIT = OM + 'Unit'
OM_VELOCITY = OM + 'Velocity'

# ENVO
ENVO_WATER_BODY = ENVO + 'ENVO_00000063'
ENVO_CANAL = ENVO + 'ENVO_00000014'
ENVO_SEA = ENVO + 'ENVO_00000016'
ENVO_LAKE = ENVO + 'ENVO_00000020'
ENVO_RIVER = ENVO + 'ENVO_00000022'
ENVO_FLOOD = ENVO + 'ENVO_01000710'
ENVO_COASTAL_FLOOD = ENVO + 'ENVO_01000711'
ENVO_RIVERINE_FLOOD = ENVO + 'ENVO_01000712'
ENVO_FLASH_FLOOD = ENVO + 'ENVO_01000713'
ENVO_FORMED_AS_RESULT_OF = ENVO + 'RO_0002354'
ENVO_COASTAL_FLOODING = ENVO + 'ENVO_01000707'
ENVO_RIVERINE_FLOODING = ENVO + 'ENVO_01000708'
ENVO_FLASH_FLOODING = ENVO + 'ENVO_01000709'
ENVO_AREAL_FLOODING = ENVO + 'ENVO_01000717'
ENVO_URBAN_FLOODING = ENVO + 'ENVO_01000718'
ENVO_FLOODING = ENVO + 'ENVO_02500002'

# SOPH, SOPHHY
SOPH_EVENT = SOPH + 'Event'
SOPHHY_FLOOD = SOPHHY + 'Flood'

# Data types
RDF_TYPE = RDF + 'type'
RDFS_COMMENT = RDFS + 'comment'
RDFS_LABEL = RDFS + 'label'
XSD_STRING = XSD + 'string'
XSD_FLOAT = XSD + 'float'
XSD_DATETIME = XSD + 'dateTime'

# GeoSPARQL
GEO_FEATURE = GEO + 'Feature'
GEO_HAS_GEOMETRY = GEO + 'hasGeometry'
GEO_ASWKT = GEO + 'asWKT'

# OWL
OWL_VERSION = OWL + 'versionInfo'
OWL_SAMEAS = OWL + 'sameAs'

# Office for National Statistics (ONS)
ONS_GEOGRAPHY = ONS1 + 'Statistical-Geography'
ONS_NAME = ONS1 + 'officialname'
ONS_COUNTIES = ONS2 + 'E10'
ONS_MEMBER_OF = ONS3 + 'memberOf'


# OM / UOM unit symbols
OM_GBP = OM + 'poundSterling'
# NOTE: There are issues with encoding of special characters; hence, all units are
#       "properly" uploaded to KG at agent startup and decoded when retrieved
#       For details see: kgutils.initialise_kg.instantiate_all_units()
GBP_SYMBOL = '£'