################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk)        #    
# Date: 17 May 2023                            #
################################################

XSD = 'http://www.w3.org/2001/XMLSchema#'
CLIMA =  "http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/"
CLIMB = "http://www.theworldavatar.com/ontology/ontogasgrid/ontoclimate.owl#"
RDF = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
OM = "http://www.ontology-of-units-of-measure.org/resource/om-2/"
COMP = "http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#"
REGION = "http://www.theworldavatar.com/ontology/ontoregionalanalysis/"
ONTOCAPE = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#"
RDFS = "http://www.w3.org/2000/01/rdf-schema#"
OFP = "http://www.theworldavatar.com/ontology/ontofuelpoverty/ontofuelpoverty.owl#"

RDF_TYPE = RDF + "type"
IS_A = RDFS + "subClassOf"
ONTOCAPE_COUNTRY = ONTOCAPE + "Country"

ONS_ID = "http://statistics.data.gov.uk/id/statistical-geography/"
ONS_DEF = "http://statistics.data.gov.uk/def/statistical-geography#"
ONS_DEF_STAT= ONS_DEF + "Statistical-Geography"

CLIMA_TAS = CLIMA + "tas"
CLIMA_TASMAX = CLIMA + "tasmax"
CLIMA_TASMIN = CLIMA + "tasmin"

CLIMB_CLIMATEMEASUREMENT = CLIMB + "ClimateMeasurement"
CLIMB_HASVAR = CLIMB + "hasClimateVariable"
CLIMB_HASMEASURE = CLIMB + "hasClimateMeasurement"

COMP_HAS_STARTUTC = COMP + "hasStartUTC"
COMP_HAS_ENDUTC = COMP + "hasEndUTC"


OM_MEASURE = OM + "Measure"
OM_HAS_VALUE = OM + "hasValue"
OM_HAS_NUMERICALVALUE = OM +"hasNumericalValue"
OM_HAS_PHENO = OM + "hasPhenomenon"

XSD_STRING = XSD + "string"
XSD_FLOAT = XSD + "float"
XSD_DATETIME = XSD + "dateTime"

OFP_VALIDFROM = OFP + "validFrom"
OFP_VALIDTO = OFP + "validTo"

REGION_ISWITHIN = REGION + "iswithin"
REGION_APPLICABLETO = REGION + "applicableto"
REGION_HEATPUMP_EFFICIENCY = REGION + "HeatPumpEfficiency"
REGION_HOTSIDE_TEMPERATURE = REGION + "HotSideTemperature"
REGION_COP = REGION + "COP"
REGION_HASCOP = REGION + "hasCOP"
REGION_HASVAR = REGION + "hasVar"
REGION_MAX_VAL = REGION + "hasMaxValue"
REGION_MEAN_VAL = REGION + "hasMeanValue"
REGION_MIN_VAL = REGION + "hasMinValue"
