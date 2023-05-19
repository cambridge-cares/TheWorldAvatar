################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk) #
# Date: 30/11 2022                            #
################################################

# The purpose of this module is to provide templates for
# required SPARQL queries/updates

from agent.datamodel.iris import *
'''
from agent.kgutils.stackclients import PostGISClient
###--- --- --- --- --- URLs to ontology .owl files --- --- --- --- ---###
TBOX_URL_FUELPOVERTY = 'http://www.theworldavatar.com/ontology/ontofuelpoverty/ontofuelpoverty.owl'
TBOX_URL_GASGRID = 'http://www.theworldavatar.com/ontology/ontogasgrid/ontogasgrid.owl'
TBOX_URL_CLIMATE = 'http://www.theworldavatar.com/ontology/ontogasgrid/ontoclimate.owl'
TBOX_URL_GASCOMPONENT = 'http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl'
TBOX_URL_GASSYSTEM = 'http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl'

ABOX_URL = 'not found???'

GAS = "http://www.theworldavatar.com/ontology/ontogasgrid/ontogasgrid.owl#"
COMP = "http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#"
COMPA = "http://www.theworldavatar.com/kb/ontogasgrid/offtakes_abox/"
CLIMB =  "http://www.theworldavatar.com/ontology/ontogasgrid/ontoclimate.owl#"
OFP = "http://www.theworldavatar.com/ontology/ontofuelpoverty/ontofuelpoverty.owl#"
OFPT = "http://www.theworldavatar.com/kb/ontofuelpoverty/abox/"
CLIMA =  "http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/"

###--- --- --- --- --- Common Base URLs --- --- --- --- ---###

# External ontologies
RDF = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
RDFS = "http://www.w3.org/2000/01/rdf-schema#"
XSD = "http://www.w3.org/2001/XMLSchema#"
GEO = "http://www.opengis.net/ont/geosparql#"
OM = "http://www.ontology-of-units-of-measure.org/resource/om-2/"
OWL = "http://www.w3.org/2002/07/owl#"

# ONS related
ONS_ID = "http://statistics.data.gov.uk/id/statistical-geography/"
ONS_DEF = "http://statistics.data.gov.uk/def/statistical-geography#"
ONS_DEF_STAT= ONS_DEF + "Statistical-Geography"
# CoMo / CARES ontologies
TS = "https://www.theworldavatar.com/kg/ontotimeseries/"

###--- --- --- --- --- URLs to ontologies --- --- --- --- ---###

# OntoGasgrid
COMP_HASCONSUMED = COMP + 'hasConsumed'
COMP_HASUSED = COMP + 'hasUsed'
COMP_ELEC = COMP + "Electricity"
COMP_HAS_STARTUTC = COMP + "hasStartUTC"
COMP_HAS_ENDUTC = COMP + "hasEndUTC"
COMP_OFFTAKENGAS = COMP + "OfftakenGas"

GAS_ELEC = GAS + "ElecMeters"
GAS_GASMETER = GAS + "GasMeters"
GAS_HAS_ELECMETERS = GAS + "hasElecMeters"
GAS_HAS_CONSUM_ELECMETERS = GAS + "hasConsumingElecMeters"
GAS_HAVE_GASMETERS = GAS + "hasGasMeters"
GAS_HAVE_CONSUM_GASMETERS = GAS +"hasConsumingGasMeters"
GAS_HAVE_NONCONSUM_GASMETERS = GAS +"hasNonConsumingGasMeters"

# Ontofuelpoverty
OFP_HASHOUSEHOLD = OFP + 'hasHouseholds'
OFP_FUELPOOR = OFP + 'fuelpoorhouseholds'
OFP_NUMBEROFHOUSEHOLD = OFP + 'numberofhouseholds'
OFP_VALIDFROM = OFP + "validFrom"
OFP_VALIDTO = OFP + "validTo"

# OntoClimate
CLIMB_HASMEASURE = CLIMB + "hasClimateMeasurement"
CLIMB_HASVAR = CLIMB + "hasClimateVariable"
CLIMB_CLIMATEMEASUREMENT = CLIMB + "ClimateMeasurement"
CLIMB_CLIMBVARIABLE = CLIMB + "ClimateVariable"

# OntoTimeSeries
TS_TIMESERIES = TS + "TimeSeries"
TS_HAS_TIMESERIES = TS + "hasTimeSeries"
TS_HAS_TIME_UNIT = TS + "hasTimeUnit"
TS_HAS_RDB = TS + "hasRDB"

# Ontology of units of measure
OM_QUANTITY = OM + "Quantity"
OM_ENERGY = OM + "Energy"
OM_MEASURE = OM + "Measure"
OM_UNIT = OM + "Unit"
OM_TEMPERATURE = OM + "Temperature"

OM_HAS_VALUE = OM + "hasValue"
OM_HAS_NUMERICALVALUE = OM +"hasNumericalValue"
OM_HAS_UNIT = OM + "hasUnit"
OM_HAS_PHENO = OM + "hasPhenomenon"

OM_SYMBOL = OM + "symbol"
OM_DEGREE_C = OM + "degreeCelsius"
OM_HECTO_PASCAL = OM + "hectopascal"
OM_PERCENT = OM + "percent"
OM_METRE = OM + "metre"
OM_MPH = OM + "mile-StatutePerHour"
OM_DEGREE = OM + "degree"
OM_UNITLESS = OM + "one"
OM_KW = OM + "kilowattHour"

# Data types
RDF_TYPE = RDF + "type"
RDFS_COMMENT = RDFS + "comment"
RDFS_LABEL = RDFS + "label"
XSD_STRING = XSD + "string"
XSD_FLOAT = XSD + "float"
XSD_DATETIME = XSD + "dateTime"

# GeoSPARQL
GEO_FEATURE = GEO + "Feature"
GEO_GEOMETRY = GEO + 'Geometry'
GEO_HAS_GEOMETRY = GEO + "hasGeometry"
GEO_ASWKT = GEO + "asWKT"

# OWL
OWL_VERSION = OWL + "versionInfo"
OWL_SAMEAS = OWL + "sameAs"
'''

def electricity_update_template(mes_uuid,used_uuid,start_time,end_time,region,kw_uuid,cons,met_uuid,meters) -> str:
    '''
        Return strings of SPARQL Update query regarding to each triple
        
        Arguments:
         variables to be imported as data to the knowledge graph.
         Specific definitions can be refered to the reference
    '''
    region = ONS_ID + region

    triples = f""" <{mes_uuid}> <{RDF_TYPE}> <{OM_MEASURE}>.
                <{mes_uuid}> <{OM_HAS_UNIT}> <{OM_KW}>.
                <{used_uuid}> <{RDF_TYPE}> <{COMP_ELEC}>;
                        <{COMP_HAS_STARTUTC}>  "{start_time}"^^<{XSD_DATETIME}> ;
                        <{COMP_HAS_ENDUTC}>  "{end_time}"^^<{XSD_DATETIME}> .
                
                <{region}> <{RDF_TYPE}> <{ONS_DEF_STAT}>;
                           <{COMP_HASCONSUMED}> <{used_uuid}> .

                <{kw_uuid}> <{RDF_TYPE}> <{OM_ENERGY}>;
                        <{OM_HAS_PHENO}> <{used_uuid}> ;
                        <{OM_HAS_VALUE}> <{mes_uuid}>.

                <{mes_uuid}> <{OM_HAS_NUMERICALVALUE}>  {cons}.
                <{met_uuid}> <{RDF_TYPE}> <{GAS_ELEC}>.
                <{region}> <{GAS_HAS_ELECMETERS}> <{met_uuid}>.
                <{met_uuid}> <{GAS_HAS_CONSUM_ELECMETERS}>  {meters};
                        <{COMP_HAS_STARTUTC}> "{start_time}"^^<{XSD_DATETIME}> ;
                        <{COMP_HAS_ENDUTC}>  "{end_time}"^^<{XSD_DATETIME}> . 
                """

    return triples

def region_update_template(region,wkt):
    '''
        Return strings of SPARQL Update query regarding to each triple
        
        Arguments:
         variables to be imported as data to the knowledge graph.
         Specific definitions can be refered to the reference
    ''' 

    geo = COMPA + region + '_geometry'
    region = ONS_ID + region
    # geometry instance (regional code used for URI)
    # associating region with geometry
    # NOTE: the region iteself is not defined here as it's class (statistical geography) because it was already defined 
    # adding WKT string property to geometry instance
    triples = f"""
        <{region}> <{RDF_TYPE}> <{ONS_DEF_STAT}>; 
                   <{GEO_HAS_GEOMETRY}> <{geo}>.
        <{geo}> <{RDF_TYPE}> <{GEO_GEOMETRY}>;
                 <{GEO_ASWKT}> "{wkt}".
                """

    return triples

def gas_update_template(mes_uuid,used_uuid,start_time,end_time,region,kw_uuid,cons,met_uuid,meters,non_meters):
    '''
        Return strings of SPARQL Update query regarding to each triple
        
        Arguments:
         variables to be imported as data to the knowledge graph.
         Specific definitions can be refered to the reference
    ''' 
    region = ONS_ID + region
    triples = f""" 
            <{mes_uuid}> <{RDF_TYPE}> <{OM_MEASURE}>.
            <{mes_uuid}> <{OM_HAS_UNIT}> <{OM_KW}>.
            <{used_uuid}> <{RDF_TYPE}> <{COMP_OFFTAKENGAS}>;
                    <{COMP_HAS_STARTUTC}>  "{start_time}"^^<{XSD_DATETIME}> ;
                    <{COMP_HAS_ENDUTC}>  "{end_time}"^^<{XSD_DATETIME}> .
        
            <{region}> <{RDF_TYPE}> <{ONS_DEF_STAT}>;
                        <{COMP_HASUSED}> <{used_uuid}> .
        
            <{kw_uuid}> <{RDF_TYPE}> <{OM_ENERGY}>;
                            <{OM_HAS_PHENO}> <{used_uuid}> ;
                            <{OM_HAS_VALUE}> <{mes_uuid}>.
            
            <{mes_uuid}> <{OM_HAS_NUMERICALVALUE}>  {cons}.
            <{met_uuid}> <{RDF_TYPE}> <{GAS_GASMETER}>.

            <{region}> <{GAS_HAVE_GASMETERS}> <{met_uuid}>.
            <{met_uuid}> <{GAS_HAVE_CONSUM_GASMETERS}>  {meters};
                         <{GAS_HAVE_NONCONSUM_GASMETERS}>  {non_meters};
                    <{COMP_HAS_STARTUTC}> "{start_time}"^^<{XSD_DATETIME}> ;
                    <{COMP_HAS_ENDUTC}>  "{end_time}"^^<{XSD_DATETIME}>. 
        """

    return triples

def fuel_poor_update_template(region,house_uuid,start_time,end_time,houses,poor):
    '''
        Return strings of SPARQL Update query regarding to each triple
        
        Arguments:
         variables to be imported as data to the knowledge graph.
         Specific definitions can be refered to the reference
    ''' 
    region = ONS_ID + region
    triples = f""" 
    <{region}> <{RDF_TYPE}> <{ONS_DEF_STAT}>;
               <{OFP_HASHOUSEHOLD}> <{house_uuid}>.

    <{house_uuid}> <{OFP_VALIDFROM}> "{start_time}"^^<{XSD_DATETIME}> ;
                   <{OFP_VALIDTO}>   "{end_time}"^^<{XSD_DATETIME}> ;
                   <{OFP_NUMBEROFHOUSEHOLD}>  {houses} ;
                   <{OFP_FUELPOOR}>  {poor} .
    """

    return triples

def climate_temperature_update_template(region,meas_uuid,clim_var,start_time,end_time,temp_uuid,val_uuid,value):
    '''
        Return strings of SPARQL Update query regarding to each triple
        
        Arguments:
         variables to be imported as data to the knowledge graph.
         Specific definitions can be refered to the reference
    ''' 
    region = ONS_ID + region
    triples = f""" 
    <{meas_uuid}> <{RDF_TYPE}>  <{CLIMB_CLIMATEMEASUREMENT}> .
            <{region}> <{CLIMB_HASMEASURE}>  <{meas_uuid}> .
            <{clim_var}> <{RDF_TYPE}>  <{CLIMB_CLIMBVARIABLE}> .
            <{meas_uuid}>  <{COMP_HAS_STARTUTC}> "{start_time}"^^<{XSD_DATETIME}> ;
                        <{COMP_HAS_ENDUTC}>  "{end_time}"^^<{XSD_DATETIME}>;
                        <{CLIMB_HASVAR}>   "{clim_var}"^^<{XSD_STRING}> .
            <{temp_uuid}>  <{RDF_TYPE}>  <{OM_TEMPERATURE}> ;
                           <{OM_HAS_PHENO}>  <{meas_uuid}> ;
                           <{OM_HAS_VALUE}>  <{val_uuid}> .
            <{val_uuid}> <{RDF_TYPE}>  <{OM_MEASURE}> ;
                         <{OM_HAS_UNIT}>  <{OM_DEGREE_C}> ;
                         <{OM_HAS_NUMERICALVALUE}> {value} .
        """

    return triples

def climate_temperature_query_template(region):
    '''
        Return strings of SPARQL Update query regarding to each triple
        
        Arguments:
         variables to be imported as data to the knowledge graph.
         Specific definitions can be refered to the reference
    ''' 
    region = ONS_ID + region
    triples = f""" 
    SELECT DISTINCT ?meas_uuid ?start_time
    WHERE {{
    ?meas_uuid <{RDF_TYPE}>  <{CLIMB_CLIMATEMEASUREMENT}> .
            <{region}> <{CLIMB_HASMEASURE}>  ?meas_uuid .
            ?clim_var <{RDF_TYPE}>  <{CLIMB_CLIMBVARIABLE}> .
            ?meas_uuid  <{COMP_HAS_STARTUTC}> ?start_time ;
                        <{COMP_HAS_ENDUTC}>  ?end_time;
                        <{CLIMB_HASVAR}>   ?var .
            ?temp_uuid  <{RDF_TYPE}>  <{OM_TEMPERATURE}> ;
                           <{OM_HAS_PHENO}>  ?meas_uuid;
                           <{OM_HAS_VALUE}>  ?val_uuid .
            ?val_uuid <{RDF_TYPE}>  <{OM_MEASURE}> ;
                         <{OM_HAS_UNIT}>  <{OM_DEGREE_C}> ;
                         <{OM_HAS_NUMERICALVALUE}> ?value .
                         }}
        """

    return triples

# triple = climate_temperature_query_template('E01000001')
# print(triple)
