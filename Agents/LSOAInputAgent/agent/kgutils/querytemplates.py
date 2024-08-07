################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk) #
# Date: 30/11 2022                            #
################################################

# The purpose of this module is to provide templates for
# required SPARQL queries/updates

from agent.datamodel.iris import *

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
def month_num(month_str):
      '''
      Converts a string of month name to that month's number 
      Starting at 0
      '''
      months = {'January':0,'February':1,'March':2,'April':3,'May':4,'June':5,'July':6,'August':7,'September':8,'October':9,'November':10,'December':11}
      month_ends = {'January':31,'February':28,'March':31,'April':30,'May':31,'June':30,'July':30,'August':31,'September':30,'October':29,'November':30,'December':31}
      month = months[month_str]
      month_end = month_ends[month_str]
      return month, month_end

# clim_vars = ['tasmin','tas','tasmax']
# months = ['January','February','March','April','May','June','July','August','September','October','November','December']

# from datetime import datetime
# import uuid
# for month_it in range(len(months)):
#     for var in range(3):
#         month_str = str(month_it+1)
#         _, month_end = month_num(months[month_it])
#         startUTC = datetime.strptime('2020-'+month_str+'-01T12:00:00.000Z', '%Y-%m-%dT%H:%M:%S.000Z')
#         endUTC = datetime.strptime('2020-'+month_str+'-'+str(month_end)+'T12:00:00.000Z', '%Y-%m-%dT%H:%M:%S.000Z')
#         LSOA_code = 'E01000001'
#         meas_uuid = CLIMA + 'ClimateMeasurement_' + str(uuid.uuid4())
#         clim_var = CLIMA + str(clim_vars[var])
#         temp_uuid = CLIMA + 'Temperature_' + str(uuid.uuid4())
#         val_uuid = CLIMA + 'Value_' + str(uuid.uuid4())
#         triple = climate_temperature_update_template(LSOA_code,meas_uuid,clim_var,startUTC,endUTC,temp_uuid,val_uuid,10)
#         print(triple)
