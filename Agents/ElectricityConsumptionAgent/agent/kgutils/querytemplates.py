################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk) #
# Date: 30/11 2022                            #
################################################

# The purpose of this module is to provide templates for
# required SPARQL queries/updates

from agent.datamodel.iris import *
'''
from agent.kgutils.stackclients import PostGISClient
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
                        <{COMP_HASCONSUMED}> <{used_uuid}> .
        
            <{kw_uuid}> <{RDF_TYPE}> <{OM_ENERGY}>;
                            <{OM_HAS_PHENO}> <{used_uuid}> ;
                            <{OM_HAS_VALUE}> <{mes_uuid}>.
            
            <{mes_uuid}> <{OM_HAS_NUMERICALVALUE}>  {cons}.
            <{met_uuid}> <{RDF_TYPE}> <{GAS_GASMETER}>.

            <{region}> <{GAS_HAVE_GASMETERS}> <{met_uuid}>.
            <{met_uuid}> <{GAS_HAVE_CONSUM_GASMETERS}>  {meters};
                         <{GAS_HAVE_NONCONSUM_GASMETERS}>  {non_meters};
                    <{COMP_HAS_STARTUTC}> "{start_time}"^^<{XSD_DATETIME}> ;
                    <{COMP_HAS_ENDUTC}>  "{end_time}"^^<{XSD_DATETIME}> 
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
                   <{OFP_NUMBEROFHOUSEHOLD} <{houses}> ;
                   <{OFP_FUELPOOR}> <{poor}> .
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
                        <{COMP_HAS_ENDUTC}>  "{end_time}"^^<{XSD_DATETIME}>
                        <{CLIMB_HASVAR}>   <{clim_var}>  .
            <{temp_uuid}>  <{RDF_TYPE}>  <{OM_TEMPERATURE}> ;
                           <{OM_HAS_PHENO}>  <{meas_uuid}> ;
                           <{OM_HAS_VALUE}>  <{val_uuid}> .
            <{val_uuid}> <{RDF_TYPE}>  <{OM_MEASURE}> ;
                         <{OM_HAS_UNIT}>  <{OM_DEGREE_C}> ;
                         <{OM_HAS_NUMERICALVALUE}> <{value}> .
        """

    return triples

def output_query_template(keyword: str, year: str = '2020'):
    '''
        Return the SPARQL query string for Electricity, Gas, Fuel poverty, Temperature, ONS output area.

        Arguments:
         Keyword: 'Electricity'/'Gas'/'Fuel poverty'/'Temperature'/'ONS output area'
                   Each keyword will generate a specific query string to obtain the data from the KG


    '''
    query = f'''SELECT DISTINCT ?s'''

    #--------------------------Query heading------------------------------------------
    # 'Electricity' - return LSOA code, electricity usage, number of electricity meters
    if keyword == 'Electricity':
        query+= ' ?usage ?meter'

    # 'Gas' - return LSOA code, gas usage, number of gas consuming meters, number of gas non consuming meters
    if keyword == 'Gas':
        query+= ' ?usage ?meter ?nonmeter'

    # 'Temperature' - return LSOA code, start time, end time, variable (min/mean/max), temperature value
    if keyword == 'Temperature':
        query+=' ?start ?var ?t'

    # 'Fuel poverty' - return LSOA code, propotion of fuel poor, number of household
    if keyword == 'Fuel poverty':
        query+= ' (xsd:float(?a)/xsd:float(?b) AS ?result) ?num'

    # 'ONS output area' - return LSOA code, WKT form geometry data
    if keyword == 'ONS output area':
        query+=' ?geom'

    #--------------------------Main Query body----------------------------------------------------#
    query+=   ' WHERE {'
    query+=   f"""?s <{RDF_TYPE}> <{ONS_DEF_STAT}>;"""

    if keyword == 'Electricity':
        query+= f"""<{COMP_HASCONSUMED}> ?elec;
                    <{GAS_HAS_ELECMETERS}> ?meteriri.
    ?meteriri <{GAS_HAS_CONSUM_ELECMETERS}> ?meter;
              <{COMP_HAS_STARTUTC}>  "{year + "-01-01T12:00:00"}"^^<{XSD_DATETIME}>.
    ?energy <{OM_HAS_PHENO}> ?elec;
            <{OM_HAS_VALUE}> ?usageiri.
    ?usageiri <{OM_HAS_NUMERICALVALUE}> ?usage.
    ?elec <{COMP_HAS_STARTUTC}>  "{year + "-01-01T12:00:00"}"^^<{XSD_DATETIME}>.
    """

    if keyword == 'Gas':
        query+= f"""<{COMP_HASUSED}> ?gas;
                    <{GAS_HAVE_GASMETERS}> ?metiri.
        ?metiri <{GAS_HAVE_CONSUM_GASMETERS}> ?meter;
                <{COMP_HAS_STARTUTC}>  "{year + "-01-01T12:00:00"}"^^<{XSD_DATETIME}>; 
                <{GAS_HAVE_NONCONSUM_GASMETERS}> ?nonmeter.
        ?energy <{OM_HAS_PHENO}> ?gas;
                 <{OM_HAS_VALUE}> ?usageiri.
        ?usageiri <{OM_HAS_NUMERICALVALUE}>  ?usage;
                  <{COMP_HAS_STARTUTC}>  "{year + "-01-01T12:00:00"}"^^<{XSD_DATETIME}>.

        """

    if keyword == 'Temperature':
        query+= f"""    <{CLIMB_HASMEASURE}  ?m.
    ?m <{COMP_HAS_STARTUTC}> ?start;
        <{COMP_HAS_ENDUTC}> ?end.
    ?m  <{CLIMB_HASVAR}> ?var.
    ?p <{OM_HAS_PHENO}> ?m.
    ?p <{OM_HAS_VALUE}> ?t_iri.
    ?t_iri <{OM_HAS_NUMERICALVALUE}> ?t.

    FILTER (regex(str(?start), "{year}-\\d\\d-01T12:00:00") && datatype(?start) = xsd:dateTime)
        """

    if keyword == 'Fuel poverty':
        query+= f"""  <{OFP_HASHOUSEHOLD}> ?housesiri.
     ?housesiri <{OFP_FUELPOOR}> ?a;
             <{OFP_VALIDFROM}> "{year + "-01-01T12:00:00"}"^^<{XSD_DATETIME}>;
             <{OFP_NUMBEROFHOUSEHOLD}> ?num.
        """

    if keyword == 'ONS output area':
        query+= f"""   <{GEO_HAS_GEOMETRY}> ?o.
    OPTIONAL{{
            ?o <{GEO_ASWKT}> ?geom}}
    """
    #--------------------------Query end here----------------------------------
    query+= "}"
    return query
