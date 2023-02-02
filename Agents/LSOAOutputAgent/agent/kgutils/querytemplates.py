################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk) #
# Date: 30/11 2022                            #
################################################

# The purpose of this module is to provide templates for
# required SPARQL queries/updates

from agent.datamodel.iris import *
from agent.utils.env_configs import YEAR

def output_query_template(keyword: str, year: str = YEAR):
    '''
        Return the SPARQL query string for Electricity, Gas, Fuel poverty, Temperature, ONS output area.

        Arguments:
         Keyword: 'Electricity'/'Gas'/'Fuel poverty'/'Temperature'/'ONS output area'
                   Each keyword will generate a specific query string to obtain the data from the KG


    '''
    if keyword == 'Temperature':
        query = 'PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> '
    else:
        query = ''
        
    query += f'''SELECT DISTINCT ?s'''

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
        query+= ' (xsd:float(?a)/xsd:float(?num) AS ?result) ?num'

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
              <{COMP_HAS_STARTUTC}>  "{year + "-01-01 12:00:00"}"^^<{XSD_DATETIME}>.
    ?energy <{OM_HAS_PHENO}> ?elec;
            <{OM_HAS_VALUE}> ?usageiri.
    ?usageiri <{OM_HAS_NUMERICALVALUE}> ?usage.
    ?elec <{COMP_HAS_STARTUTC}>  "{year + "-01-01 12:00:00"}"^^<{XSD_DATETIME}>.
    """

    if keyword == 'Gas':
        query+= f"""<{COMP_HASUSED}> ?gas;
                    <{GAS_HAVE_GASMETERS}> ?metiri.
        ?metiri <{GAS_HAVE_CONSUM_GASMETERS}> ?meter;
                <{COMP_HAS_STARTUTC}>  "{year + "-01-01 12:00:00"}"^^<{XSD_DATETIME}>; 
                <{GAS_HAVE_NONCONSUM_GASMETERS}> ?nonmeter.
        ?energy <{OM_HAS_PHENO}> ?gas;
                 <{OM_HAS_VALUE}> ?usageiri.
        ?usageiri <{OM_HAS_NUMERICALVALUE}>  ?usage.
        ?gas      <{COMP_HAS_STARTUTC}>  "{year + "-01-01 12:00:00"}"^^<{XSD_DATETIME}>.

        """

    if keyword == 'Temperature':
        query+= f"""    <{CLIMB_HASMEASURE}>  ?m.
    ?m <{COMP_HAS_STARTUTC}> ?start;
        <{COMP_HAS_ENDUTC}> ?end.
    ?m  <{CLIMB_HASVAR}> ?var.
    ?p <{OM_HAS_PHENO}> ?m.
    ?p <{OM_HAS_VALUE}> ?t_iri.
    ?t_iri <{OM_HAS_NUMERICALVALUE}> ?t.

    FILTER (regex(str(?start), "{year}-\\\d\\\d-01 12:00:00") && datatype(?start) = xsd:dateTime)
        """

    if keyword == 'Fuel poverty':
        query+= f"""  <{OFP_HASHOUSEHOLD}> ?housesiri.
     ?housesiri <{OFP_FUELPOOR}> ?a;
             <{OFP_VALIDFROM}> "{year + "-01-01 12:00:00"}"^^<{XSD_DATETIME}>;
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

