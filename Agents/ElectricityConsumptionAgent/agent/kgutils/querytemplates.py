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

def electricity_update_template(mes_uuid,used_uuid,start_time,end_time,region,kw_uuid,cons,met_uuid,meters,non_meters: int = None) -> str:
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
                        <{COMP_HAS_ENDUTC}>  "{end_time}"^^<{XSD_DATETIME}>  .
                
                <{region}> <{RDF_TYPE}> <{ONS_DEF_STAT}>.
                <{region}> <{COMP_HASCONSUMED}> <{used_uuid}> .

                <{kw_uuid}> <{RDF_TYPE}> <{OM}>;
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

def output_query_template(keyword, iris = False):
    '''
        Return the SPARQL query string for Electricity, Gas, Fuel poverty, Temperature, ONS output area.

        Arguments:
         Keyword: 'Electricity'/'Gas'/'Fuel poverty'/'Temperature'/'ONS output area'
                   Each keyword will generate a specific query string to obtain the data from the KG

                   iris: 'True'/'False'
                   If 'True' then return the data iri along with the query result

    '''
    query = f'''SELECT DISTINCT ?s'''

    #--------------------------Query heading------------------------------------------
    # 'Electricity' - return LSOA code, electricity usage, number of electricity meters
    if keyword == 'Electricity':
        query+= ' ?usage ?meter'
        if iris == True:
            query+= ' ?usageiri ?meteriri'

    # 'Gas' - return LSOA code, gas usage, number of gas consuming meters, number of gas non consuming meters
    if keyword == 'Gas':
        query+= ' ?usage ?con ?non'

    # 'Temperature' - return LSOA code, start time, end time, variable (min/mean/max), temperature value
    if keyword == 'Temperature':
        query+=' ?start ?end ?var ?t'

    # 'Fuel poverty' - return LSOA code, propotion of fuel poor, number of household
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
    ?meteriri <{GAS_HAS_CONSUM_ELECMETERS}> ?meter.
    ?energy <{OM_HAS_PHENO}> ?elec;
            <{OM_HAS_VALUE}> ?usageiri.
    ?usageiri <{OM_HAS_NUMERICALVALUE}> ?usage. 
    """


    if keyword == 'Gas':
        query+= f"""<{COMP_HASUSED}> ?gas;
                    <{GAS_HAVE_GASMETERS}> ?met.
        ?met <{GAS_HAVE_CONSUM_GASMETERS}> ?con;
             <{GAS_HAVE_NONCONSUM_GASMETERS}> ?non.
        ?energy <{OM_HAS_PHENO}> ?gas;
                 <{OM_HAS_VALUE}> ?enval.
        ?enval <{OM_HAS_NUMERICALVALUE}>  ?usage.
        """


    if keyword == 'Temperature':
        query+= f"""    <{CLIMB_HASMEASURE}  ?m.
    ?m <{COMP_HAS_STARTUTC}> ?start;
        <{COMP_HAS_ENDUTC}> ?end.
    ?m  <{CLIMB_HASVAR}> ?var.
    ?p <{OM_HAS_PHENO}> ?m.
    ?p <{OM_HAS_VALUE}> ?oval.
    ?oval <{OM_HAS_NUMERICALVALUE}> ?t.
        """


    if keyword == 'Fuel poverty':
        query+= f"""  <{OFP_HASHOUSEHOLD}> ?houses.
     ?houses <{OFP_FUELPOOR}> ?a.
     ?houses <{OFP_NUMBEROFHOUSEHOLD}> ?num.
        """


    if keyword == 'ONS output area':
        query+= f"""   <{GEO_HAS_GEOMETRY}> ?o.
    OPTIONAL{{
            ?o <{GEO_ASWKT}> ?geom}}
    """
    #--------------------------Query end here----------------------------------
    query+= "}"
    return query

'''
def add_om_quantity(
    station_iri,
    quantity_iri,
    quantity_type,
    data_iri,
    data_iri_type,
    unit,
    symbol,
    is_observation: bool,
    creation_time=None,
    comment=None,
):
    """
    Create triples to instantiate station measurements
    """
    # Create triple for measure vs. forecast
    if is_observation:
        triple = f"""<{quantity_iri}> <{OM_HAS_VALUE}> <{data_iri}> . """

    else:
        triple = f"<{quantity_iri}> <{EMS_HAS_FORECASTED_VALUE}> <{data_iri}> . "
        if creation_time:
            triple += f'<{data_iri}> <{EMS_CREATED_ON}> "{creation_time}"^^<{XSD_DATETIME}> . '

    # Create triples to instantiate station measurement according to OntoEMS
    triples = f"""
        <{station_iri}> <{EMS_REPORTS}> <{quantity_iri}> . 
        <{quantity_iri}> <{RDF_TYPE}> <{quantity_type}> . 
        <{data_iri}> <{RDF_TYPE}> <{data_iri_type}> . 
        <{data_iri}> <{OM_HAS_UNIT}> <{unit}> . 
        <{unit}> <{RDF_TYPE}> <{OM_UNIT}> . 
        <{unit}> <{OM_SYMBOL}> "{symbol}"^^<{XSD_STRING}> . 
    """
    triples += triple

    # Create optional comment to quantity
    if comment:
        triples += (
            f"""<{quantity_iri}> <{RDFS_COMMENT}> "{comment}"^^<{XSD_STRING}> . """
        )

    return triples


def instantiated_observations(station_iris: list = None):
    # Returns query to retrieve (all) instantiated observation types per station
    if station_iris:
        iris = ", ".join(["<" + iri + ">" for iri in station_iris])
        substring = f"""FILTER (?station IN ({iris}) ) """
    else:
        substring = f"""
            ?station <{RDF_TYPE}> <{EMS_REPORTING_STATION}> ;
                     <{EMS_DATA_SOURCE}> "Met Office DataPoint" . """
    query = f"""
        SELECT ?station ?stationID ?quantityType
        WHERE {{
            ?station <{EMS_HAS_IDENTIFIER}> ?stationID ;
                     <{EMS_REPORTS}> ?quantity .
            {substring}                     
            ?quantity <{OM_HAS_VALUE}> ?measure ;
                      <{RDF_TYPE}> ?quantityType .
        }}
        ORDER BY ?station
    """
    return query


def instantiated_forecasts(station_iris: list = None):
    # Returns query to retrieve (all) instantiated forecast types per station
    if station_iris:
        iris = ", ".join(["<" + iri + ">" for iri in station_iris])
        substring = f"""FILTER (?station IN ({iris}) ) """
    else:
        substring = f"""
            ?station <{RDF_TYPE}> <{EMS_REPORTING_STATION}> ;
                     <{EMS_DATA_SOURCE}> "Met Office DataPoint" . """
    query = f"""
        SELECT ?station ?stationID ?quantityType
        WHERE {{
            ?station <{EMS_HAS_IDENTIFIER}> ?stationID ;
                     <{EMS_REPORTS}> ?quantity .
            {substring} 
            ?quantity <{EMS_HAS_FORECASTED_VALUE}> ?forecast ;
                      <{RDF_TYPE}> ?quantityType .
        }}
        ORDER BY ?station
    """
    return query


def instantiated_observation_timeseries(station_iris: list = None):
    # Returns query to retrieve (all) instantiated observation time series per station
    if station_iris:
        iris = ", ".join(["<" + iri + ">" for iri in station_iris])
        substring = f"""FILTER (?station IN ({iris}) ) """
    else:
        substring = f"""
            ?station <{RDF_TYPE}> <{EMS_REPORTING_STATION}> ;
                     <{EMS_DATA_SOURCE}> "Met Office DataPoint" . """
    query = f"""
        SELECT ?station ?stationID ?quantityType ?dataIRI ?unit ?tsIRI 
        WHERE {{
            ?station <{EMS_HAS_IDENTIFIER}> ?stationID ;
                     <{EMS_REPORTS}> ?quantity .
            {substring} 
            ?quantity <{OM_HAS_VALUE}> ?dataIRI ;
                      <{RDF_TYPE}> ?quantityType .
            ?dataIRI <{TS_HAS_TIMESERIES}> ?tsIRI ;   
                     <{OM_HAS_UNIT}>/<{OM_SYMBOL}> ?unit .
            ?tsIRI <{RDF_TYPE}> <{TS_TIMESERIES}> .
        }}
        ORDER BY ?tsIRI
    """
    return query


def instantiated_forecast_timeseries(station_iris: list = None):
    # Returns query to retrieve (all) instantiated forecast time series per station
    if station_iris:
        iris = ", ".join(["<" + iri + ">" for iri in station_iris])
        substring = f"""FILTER (?station IN ({iris}) ) """
    else:
        substring = f"""
            ?station <{RDF_TYPE}> <{EMS_REPORTING_STATION}> ;
                     <{EMS_DATA_SOURCE}> "Met Office DataPoint" . """
    query = f"""
        SELECT ?station ?stationID ?quantityType ?dataIRI ?unit ?tsIRI
        WHERE {{
            ?station <{EMS_HAS_IDENTIFIER}> ?stationID ;
                     <{EMS_REPORTS}> ?quantity .
            {substring} 
            ?quantity <{EMS_HAS_FORECASTED_VALUE}> ?dataIRI ;
                      <{RDF_TYPE}> ?quantityType .
            ?dataIRI <{TS_HAS_TIMESERIES}> ?tsIRI ;
                     <{OM_HAS_UNIT}>/<{OM_SYMBOL}> ?unit .
            ?tsIRI <{RDF_TYPE}> <{TS_TIMESERIES}> .
        }}
        ORDER BY ?tsIRI
    """
    return query


def split_insert_query(triples: str, max: int):
    """ "
    Split large SPARQL insert query into list of smaller chunks (primarily
    to avoid heap size/memory issues when executing large SPARQL updated)

    Arguments
        triples - original SPARQL update string with individual triples
                  separated by " . ", i.e. in the form
                  <s1> <p1> <o1> .
                  <s2> <p2> <o2> .
                  ...
        max - maximum number of triples per SPARQL update

    """

    # Initialise list of return queries
    queries = []

    # Split original query every max'th occurrence of " . " and append queries list
    splits = triples.split(" . ")
    cutoffs = list(range(0, len(splits), max))
    cutoffs.append(len(splits))
    for i in range(len(cutoffs) - 1):
        start = cutoffs[i]
        end = cutoffs[i + 1]
        query = " . ".join([t for t in splits[start:end]])
        query = " INSERT DATA { " + query + " } "
        queries.append(query)

    return queries


def update_forecast_creation_datetime(issue_time: str):
    # Returns beginning of update query to update forecast creation times
    query = f"""
        DELETE {{
	        ?forecast <{EMS_CREATED_ON}> ?old }}
        INSERT {{
	        ?forecast <{EMS_CREATED_ON}> \"{issue_time}\"^^<{XSD_DATETIME}> }}
        WHERE {{
	        ?forecast <{EMS_CREATED_ON}> ?old .
            FILTER ( ?forecast IN (
    """

    return query
'''
