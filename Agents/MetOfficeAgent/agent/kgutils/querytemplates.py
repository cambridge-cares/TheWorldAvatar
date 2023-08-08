################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 01 Apr 2022                            #
################################################

# The purpose of this module is to provide templates for (frequently)
# required SPARQL queries

from agent.datamodel import *
from agent.kgutils.stackclients import PostGISClient


def all_metoffice_station_ids() -> str:
    # Returns query to retrieve all identifiers of instantiated stations
    query = f"""
        SELECT ?id
        WHERE {{
            ?station <{RDF_TYPE}> <{EMS_REPORTING_STATION}> ;
                     <{EMS_DATA_SOURCE}> "Met Office DataPoint" ;
                     <{EMS_HAS_IDENTIFIER}> ?id 
              }}
    """
    return query


def filter_stations_in_circle(circle_center: str, circle_radius: str):
    # Retrieve stationIRIs for stations of interest
    lat, lon = circle_center.split('#')
    lat = float(lat)
    lon = float(lon)
    circle_radius = float(circle_radius)
    postgis_client = PostGISClient()
    iris = postgis_client.get_feature_iris_in_circle(lat, lon, circle_radius)
    iris = ', '.join(f'<{iri}>' for iri in iris)
    filter_expression = f'FILTER ( ?station IN ({iris}) )'
    
    return filter_expression


def instantiated_metoffice_stations(circle_center: str = None,
                                    circle_radius: str = None) -> str:
    if circle_center and circle_radius:
        # Retrieve only stations in provided circle (radius in km)
        filter_expression = filter_stations_in_circle(circle_center, circle_radius)
    else:
        # Returns query to retrieve all instantiated station details
        filter_expression = ''
    
    # Construct query
    query = f"""
        SELECT ?stationID ?station
        WHERE {{
        {filter_expression}
        ?station <{RDF_TYPE}> <{EMS_REPORTING_STATION}> ;
                    <{EMS_DATA_SOURCE}> "Met Office DataPoint" ;
                    <{EMS_HAS_IDENTIFIER}> ?stationID .
            }}
    """
    
    return query


def instantiated_metoffice_stations_with_details(circle_center: str = None,
                                                 circle_radius: str = None) -> str:   
    if circle_center and circle_radius:
        # Retrieve only stations in provided circle (radius in km)
        filter_expression = filter_stations_in_circle(circle_center, circle_radius)
    else:
        # Returns query to retrieve all instantiated station details
        filter_expression = ''
    
    # Construct query
    query = f"""
        SELECT ?stationID ?station ?label ?elevation ?dataIRI_obs ?dataIRI_fc
        WHERE {{
        {filter_expression}
        ?station <{RDF_TYPE}> <{EMS_REPORTING_STATION}> ;
                    <{EMS_DATA_SOURCE}> "Met Office DataPoint" ;
                    <{EMS_HAS_IDENTIFIER}> ?stationID .
        OPTIONAL {{ ?station <{RDFS_LABEL}> ?label }}
        OPTIONAL {{ ?station <{EMS_HAS_OBSERVATION_ELEVATION}> ?elevation }}
        OPTIONAL {{ ?station <{EMS_REPORTS}>/<{OM_HAS_VALUE}> ?dataIRI_obs }}
        OPTIONAL {{ ?station <{EMS_REPORTS}>/<{EMS_HAS_FORECASTED_VALUE}> ?dataIRI_fc }}

            }}
    """
    
    return query


def geospatial_station_info(station_iris: list = None,
                            ontop_endpoint: str = None) -> str:
    """
    Returns query to retrieve geospatial station information (via Ontop)

    Arguments:
        station_iris: list of station IRIs for which to retrieve geospatial information
                      (provide info for all stations if not provided)
        federation_endpoint: Returns query as federated query (i.e. using SERVICE keyword)
                             to query Ontop via Blazegraph if ontop_endpoint provided;
                             otherwise returns query to be used with Ontop client directly
                             --> Using SERVICE keyword seems to resolve several connection 
                                 issues with using Ontop client directly
    """
    if station_iris:
        # Use FILTER IN expression
        #iris = ', '.join(['<'+iri+'>' for iri in station_iris])
        #filter_expression = f'FILTER (?station IN ({iris}) ) '
        # Use VALUES expression
        iris = ' '.join(['<'+iri+'>' for iri in station_iris])
        filter_expression = f'VALUES ?station {{ {iris} }} '
    else:
        filter_expression = ''

    if ontop_endpoint:
        service_expression = f'SERVICE <{ontop_endpoint}> {{ '
    else:
        service_expression = ''

    query = f"""
        SELECT ?station ?wkt
        WHERE {{
            {service_expression}
            {filter_expression}
            ?station <{RDF_TYPE}> <{GEO_FEATURE}> ;
                     <{GEO_HAS_GEOMETRY}>/<{GEO_ASWKT}> ?wkt 
        }}
    """
    if ontop_endpoint:
        query += f' }}'

    return query


def add_station_data(station_iri: str = None, dataSource: str = None, 
                     label: str = None, id: str = None, elevation: float = None) -> str:
    if station_iri:
        # Returns triples to instantiate a measurement station according to OntoEMS
        triples = f"<{station_iri}> <{RDF_TYPE}> <{EMS_REPORTING_STATION}> . "
        if dataSource: triples += f"<{station_iri}> <{EMS_DATA_SOURCE}> \"{dataSource}\"^^<{XSD_STRING}> . "
        if label: triples += f"<{station_iri}> <{RDFS_LABEL}> \"{label}\"^^<{XSD_STRING}> . "
        if id: triples += f"<{station_iri}> <{EMS_HAS_IDENTIFIER}> \"{id}\"^^<{XSD_STRING}> . "
        if elevation: triples += f"<{station_iri}> <{EMS_HAS_OBSERVATION_ELEVATION}> \"{elevation}\"^^<{XSD_FLOAT}> . "
    else:
        triples = None
    
    return triples


def add_om_quantity(station_iri, quantity_iri, quantity_type, data_iri,
                    data_iri_type, unit, is_observation: bool, 
                    creation_time=None, comment=None):
    """
        Create triples to instantiate station measurements
    """
    # Create triple for measure vs. forecast
    if is_observation:
        triple = f"""<{quantity_iri}> <{OM_HAS_VALUE}> <{data_iri}> . """
        
    else:
        triple = f"<{quantity_iri}> <{EMS_HAS_FORECASTED_VALUE}> <{data_iri}> . "
        if creation_time: triple += f"<{data_iri}> <{EMS_CREATED_ON}> \"{creation_time}\"^^<{XSD_DATETIME}> . "

    # Create triples to instantiate station measurement according to OntoEMS
    triples = f"""
        <{station_iri}> <{EMS_REPORTS}> <{quantity_iri}> . 
        <{quantity_iri}> <{RDF_TYPE}> <{quantity_type}> . 
        <{data_iri}> <{RDF_TYPE}> <{data_iri_type}> . 
        <{data_iri}> <{OM_HAS_UNIT}> <{unit}> . 
        <{unit}> <{RDF_TYPE}> <{OM_UNIT}> . 
    """
    triples += triple

    # Create optional comment to quantity
    if comment:
        triples += f"""<{quantity_iri}> <{RDFS_COMMENT}> "{comment}"^^<{XSD_STRING}> . """

    return triples


def instantiated_observations(station_iris: list = None):
    # Returns query to retrieve (all) instantiated observation types per station
    if station_iris:
        iris = ', '.join(['<'+iri+'>' for iri in station_iris])
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
        iris = ', '.join(['<'+iri+'>' for iri in station_iris])
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
        iris = ', '.join(['<'+iri+'>' for iri in station_iris])
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
        iris = ', '.join(['<'+iri+'>' for iri in station_iris])
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
    """"
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
    splits = triples.split(' . ')
    cutoffs = list(range(0, len(splits), max))
    cutoffs.append(len(splits))
    for i in range(len(cutoffs)-1):
        start = cutoffs[i]
        end = cutoffs[i+1]
        query = ' . '.join([t for t in splits[start:end]])
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
