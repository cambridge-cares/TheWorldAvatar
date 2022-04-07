###############################################
# Authors: Markus Hofmeister (mh807cam.ac.uk) #    
# Date: 01 Apr 2022                           #
###############################################

# The purpose of this module is to provide templates for (frequently)
# required SPARQL queries

from metoffice.kgutils.prefixes import create_sparql_prefix


def all_metoffice_station_ids() -> str:
    # Returns query to retrieve all identifiers of instantiated stations
    query = f"""
        {create_sparql_prefix('rdf')}
        {create_sparql_prefix('ems')}
        SELECT ?id
        WHERE {{
            ?station rdf:type ems:ReportingStation ;
                     ems:dataSource "Met Office DataPoint" ;
                     ems:hasIdentifier ?id 
              }}
    """
    return query


def all_metoffice_stations(circle_center: str = None,
                           circle_radius: str = None) -> str:
    # Returns query to retrieve identifiers and IRIs of instantiated stations
    if not circle_center and not circle_radius:
        # Retrieve all stations
        query = f"""
            {create_sparql_prefix('rdf')}
            {create_sparql_prefix('ems')}
            SELECT ?id ?station
            WHERE {{
                ?station rdf:type ems:ReportingStation ;
                        ems:dataSource "Met Office DataPoint" ;
                        ems:hasIdentifier ?id 
                }}
        """
    else:
        # Retrieve only stations in provided circle (radius in km)
        query = f"""
            {create_sparql_prefix('rdf')}
            {create_sparql_prefix('ems')}
            {create_sparql_prefix('geo')}
            {create_sparql_prefix('geolit')}
            SELECT ?id ?station
            WHERE {{
                  SERVICE geo:search {{
                    ?station geo:search "inCircle" .
                    ?station geo:predicate ems:hasObservationLocation .
                    ?station geo:searchDatatype geolit:lat-lon .
                    ?station geo:spatialCircleCenter "{circle_center}" .
                    ?station geo:spatialCircleRadius "{circle_radius}" . 
                }}
                ?station rdf:type ems:ReportingStation ;
                         ems:dataSource "Met Office DataPoint" ;
                         ems:hasIdentifier ?id 
                }}
        """
    
    return query


def add_station_data(station_iri: str = None, dataSource: str = None, 
                     comment: str = None, id: str = None, location: str = None,
                     elevation: float = None) -> str:
    if station_iri:
        # Returns triples to instantiate a measurement station according to OntoEMS
        triples = f"<{station_iri}> rdf:type ems:ReportingStation . "
        if dataSource: triples += f"<{station_iri}> ems:dataSource \"{dataSource}\"^^xsd:string . "
        if comment: triples += f"<{station_iri}> rdfs:comment \"{comment}\"^^xsd:string . "
        if id: triples += f"<{station_iri}> ems:hasIdentifier \"{id}\"^^xsd:string . "
        if location: triples += f"<{station_iri}> ems:hasObservationLocation \"{location}\"^^geolit:lat-lon . "
        if elevation: triples += f"<{station_iri}> ems:hasObservationElevation \"{elevation}\"^^xsd:float . "
    else:
        triples = None
    
    return triples


def add_om_quantity(station_iri, quantity_iri, quantity_type, data_iri,
                    data_iri_type, unit, symbol, is_observation: bool, 
                    creation_time=None, comment=None):
    """
        Create triples to instantiate station measurements
    """
    # Create triple for measure vs. forecast
    if is_observation:
        triple = f"""<{quantity_iri}> om:hasValue <{data_iri}> . """
        
    else:
        triple = f"<{quantity_iri}> ems:hasForecastedValue <{data_iri}> . "
        if creation_time: triple += f"<{data_iri}> ems:createdOn \"{creation_time}\"^^xsd:dateTime . "

    # Create triples to instantiate station measurement according to OntoEMS
    triples = f"""
        <{station_iri}> ems:reports <{quantity_iri}> . 
        <{quantity_iri}> rdf:type <{quantity_type}> .
        <{data_iri}> rdf:type <{data_iri_type}> .
        <{data_iri}> om:hasUnit {unit} .
        {unit} om:symbol "{symbol}"^^xsd:string .
    """
    triples += triple

    # Create optional comment to quantity
    if comment:
        triples += f"""<{quantity_iri}> rdfs:comment "{comment}"^^xsd:string . """

    return triples


def all_instantiated_observations():
    # Returns query to retrieve all instantiated observation types per station
    query = f"""
        {create_sparql_prefix('rdf')}
        {create_sparql_prefix('om')}
        {create_sparql_prefix('ems')}
        SELECT ?station ?stationID ?quantityType
        WHERE {{
            ?station rdf:type ems:ReportingStation ;
                     ems:dataSource "Met Office DataPoint" ;
                     ems:hasIdentifier ?stationID ;
                     ems:reports ?quantity .
            ?quantity om:hasValue ?measure ;
                      rdf:type ?quantityType .
        }}
        ORDER BY ?station
    """
    return query


def all_instantiated_forecasts():
    # Returns query to retrieve all instantiated forecast types per station
    query = f"""
        {create_sparql_prefix('rdf')}
        {create_sparql_prefix('om')}
        {create_sparql_prefix('ems')}
        SELECT ?station ?stationID ?quantityType
        WHERE {{
            ?station rdf:type ems:ReportingStation ;
                     ems:dataSource "Met Office DataPoint" ;
                     ems:hasIdentifier ?stationID ;
                     ems:reports ?quantity .
            ?quantity ems:hasForecastedValue ?forecast ;
                      rdf:type ?quantityType .
        }}
        ORDER BY ?station
    """
    return query


def all_instantiated_observation_timeseries():
    # Returns query to retrieve all instantiated observation time series per station
    query = f"""
        {create_sparql_prefix('rdf')}
        {create_sparql_prefix('om')}
        {create_sparql_prefix('ems')}
        {create_sparql_prefix('ts')}
        SELECT ?station ?stationID ?quantityType ?dataIRI ?tsIRI
        WHERE {{
            ?station rdf:type ems:ReportingStation ;
                     ems:dataSource "Met Office DataPoint" ;
                     ems:hasIdentifier ?stationID ;
                     ems:reports ?quantity .
            ?quantity om:hasValue ?dataIRI ;
                      rdf:type ?quantityType .
            ?dataIRI ts:hasTimeSeries ?tsIRI .   
        }}
        ORDER BY ?tsIRI
    """
    return query


def all_instantiated_forecast_timeseries():
    # Returns query to retrieve all instantiated forecast time series per station
    query = f"""
        {create_sparql_prefix('rdf')}
        {create_sparql_prefix('om')}
        {create_sparql_prefix('ems')}
        {create_sparql_prefix('ts')}
        SELECT ?station ?stationID ?quantityType ?dataIRI ?tsIRI
        WHERE {{
            ?station rdf:type ems:ReportingStation ;
                     ems:dataSource "Met Office DataPoint" ;
                     ems:hasIdentifier ?stationID ;
                     ems:reports ?quantity .
            ?quantity ems:hasForecastedValue ?dataIRI ;
                      rdf:type ?quantityType .
            ?dataIRI ts:hasTimeSeries ?tsIRI .   
        }}
        ORDER BY ?tsIRI
    """
    return query
