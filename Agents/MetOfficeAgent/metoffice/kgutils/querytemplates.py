###############################################
# Authors: Markus Hofmeister (mh807cam.ac.uk) #    
# Date: 01 Apr 2022                           #
###############################################

# The purpose of this module is to provide templates for (frequently)
# required SPARQL queries

import uuid

from metoffice.kgutils.prefixes import PREFIXES
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


def instantiate_om_quantity(station, quantity_type_str, quantity_type,
                            comment, unitsymbol, observation=True):
    """
        Create 
    """
    # Create IRI for reported quantity
    quantity = PREFIXES['kb'] + quantity_type_str + '_' + str(uuid.uuid4())
    # Create IRI for measure/forecast
    if observation:
        iri = PREFIXES['kb'] + mf_type_string + '_' + str(uuid.uuid4())
        substring = f"""<{quantity}> om:hasValue <{iri}> ."""
    else:
        iri = PREFIXES['kb'] + mf_type_string + '_' + str(uuid.uuid4())
        substring = f"""<{quantity}> om:hasForecastedValue <{iri}> ."""
    
    # Construct query
    quantity = PREFIXES['kb'] + quantity_type_str + '_' + str(uuid.uuid4())
    query = f"""
        {create_sparql_prefix('rdf')}
        {create_sparql_prefix('rdfs')}
        {create_sparql_prefix('om')}
        {create_sparql_prefix('xsd')}
        {create_sparql_prefix('ems')}
        {create_sparql_prefix('kb')}
        INSERT DATA {{
            <{station}> ems:reports <{quantity}> .
            <{quantity}> rdf:type {quantity_type} .
            <{quantity}> rdfs:comment "{comment}"^^xsd:string .
            {substring}
            <{iri}> rdf:type {mf_type} .
            <{iri}> om:hasUnit om:Unit .
            om:Unit om:symbol "{unitsymbol}"^^xsd:string .
    """

def add_quantity(quantity: str):
    # Create IRI for instance of subclass of quantity
    iri = PREFIXES['kb'] + '{quantity}_' + str(uuid.uuid4())
    triple = f"<{iri}> rdf:type ems:{quantity} ."
    return triple, iri