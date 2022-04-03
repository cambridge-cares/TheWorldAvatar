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
        {create_sparql_prefix('geo')}
        {create_sparql_prefix('rdf')}
        {create_sparql_prefix('rdfs')}
        {create_sparql_prefix('ems')}
        {create_sparql_prefix('kb')}
        SELECT ?id
        WHERE {{
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
        triples = f"<{station_iri}> rdf:type ems:ReportingStation ; "
        if dataSource: triples += f"ems:dataSource \"{dataSource}\"^^xsd:string ; "
        if comment: triples += f"rdfs:comment \"{comment}\"^^xsd:string ; "
        if id: triples += f"ems:hasIdentifier \"{id}\"^^xsd:string ; "
        if location: triples += f"ems:hasObservationLocation \"{location}\"^^geo:lat-lon ; "
        if elevation: triples += f"ems:hasObservationElevation \"{elevation}\"^^xsd:float ; "
    else:
        triples = None
    
    return triples
