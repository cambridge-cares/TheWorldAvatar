###############################################
# Authors: Markus Hofmeister (mh807cam.ac.uk) #    
# Date: 01 Apr 2022                           #
###############################################

# The purpose of this module is to provide templates for (frequently)
# required SPARQL queries

from metoffice.kgoperations.prefixes import create_sparql_prefix


def all_metoffice_station_ids():
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
