##########################################
# Author: Feroz Farazi (msff2@cam.ac.uk) #
# Date:  27 Jan 2022                      #
##########################################

# Get settings and functions from the kg utils module
import kg_utils
import json
# Get the JVM module view (via jpsBaseLibGateWay instance) from the jpsSingletons module to access
# the TimeSeriesClient in the JPB_BASE_LIB
from jpsSingletons import jpsBaseLibView

# Specify plotting properties for GeoJSON features
geojson_attributes = { 'displayName': '',
                  'description': '',
                  'circle-color': '#FF0000',
                  'circle-stroke-width': 1,
                  'circle-stroke-color': '#000000',
                  'circle-stroke-opacity': 0.75,
                  'circle-opacity': 0.75
                  }

def format_in_geojson(feature_id, properties, coordinates):
    """
       It structures geodata of terminals into geoJSON format.
    """
    feature = {'type': 'Feature',
               'id': int(feature_id),
               'properties': properties.copy(),
               'geometry': {'type': 'Point',
                            'coordinates': coordinates
                            }
               }
    return feature

def get_all_terminal_geodata(KGClient):
    '''
        Returns coordinates ([lon, lat]) and name (label) for all terminals
    '''

# SPARQL query string
    query = kg_utils.create_sparql_prefix('rdf') + \
            kg_utils.create_sparql_prefix('rdfs') + \
            kg_utils.create_sparql_prefix('loc') + \
            kg_utils.create_sparql_prefix('comp') + \
            '''SELECT ?location ?name
                WHERE
                {
                    ?term rdf:type comp:GasTerminal ;
                        rdfs:label ?name ;
                        loc:lat-lon ?location.
                }'''
    # Execute query
    response = KGClient.execute(query)

    # Convert JSONArray String back to list
    response = json.loads(response)
    terminal_coorindates = dict()
    for r in response:
        coordinates = r['location'].split('#')
        coordinates = [float(i) for i in coordinates]
        coordinates = coordinates[::-1]
        terminal_coorindates[r['name'].lower()] = coordinates
    return terminal_coorindates

def get_all_terminals(KGClient):
    '''
        Returns all terminals instantiated in KG as list
    '''
    # Initialise SPARQL query variables for gas terminal IRIs and names
    var1, var2 = 'iri', 'name'

    # Define query
    query = kg_utils.create_sparql_prefix('comp') + \
            kg_utils.create_sparql_prefix('rdf') + \
            kg_utils.create_sparql_prefix('rdfs') + \
            'SELECT distinct ?' + var1 + ' ?' + var2 + ' ' \
            'WHERE { ?' + var1 + ' rdf:type comp:GasTerminal; \
                                   rdfs:label ?' + var2 + '. }'
    # Execute query
    response = KGClient.execute(query)
    # Convert JSONArray String back to list
    response = json.loads(response)
    # A key-value paired list where key is the name and
    # value is the IRI of terminal
    terminals = dict()
    for r in response:
        terminals[r[var2]] = r[var1]
    return terminals

def geojson_initialise_dict():
    # Start GeoJSON FeatureCollection
    geojson = {'type': 'FeatureCollection',
               'features': []
               }
    return geojson

def generate_all_visualisation_data():
    """
       Generates all data for Gas Grid visualisation.
    """
    # Set Mapbox API key in DTVF 'index.html' file
    kg_utils.set_mapbox_apikey()

    # Initialise remote KG client with only query endpoint specified
    KGClient = jpsBaseLibView.RemoteStoreClient(kg_utils.QUERY_ENDPOINT)

    # Retrieve Java's Instant class to initialise TimeSeriesClient
    Instant = jpsBaseLibView.java.time.Instant
    instant_class = Instant.now().getClass()
    # Initialise TimeSeriesClass
    TSClient = jpsBaseLibView.TimeSeriesClient(instant_class, kg_utils.PROPERTIES_FILE)
    # Initialise a dictionary for geoJSON outputs
    geojson = geojson_initialise_dict()
    # Initialise an array for metadata outputs
    metadata = []
    # Initialise an array with four elements for timeseries outputs
    ts_data = { 'ts': [],
                'id': [],
                'units': [],
                'headers': []
                }
    feature_id = 0

    # Get all terminals of interest
    terminals = get_all_terminals(KGClient)
    # Retrieve all geocoordinates of terminals for GeoJSON output
    terminal_coordinates = get_all_terminal_geodata(KGClient)

    # Iterate over all terminals
    for terminal, iri in terminals.items():
        feature_id += 1
        # Update GeoJSON properties
        geojson_attributes['description'] = str(terminal)
        geojson_attributes['displayName'] = terminal
        # Append results to overall GeoJSON FeatureCollection
        if terminal.lower() in terminal_coordinates:
            print('terminal_coordinates[terminal.lower()]:', terminal_coordinates[terminal.lower()])
            geojson['features'].append(format_in_geojson(feature_id, geojson_attributes, terminal_coordinates[terminal.lower()]))


if __name__ == '__main__':
    """
    If this module is executed, it will generate all data required for the
    Digital Twin Visualisation Framework.
    """
    generate_all_visualisation_data()