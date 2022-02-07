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

if __name__ == '__main__':
    """
    If this module is executed, it will generate all data required for the
    Digital Twin Visualisation Framework.
    """
    generate_all_visualisation_data()