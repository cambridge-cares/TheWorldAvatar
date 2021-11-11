# This module instantiates several sample geospatial time series (i.e. sample
# consumption time series for various utilities (water, gas, electricity) for
# several consumers with location given as geospatial point
# ===============================================================================

import os.path
import json


# Get the JVM module view (via jpsBaseLibGateWay instance) from the jpsSingletons module to access
# the TimeSeriesClient in the JPB_BASE_LIB
from jpsSingletons import jpsBaseLibView
# Get settings and functions from utils module
import utils

# ===============================================================================
# Specify Relevant Data
# All coordinates are given in EPSG:4326 CRS, neglecting any elevation (i.e. Z coordinate)

# Center of Cambridge (Market Square) 'lat#lon'
center = '52.205363#0.119115'
# Search radius in km
radius = 1

# Specify plotting parameter for GeoJSON features
geojson_props = { 'displayName': '',
                  'description': '',
                  'circle-color': '#FF0000',
                  'circle-stroke-width': 1,
                  'circle-stroke-color': '#000000',
                  'circle-stroke-opacity': 0.75,
                  'circle-opacity': 0.75
                  }

# ===============================================================================
# Functions to Query Example Data from KG

def get_all_consumers():
    '''
        Returns all consumers instantiated in KG as list
    '''

    # Define query
    query = utils.create_sparql_prefix('ex') + \
            utils.create_sparql_prefix('rdf') + \
            '''SELECT ?cons \
               WHERE { ?cons rdf:type ex:Consumer }'''
    # Execute query
    response = KGClient.execute(query)

    # Convert JSONArray String back to list
    response = json.loads(response)
    # Unpack all consumers to list
    consumers = [r['cons'] for r in response]

    return consumers


def get_consumers_in_circle(center, radius):
    '''
        Returns all instantiated consumers within a radius of 'radius' km from 'center' as list
    '''

    # Define query
    query = utils.create_sparql_prefix('ex') + \
            utils.create_sparql_prefix('rdf') + \
            utils.create_sparql_prefix('geo') + \
            utils.create_sparql_prefix('geolit') + \
            '''SELECT ?cons \
               WHERE { \
                 SERVICE geo:search \
                 { \
                    ?cons geo:search "inCircle" . \
                    ?cons geo:searchDatatype geolit:lat-lon . \
                    ?cons geo:predicate ex:hasLocation . \
                    ?cons geo:spatialCircleCenter "%s" . \
                    ?cons geo:spatialCircleRadius "%s" . \
                 } \
                 ?cons rdf:type ex:Consumer \
               }''' % (str(center), str(radius))

    # Execute query
    response = KGClient.execute(query)

    # Convert JSONArray String back to list
    response = json.loads(response)
    # Unpack all consumers to list
    consumers = [r['cons'] for r in response]

    return consumers


def get_geojson_data(consumer):
    '''
        Returns coordinates ([lon, lat]) and name (label) for given 'consumer'
    '''

    # Define query
    query = utils.create_sparql_prefix('ex') + \
            utils.create_sparql_prefix('rdfs') + \
            '''SELECT ?loc ?name \
               WHERE { <%s> ex:hasLocation ?loc ; \
                            rdfs:label ?name }''' % consumer
    # Execute query
    response = KGClient.execute(query)

    # Convert JSONArray String back to list
    response = json.loads(response)

    # Unpack consumer name
    name = response[0]['name']
    # Unpack consumer location, and convert to [lon, lat] format
    coordinates = response[0]['loc'].split('#')
    coordinates = [float(i) for i in coordinates]
    coordinates = coordinates[::-1]

    return coordinates, name


def get_metadata(consumer):
    '''
        Returns coordinates ([lon, lat]) and name (label) for given 'consumer'
    '''

    # Define query
    query = utils.create_sparql_prefix('ex') + \
            utils.create_sparql_prefix('rdfs') + \
            '''SELECT ?loc ?utility \
               WHERE { <%s> ex:hasLocation ?loc ; \
                            ex:consumes ?dataIRI. \
                       ?dataIRI rdfs:label ?utility }''' % consumer
    # Execute query
    response = KGClient.execute(query)

    # Convert JSONArray String back to list
    response = json.loads(response)

    # Unpack consumer location
    coordinates = response[0]['loc'].split('#')
    lon = coordinates[1]
    lat = coordinates[0]

    # Derive consumed utilities
    elec, water, gas = 'no', 'no', 'no'
    for r in response:
        if str.startswith(r['utility'], 'Electricity'):
            elec = 'yes'
        elif str.startswith(r['utility'], 'Water'):
            water = 'yes'
        elif str.startswith(r['utility'], 'Gas'):
            gas = 'yes'

    return lon, lat, elec, water, gas


# ===============================================================================
# Functions to Structure Retrieved Data for DTVF

def geojson_initialise_dict():
    # Start GeoJSON FeatureCollection
    geojson = {'type': 'FeatureCollection',
               'features': []
               }
    return geojson


def geojson_add_consumer(feature_id, properties, coordinates):
    # Define new GeoJSON feature
    feature = {'type': 'Feature',
               'id': int(feature_id),
               'properties': properties.copy(),
               'geometry': {'type': 'Point',
                            'coordinates': coordinates
                            }
               }
    return feature


def json_add_metadata(feature_id, lon, lat, elec, water, gas):
    # Define metadata dictionary
    metadata = { 'id': feature_id,
                 'Longitude': lon,
                 'Latitude': lat,
                 'Consumes electricity': elec,
                 'Consumes water': water,
                 'Consumes gas': gas
                 }
    return metadata

# ===============================================================================
# Retrieve Example Data from KG and Store as Files for DTVF

if __name__ == '__main__':

    # Initialise remote KG client with only query endpoint specified
    KGClient = jpsBaseLibView.RemoteStoreClient(utils.QUERY_ENDPOINT)

    # Retrieve Java's Instant class to initialise TimeSeriesClient
    Instant = jpsBaseLibView.java.time.Instant
    instant_class = Instant.now().getClass()
    # Initialise TimeSeriesClass
    TSClient = jpsBaseLibView.TimeSeriesClient(instant_class, utils.PROPERTIES_FILE)

    # Initialise output files/dictionaries
    geojson = geojson_initialise_dict()
    metadata = []
    feature_id = 0

    # Get consumers of interest
    #consumers = get_all_consumers()
    consumers = get_consumers_in_circle(center, radius)

    # Loop over all consumers
    for c in consumers:
        feature_id += 1

        # 1) Retrieve data for GeoJSON output
        coords, name = get_geojson_data(c)

        # Update GeoJSON properties
        geojson_props['description'] = str(c)
        geojson_props['displayName'] = name
        # Append results to overall GeoJSON FeatureCollection
        geojson['features'].append(geojson_add_consumer(feature_id, geojson_props, coords))

        # 2) Retrieve data for metadata output
        lon, lat, elec, water, gas = get_metadata(c)
        metadata.append(json_add_metadata(feature_id, lon, lat, elec, water, gas))


    # Write GeoJSON dictionary formatted to file
    file_name = os.path.join(utils.OUTPUT_DIR, 'consumers.geojson')
    with open(file_name, 'w') as f:
        json.dump(geojson, indent=4, fp=f)
    file_name = os.path.join(utils.OUTPUT_DIR, 'consumers-meta.json')
    with open(file_name, 'w') as f:
        json.dump(metadata, indent=4, fp=f)




