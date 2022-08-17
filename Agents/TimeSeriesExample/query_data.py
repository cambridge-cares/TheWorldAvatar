# This module queries the previously instantiated sample data from Blazegraph
# and creates output files suitable for use with the DTVF
# ===============================================================================

import copy
import os.path
import json

# Get the JVM module view (via jpsBaseLibGateWay instance) from the jpsSingletons module to access
# the TimeSeriesClient in the JPB_BASE_LIB
import random

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

# Specify plotting parameters for GeoJSON features (points, polygons, extruded polygons)
circle_props = {'displayName': '',
                'description': '',
                'circle-color': '#ff0000',
                'circle-stroke-width': 1,
                'circle-stroke-color': '#000000',
                'circle-stroke-opacity': 0.75,
                'circle-opacity': 0.66
                }
fill_props = {'displayName': '',
              'description': '',
              'fill-color': '#2669c7',
              'fill-outline-color': '#000000',
              'fill-opacity': 0.66
              }
extrusion_props = {'displayName': '',
                   'description': '',
                   'fill-extrusion-base': 0,
                   'fill-extrusion-height': 0,
                   'fill-extrusion-color': '#666666',
                   'fill-extrusion-opacity': 0.66
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
        Returns coordinates ([lon, lat]), ground elevation (z), and name (label) for given 'consumer'
    '''

    # Define query
    query = utils.create_sparql_prefix('ex') + \
            utils.create_sparql_prefix('rdfs') + \
            '''SELECT ?geom ?name \
               WHERE { <%s> ex:hasGeometry ?geom ; \
                            rdfs:label ?name }''' % consumer
    # Execute query
    response = KGClient.execute(query)

    # Convert JSONArray String back to list
    response = json.loads(response)

    # Unpack consumer name
    name = response[0]['name']
    # Unpack consumer location geometry, and convert to [lon, lat] format
    coords = response[0]['geom'].split('#')
    coords = [float(i) for i in coords]
    # Swap lat and lon
    if not len(coords) % 3 == 0:
        raise IndexError('ERROR: Not all points/vertices are properly defined with 3 coordinates (lat, lon, height)')
    coordinates = [[coords[i+1], coords[i]] for i in range(0, len(coords), 3)]
    # Extract ground elevation
    z = coords[2]

    return coordinates, z, name


def get_metadata(consumer):
    '''
        Returns meta data for given 'consumer'
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


def get_all_time_series(consumer):
    '''
        Returns data for all time series associated with given 'consumer'
    '''

    # Define query
    query = utils.create_sparql_prefix('ex') + \
            utils.create_sparql_prefix('rdfs') + \
            '''SELECT ?dataIRI ?utility ?unit \
               WHERE { <%s> ex:consumes ?dataIRI. \
                       ?dataIRI rdfs:label ?utility ;
                                ex:unit ?unit }''' % consumer
    # Execute query
    response = KGClient.execute(query)

    # Convert JSONArray String back to list
    response = json.loads(response)

    # Initialise lists
    dataIRIs = []
    utilities = []
    units = []
    # Append lists with all query results
    for r in response:
        dataIRIs.append(r['dataIRI'])
        utilities.append(r['utility'])
        units.append((r['unit']))

    # Retrieve time series data for retrieved set of dataIRIs
    timeseries = TSClient.getTimeSeries(dataIRIs)

    # Return time series and associated lists of variables and units
    return timeseries, dataIRIs, utilities, units


# ===============================================================================
# Functions to Structure Retrieved Data for DTVF

def geojson_initialise_dict():
    # Start GeoJSON FeatureCollection
    geojson = {'type': 'FeatureCollection',
               'features': []
               }
    return geojson


def geojson_add_consumer(feature_id, properties, geom_type, coordinates):
    # Define new GeoJSON feature
    feature = {'type': 'Feature',
               'id': int(feature_id),
               'properties': properties,
               'geometry': {'type': geom_type,
                            # Condition coordinates to match GeoJSON requirements:
                            # unwrap by one layers for Points, nest deeper by one layer for Polygons
                            'coordinates': coordinates[0] if geom_type == 'Point' else [coordinates]
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

    # Set Mapbox API key in DTVF 'index.html' file
    utils.set_mapbox_apikey()

    # Initialise remote KG client with only query endpoint specified
    KGClient = jpsBaseLibView.RemoteStoreClient(utils.QUERY_ENDPOINT)

    # Retrieve Java's Instant class to initialise TimeSeriesClient
    Instant = jpsBaseLibView.java.time.Instant
    instant_class = Instant.now().getClass()
    # Initialise TimeSeriesClass
    TSClient = jpsBaseLibView.TimeSeriesClient(instant_class, utils.PROPERTIES_FILE)

    # Initialise output files/dictionaries
    # Please note: This is intentionally not very elegant. It shall highlight what files need to be created for each
    # data set to visualise and respective constraints (i.e. a single data set is restricted to a single locationType)
    # For more details, see: https://github.com/cambridge-cares/TheWorldAvatar/wiki/DTVF:-Data-Structure
    geojson_colleges_points = geojson_initialise_dict()
    geojson_colleges_polygons = geojson_initialise_dict()
    geojson_colleges_buildings = geojson_initialise_dict()
    geojson_departments_polygons = geojson_initialise_dict()
    geojson_departments_buildings = geojson_initialise_dict()
    metadata_colleges_points = []
    metadata_colleges_polygons = []
    metadata_colleges_buildings = []
    metadata_departments_polygons = []
    metadata_departments_buildings = []
    ts_dict = {'ts': [],
               'id': [],
               'units': [],
               'headers': []
               }
    ts_data_colleges_points = copy.deepcopy(ts_dict)
    ts_data_colleges_polygons = copy.deepcopy(ts_dict)
    ts_data_colleges_buildings = copy.deepcopy(ts_dict)
    ts_data_departments_polygons = copy.deepcopy(ts_dict)
    ts_data_departments_buildings = copy.deepcopy(ts_dict)

    # Initialise feature_id count (required to map meta and timeseries data to GeoJSON objects)
    feature_id = 0

    # Get consumers of interest
    consumers = get_all_consumers()
    #consumers = get_consumers_in_circle(center, radius)

    # Loop over all consumers
    for c in consumers:
        feature_id += 1

        # 1) Retrieve data for GeoJSON output
        coords, z, name = get_geojson_data(c)

        # Map retrieved output to correct files/dictionaries
        if 'College' in name:
            # Sample data contains colleges represented as point and polygon
            if len(coords) == 1:
                geometry_type = 'Point'
                geojson_props = [circle_props.copy()]
                geojson = [geojson_colleges_points]
                metadata = [metadata_colleges_points]
                ts_data = [ts_data_colleges_points]
            else:
                geometry_type = 'Polygon'
                # Generate random building height
                building_height = random.randint(20, 30)
                geojson_props = [fill_props.copy(), extrusion_props.copy()]
                geojson = [geojson_colleges_polygons, geojson_colleges_buildings]
                metadata = [metadata_colleges_polygons, metadata_colleges_buildings]
                ts_data = [ts_data_colleges_polygons, ts_data_colleges_buildings]
        elif 'Department' in name:
            # Sample data contains only departments represented as polygon
            geometry_type = 'Polygon'
            # Generate random building height
            building_height = random.randint(20, 30)
            geojson_props = [fill_props.copy(), extrusion_props.copy()]
            geojson = [geojson_departments_polygons, geojson_departments_buildings]
            metadata = [metadata_departments_polygons, metadata_departments_buildings]
            ts_data = [ts_data_departments_polygons, ts_data_departments_buildings]
        else:
            # Clear output file collections for non-matching results
            geojson_props = []
            geojson = []
            metadata = []
            ts_data = []

        # Populate output file/dictionary collections
        for i in range(len(geojson)):
            # Update GeoJSON properties
            geojson_props[i]['description'] = str(c)
            geojson_props[i]['displayName'] = name
            if 'fill-extrusion-base' in geojson_props[i].keys():
                geojson_props[i]['fill-extrusion-base'] = z
                geojson_props[i]['fill-extrusion-height']= building_height
            # Append results to overall GeoJSON FeatureCollection
            geojson[i]['features'].append(geojson_add_consumer(feature_id, geojson_props[i], geometry_type, coords))

            # 2) Retrieve data for metadata output
            lon, lat, elec, water, gas = get_metadata(c)
            metadata[i].append(json_add_metadata(feature_id, lon, lat, elec, water, gas))

            # 3) Retrieve time series data
            timeseries, dataIRIs, utilities, units = get_all_time_series(c)
            # Create lists for TimeSeries objects and IDs
            ts_data[i]['ts'].append(timeseries)
            ts_data[i]['id'].append(feature_id)
            # Create dictionaries for units and headers (Python equivalent for Java HashMaps)
            ts_data[i]['units'].append(dict(zip(dataIRIs, units)))
            ts_data[i]['headers'].append(dict(zip(dataIRIs, utilities)))

    # Create lists for all files to write incl. respective folder and filenames
    geojson = [geojson_colleges_points, geojson_colleges_polygons, geojson_colleges_buildings,
               geojson_departments_polygons, geojson_departments_buildings]
    metadata = [metadata_colleges_points, metadata_colleges_polygons, metadata_colleges_buildings,
                metadata_departments_polygons, metadata_departments_buildings]
    ts_data = [ts_data_colleges_points, ts_data_colleges_polygons, ts_data_colleges_buildings,
               ts_data_departments_polygons, ts_data_departments_buildings]
    ts_json = []
    for ts in ts_data:
        # Retrieve all time series data for collected 'ts_data' from Java TimeSeriesClient at once
        ts = TSClient.convertToJSON(ts['ts'], ts['id'], ts['units'], ts['headers'])
        # Make JSON file readable in Python
        ts = json.loads(ts.toString())
        ts_json.append(ts)
    folder = ['2D', '2D', '3D', '2D', '3D']
    filename = ['colleges-2', 'colleges-1', 'colleges-1', 'departments-1', 'departments-1']

    # Write all output files
    for i in range(len(geojson)):
        # Write GeoJSON dictionaries formatted to file
        file_name = os.path.join(utils.OUTPUT_DIR, folder[i], filename[i] + '.geojson')
        with open(file_name, 'w') as f:
            json.dump(geojson[i], indent=4, fp=f)
        file_name = os.path.join(utils.OUTPUT_DIR, folder[i], filename[i] + '-meta.json')
        with open(file_name, 'w') as f:
            json.dump(metadata[i], indent=4, fp=f)
        file_name = os.path.join(utils.OUTPUT_DIR, folder[i], filename[i] + '-timeseries.json')
        with open(file_name, 'w') as f:
            json.dump(ts_json[i], indent=4, fp=f)
