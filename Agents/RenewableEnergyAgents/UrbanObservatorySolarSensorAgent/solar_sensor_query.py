###########################################
# Authors: Toby Latcham (tjl47@cam.ac.uk) #
#          Sophie Hall (sh2000@cam.ac.uk) #
# Date: 11 Feb 2022                       #
###########################################

"""This module queries the previously instantiated solar sensor data from
 Blazegraph and creates output files suitable for use with the DTVF
"""

import os.path
import json

# Get the JVM module view (via jpsBaseLibGateWay instance) from the jpsSingletons
# module to access the TimeSeriesClient in the JPB_BASE_LIB
from Utils.jpsSingletons import jpsBaseLibView
# Get settings and functions from utils module
import Utils.utils as utils
# Power calculations module
from Utils.power_module import solar_power

# ===============================================================================
# All coordinates are given in EPSG:4326 CRS, neglecting any elevation (i.e. Z coordinate)

center = '54.9751#1.6066'
# Search radius in km
radius = 2000

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
# Functions to Query MIDAS Data from KG

def get_all_sensors(KGClient):
    '''Returns all sensors instantiated in KG as list.'''

    # Define query
    query = utils.create_sparql_prefix('ontoweather') + \
            utils.create_sparql_prefix('rdf') + \
            '''SELECT ?sens \
               WHERE { ?sens rdf:type ontoweather:SolarSensor }'''
    # Execute query
    response = KGClient.execute(query)

    # Convert JSONArray String back to list
    response = json.loads(response)
    # Unpack all sensors to list
    sensors = [r['sens'] for r in response]

    return sensors


def get_sensors_in_circle(center, radius, KGClient):
    '''Returns all instantiated sensors within a radius of 'radius' km from 'center' as list'''

    # Define query
    query = utils.create_sparql_prefix('ontoweather') + \
            utils.create_sparql_prefix('rdf') + \
            utils.create_sparql_prefix('geo') + \
            utils.create_sparql_prefix('geolit') + \
            '''SELECT ?sens \
               WHERE { \
                 SERVICE geo:search \
                 { \
                    ?sens geo:search "inCircle" . \
                    ?sens geo:searchDatatype geolit:lat-lon . \
                    ?sens geo:predicate ex:hasLocation . \
                    ?sens geo:spatialCircleCenter "%s" . \
                    ?sens geo:spatialCircleRadius "%s" . \
                 } \
                 ?sens rdf:type ontoweather:SolarSensor \
               }''' % (str(center), str(radius))

    # Execute query
    response = KGClient.execute(query)

    # Convert JSONArray String back to list
    response = json.loads(response)
    # Unpack all sensors to list
    sensors = [r['sens'] for r in response]

    return sensors


def get_geojson_data(sensor, KGClient):
    '''Returns coordinates ([lon, lat]) and name (label) for given 'sensor'.'''

    # Define query
    query = utils.create_sparql_prefix('ontoweather') + \
            '''SELECT ?loc ?name \
               WHERE { <%s> ontoweather:WGS84LatitudeLongitude ?loc ; \
                            ontoweather:hasName ?name }''' % sensor
    # Execute query
    response = KGClient.execute(query)

    # Convert JSONArray String back to list
    response = json.loads(response)

    # Unpack sensor name
    name = response[0]['name']
    # Unpack sensor location, and convert to [lon, lat] format
    coordinates = response[0]['loc'].split('#')
    coordinates = [float(i) for i in coordinates]
    coordinates = coordinates[::-1]

    return coordinates, name


def get_metadata(sensor, KGClient):
    '''Returns meta data for given 'sensor'.'''

    # Define query
    query = utils.create_sparql_prefix('ontoweather') + \
            '''SELECT ?loc ?measurements ?desc \
               WHERE { <%s> ontoweather:WGS84LatitudeLongitude ?loc ; \
                            ontoweather:observesSolarProperty ?dataIRI. \
                       ?dataIRI ontoweather:hasTimeseries ?measurements ; \
                                ontoweather:hasDescription ?desc . }''' % sensor
    # Execute query
    response = KGClient.execute(query)

    # Convert JSONArray String back to list
    response = json.loads(response)

    # Unpack sensor location
    coordinates = response[0]['loc'].split('#')
    lon = coordinates[1]
    lat = coordinates[0]

    desc = response[0]['desc']

    return lon, lat, desc


def get_all_time_series(sensor, KGClient, TSClient):
    '''Returns data for all time series associated with given 'sensor'.'''

    # Define query
    query = utils.create_sparql_prefix('ontoweather') + \
            utils.create_sparql_prefix('om') + \
            '''SELECT ?dataIRI ?measurements ?unit \
               WHERE { <%s> ontoweather:observesSolarProperty ?dataIRI. \
                       ?dataIRI ontoweather:hasTimeseries ?measurements ;
                                om:hasUnit ?unit }''' % sensor
    # Execute query
    response = KGClient.execute(query)

    # Convert JSONArray String back to list
    response = json.loads(response)

    # Initialise lists
    dataIRIs = []
    measurements = []
    units = []
    # Append lists with all query results
    for r in response:
        dataIRIs.append(r['dataIRI'])
        measurements.append(r['measurements'])
        units.append((r['unit']))

    # Retrieve time series data for retrieved set of dataIRIs
    timeseries = TSClient.getTimeSeries(dataIRIs)

    # Return time series and associated lists of variables and units
    return timeseries, dataIRIs, measurements, units


# ===============================================================================
# Functions to Structure Retrieved Data for DTVF

def geojson_initialise_dict():
    '''Return initialised geojson dictionary.'''
    # Start GeoJSON FeatureCollection
    geojson = {'type': 'FeatureCollection',
               'features': []
               }
    return geojson


def geojson_add_sensor(feature_id, properties, coordinates):
    '''Return geojson feature representing a sensor.'''
    # Define new GeoJSON feature
    feature = {'type': 'Feature',
               'id': int(feature_id),
               'properties': properties.copy(),
               'geometry': {'type': 'Point',
                            'coordinates': coordinates
                            }
               }
    return feature


def json_add_metadata(feature_id, lon, lat, desc):
    '''Return metadata dictionary representing a sensor.'''
    metadata = { 'id': feature_id,
                 'Longitude': lon,
                 'Latitude': lat,
                 'Measurements description': desc
                 }
    return metadata

def run_code():
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
    geojson = geojson_initialise_dict()
    metadata = []
    ts_data = { 'ts': [],
                'id': [],
                'units': [],
                'headers': []
                }
    feature_id = 0

    # Get sensors of interest
    sensors = get_all_sensors(KGClient)
    #sensors = get_sensors_in_circle(center, radius)

    # Loop over all sensors
    for sensor in sensors:
        feature_id += 1

        # 1) Retrieve data for GeoJSON output
        coords, name = get_geojson_data(sensor, KGClient)

        # Update GeoJSON properties
        geojson_props['description'] = str(sensor)
        geojson_props['displayName'] = name
        # Append results to overall GeoJSON FeatureCollection
        geojson['features'].append(geojson_add_sensor(feature_id, geojson_props, coords))

        # 2) Retrieve data for metadata output
        lon, lat, desc = get_metadata(sensor, KGClient)
        metadata.append(json_add_metadata(feature_id, lon, lat, desc))

        # 3) Retrieve time series data
        timeseries, dataIRIs, measurement, units = get_all_time_series(sensor, KGClient, TSClient)
        ts_data['ts'].append(timeseries)
        ts_data['id'].append(feature_id)
        # Create dictionaries for units and headers (Python equivalent for Java HashMaps)
        ts_data['units'].append(dict(zip(dataIRIs, units)))
        ts_data['headers'].append(dict(zip(dataIRIs, measurement)))

    # Retrieve all time series data for collected 'ts_data' from Java TimeSeriesClient at once
    ts_json = TSClient.convertToJSON(ts_data['ts'], ts_data['id'], ts_data['units'], ts_data['headers'])
    # Make JSON file readable in Python
    ts_json = json.loads(ts_json.toString())

    i = 0
    while i < len(ts_json):
        powers = solar_power(ts_json[i]['values'][0])
        ts_json[i]['values'].append(powers)
        ts_json[i]['data'].append('Solar panel power potential')
        ts_json[i]['units'].append('kW/m2')
        i+=1

    # Write GeoJSON dictionary formatted to file
    file_name = os.path.join(utils.OUTPUT_DIR, 'sensors', 'NewcastleSensorsSolar.geojson')
    with open(file_name, 'w') as f:
        json.dump(geojson, indent=4, fp=f)
    file_name = os.path.join(utils.OUTPUT_DIR, 'sensors', 'NewcastleSensorsSolar-meta.json')
    with open(file_name, 'w') as f:
        json.dump(metadata, indent=4, fp=f)
    file_name = os.path.join(utils.OUTPUT_DIR, 'sensors', 'NewcastleSensorsSolar-timeseries.json')
    with open(file_name, 'w') as f:
        json.dump(ts_json, indent=4, fp=f)

# ===============================================================================
# Retrieve MIDAS Data from KG and Store as Files for DTVF

if __name__ == '__main__':
    run_code()




