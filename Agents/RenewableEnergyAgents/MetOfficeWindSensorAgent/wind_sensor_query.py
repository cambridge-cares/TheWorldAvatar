###########################################
# Authors: Toby Latcham (tjl47@cam.ac.uk) #
#          Sophie Hall (sh2000@cam.ac.uk) #
# Date: 11 Feb 2022                       #
###########################################




# ===============================================================================

import os.path
import json

# Get the JVM module view (via jpsBaseLibGateWay instance) from the jpsSingletons
# module to access the TimeSeriesClient in the JPB_BASE_LIB
from Utils.jpsSingletons import jpsBaseLibView
# Get settings and functions from utils module
import Utils.utils as utils
# Power calculation module
from Utils.power_module import averaging_data, optimal_windspeed, optimal_turbine, turbine_power
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
               WHERE { ?sens rdf:type ontoweather:WindSensor }'''
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
                 ?sens rdf:type ontoweather:WindSensor \
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
                            ontoweather:observesWindProperty ?dataIRI. \
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

    speed_desc = response[0]['desc']
    direction_desc = response[1]['desc']
    gust_desc = response[2]['desc']

    return lon, lat, speed_desc, direction_desc, gust_desc


def get_all_time_series(sensor, KGClient, TSClient):
    '''Returns data for all time series associated with given 'sensor'.'''

    # Define query
    query = utils.create_sparql_prefix('ontoweather') + \
            utils.create_sparql_prefix('om') + \
            '''SELECT ?dataIRI ?measurements ?unit \
               WHERE { <%s> ontoweather:observesWindProperty ?dataIRI. \
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
    #print(dataIRIs)
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


def json_add_metadata(feature_id, lon, lat, power_per_area, speed_desc, direction_desc, gust_desc):
    '''Return metadata dictionary representing a sensor.'''
    metadata = { 'id': feature_id,
                 'Longitude': lon,
                 'Latitude': lat,
                 'Wind power potential (W/m2)': power_per_area,
                 'Wind speed measurements description': speed_desc,
                 'Wind direction measurements description': direction_desc,
                 'Wind gust measurements description': gust_desc
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

        # 2) Retrieve time series data
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

        # Sort timeseries data
        p = 0
        i = feature_id - 1
        while p <= 4:
            if ts_json[i]['values'][p][0] == 1:
                windspeeds = ts_json[i]['values'][p]
            elif ts_json[i]['values'][p][0] == 3:
                gustspeeds = ts_json[i]['values'][p]
            p += 1
        timestamps = ts_json[i]['time'][1:]
        # Average the wind data by hour
        average_windspeeds, max_windspeed = averaging_data(timestamps, windspeeds[1:])
        average_gustspeeds, max_gustspeed = averaging_data(timestamps, gustspeeds[1:])

        # Acquire wind power timeseries and wind power per unit area value for sensor location
        v_op, k = optimal_windspeed(average_windspeeds)
        turbine = optimal_turbine(v_op, [max_gustspeed])
        powers = turbine_power(turbine, average_windspeeds, k)
        power_per_area = round(powers[-1], 4)*(10**3) # Rounding and converting to W/m2
        #print(power_per_area)

        # identifying good locations to build wind turbines
        if power_per_area > 20:
            geojson_props['circle-color'] = '#00FF00'
        elif power_per_area > 15:
            geojson_props['circle-color'] = '#fceb03'
        elif power_per_area > 10:
            geojson_props['circle-color'] = '#fc6b03'
        else:
            geojson_props['circle-color'] = '#ff0000'
        # Append geojson properties to overall GeoJSON FeatureCollection
        geojson['features'].append(geojson_add_sensor(feature_id, geojson_props, coords))

        # 3) Retrieve data for metadata output
        lon, lat, speed_desc, direction_desc, gust_desc = get_metadata(sensor, KGClient)
        metadata.append(json_add_metadata(feature_id, lon, lat, power_per_area,
                                          speed_desc, direction_desc, gust_desc))

    # Retrieve all time series data for collected 'ts_data' from Java TimeSeriesClient at once
    ts_json = TSClient.convertToJSON(ts_data['ts'], ts_data['id'],
                                     ts_data['units'], ts_data['headers'])
    # Make JSON file readable in Python
    ts_json = json.loads(ts_json.toString())
    
    # Sorting timeseries data
    i = 0
    while i < len(ts_json):
        #print(ts_json[i])
        p = 0
        while p <= 4:
            if ts_json[i]['values'][p][0] == 1:
                windspeed = ts_json[i]['values'][p]
                #print(windspeed)
            #elif ts_json[i]['values'][p][0] == 2:
            #    winddirection = ts_json[i]['values'][p]
            elif ts_json[i]['values'][p][0] == 3:
                gustspeed = ts_json[i]['values'][p]
                #print(gustspeed)
            #elif ts_json[i]['values'][p][0] == 4:
            #    gustdirection = ts_json[i]['values'][p]
            #elif ts_json[i]['values'][p][0] == 5:
            #    gusttime = ts_json[i]['values'][p]
            p += 1
        ts_json[i]['time'] = ts_json[i]['time'][1:]
        ts_json[i]['values'][0] = ts_json[i]['values'][0][1:]
        ts_json[i]['values'][1] = ts_json[i]['values'][1][1:]
        ts_json[i]['values'][2] = ts_json[i]['values'][2][1:]
        ts_json[i]['values'][3] = ts_json[i]['values'][3][1:]
        ts_json[i]['values'][4] = ts_json[i]['values'][4][1:]

        # Acquire wind power timeseries
        v_op, k = optimal_windspeed(windspeed[1:])
        turbine = optimal_turbine(v_op, gustspeed[1:])
        powers = turbine_power(turbine, windspeed[1:], k)
        powers = powers[:-1]

        # Append power per unit area timeseries
        ts_json[i]['values'].append(powers)
        ts_json[i]['data'].append('Wind turbine power potential')
        ts_json[i]['units'].append('kW')

        i += 1

    # Write GeoJSON dictionary formatted to file
    file_name = os.path.join(utils.OUTPUT_DIR, 'sensors', 'MIDASSensors.geojson')
    with open(file_name, 'w') as f:
        json.dump(geojson, indent=4, fp=f)
    file_name = os.path.join(utils.OUTPUT_DIR, 'sensors', 'MIDASSensors-meta.json')
    with open(file_name, 'w') as f:
        json.dump(metadata, indent=4, fp=f)
    file_name = os.path.join(utils.OUTPUT_DIR, 'sensors', 'MIDASSensors-timeseries.json')
    with open(file_name, 'w') as f:
        json.dump(ts_json, indent=4, fp=f)

# ===============================================================================
# Retrieve MIDAS Data from KG and Store as Files for DTVF

if __name__ == '__main__':
    run_code()




