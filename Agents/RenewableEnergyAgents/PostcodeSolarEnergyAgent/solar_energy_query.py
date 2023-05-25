################################################
# Authors: Toby Latcham (tjl47@cam.ac.uk)      #
#          Sophie Hall (sh2000@cam.ac.uk)      #
#          Markus Hofmeister (mh807@cam.ac.uk) #
# Date: 15 Feb 2022                            #
################################################

"""Retrieves Solar Energy Data consumed at each postcode in the UK from KG and
Stores as JSON Files for DTVF
"""

import os.path
import json

# Get the JVM module view (via jpsBaseLibGateWay instance) from the jpsSingletons module to access
# the TimeSeriesClient in the JPB_BASE_LIB
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

def get_all_postcodes(KGClient):
    '''
        Returns all postcodes instantiated in KG as list
    '''

    # Define query
    query = utils.create_sparql_prefix('ex') + \
            utils.create_sparql_prefix('rdf') + \
            '''SELECT ?cons \
               WHERE { ?cons rdf:type ex:Postcode }'''
    # Execute query
    response = KGClient.execute(query)

    # Convert JSONArray String back to list
    response = json.loads(response)
    # Unpack all sensors to list
    postcodes = [r['cons'] for r in response]

    return postcodes


def get_sensors_in_circle(center, radius, KGClient):
    '''
        Returns all instantiated sensors within a radius of 'radius' km from 'center' as list
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
                 ?cons rdf:type ex:Sensor \
               }''' % (str(center), str(radius))

    # Execute query
    response = KGClient.execute(query)

    # Convert JSONArray String back to list
    response = json.loads(response)
    # Unpack all sensors to list
    sensors = [r['cons'] for r in response]

    return sensors


def get_geojson_data(postcode, KGClient):
    '''
        Returns coordinates ([lon, lat]) and name (label) for given 'postcode'
    '''

    # Define query
    query = utils.create_sparql_prefix('ex') + \
            utils.create_sparql_prefix('rdfs') + \
            '''SELECT ?loc ?name \
               WHERE { <%s> ex:hasLocation ?loc ; \
                            rdfs:label ?name }''' % postcode
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


def get_metadata(postcode, KGClient):
    '''
        Returns meta data for given 'postcode'
    '''

    # Define query
    query = utils.create_sparql_prefix('ex') + \
            utils.create_sparql_prefix('rdfs') + \
            '''SELECT ?loc ?meters ?annualusage ?meanusage ?medianusage \
               WHERE { <%s> ex:hasLocation ?loc ; \
                            ex:hasMeters ?meters ; \
                            ex:hasAnnualusage ?annualusage ; \
                            ex:hasMeanhouseholdusage ?meanusage ; \
                            ex:hasMedianhouseholdusage ?medianusage }''' % postcode
    # Execute query
    response = KGClient.execute(query)

    # Convert JSONArray String back to list
    response = json.loads(response)

    # Unpack sensor location
    coordinates = response[0]['loc'].split('#')
    lon = coordinates[1]
    lat = coordinates[0]

    meters = int(float(response[0]['meters']))
    annual_energy = response[0]['annualusage']
    mean_hhc = response[0]['meanusage']
    median_hhc = response[0]['medianusage']

    return lon, lat, meters, annual_energy, mean_hhc, median_hhc


def get_all_time_series(postcode, KGClient, TSClient):
    '''
        Returns data for all time series associated with given 'sensor'
    '''

    # Define query
    query = utils.create_sparql_prefix('ex') + \
            utils.create_sparql_prefix('rdfs') + \
            '''SELECT ?dataIRI ?measurements ?unit \
               WHERE { <%s> ex:measures ?dataIRI. \
                       ?dataIRI rdfs:label ?measurements ;
                                ex:unit ?unit }''' % postcode
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
        utilities.append(r['measurements'])
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


def geojson_add_postcode(feature_id, properties, coordinates):
    # Define new GeoJSON feature
    feature = {'type': 'Feature',
               'id': int(feature_id),
               'properties': properties.copy(),
               'geometry': {'type': 'Point',
                            'coordinates': coordinates
                            }
               }
    return feature


def json_add_metadata(feature_id, lon, lat, meters, annual_energy, mean_hhc, median_hhc):
    # Define metadata dictionary
    metadata = { 'id': feature_id,
                 'Longitude': lon,
                 'Latitude': lat,
                 'Number of meters': meters,
                 'Annual electricity consumption (kWh)': annual_energy,
                 'Mean annual household consumption (kWh)': mean_hhc,
                 'Median annual household consumption (kWh)': median_hhc
                 }
    return metadata

def query_solar_energy():

    # Set Mapbox API key in DTVF 'index.html' file
    utils.set_mapbox_apikey()

    # Initialise remote KG client with only query endpoint specified
    KGClient = jpsBaseLibView.RemoteStoreClient(utils.QUERY_ENDPOINT)

    # Retrieve Java's Instant class to initialise TimeSeriesClient
    offsetTime = jpsBaseLibView.java.time.Instant #OffsetTime
    offsetTime_class = offsetTime.now().getClass()
    # Initialise TimeSeriesClass
    TSClient = jpsBaseLibView.TimeSeriesClient(offsetTime_class, utils.PROPERTIES_FILE)

    # Initialise output files/dictionaries
    geojson = geojson_initialise_dict()
    metadata = []
    ts_data = { 'ts': [],
                'id': [],
                'units': [],
                'headers': []
                }
    feature_id = 0

    # Get postcodes of interest
    postcodes = get_all_postcodes(KGClient)
    #sensors = get_sensors_in_circle(center, radius, KGClient)

    # Loop over all sensors
    for postcode in postcodes[:100]:
        feature_id += 1

        # 1) Retrieve data for GeoJSON output
        coords, name = get_geojson_data(postcode, KGClient)

        # Update GeoJSON properties
        geojson_props['description'] = str(postcode)
        geojson_props['displayName'] = name
        # Append results to overall GeoJSON FeatureCollection
        geojson['features'].append(geojson_add_postcode(feature_id, geojson_props, coords))

        # Retrieve data for metadata output
        lon, lat, meters, annual_energy, mean_hhc, median_hhc = get_metadata(postcode, KGClient)
        metadata.append(json_add_metadata(feature_id, lon, lat, meters, annual_energy, mean_hhc, median_hhc))

        # 3) Retrieve time series data
        timeseries, dataIRIs, measurement, units = get_all_time_series(postcode, KGClient, TSClient)
        ts_data['ts'].append(timeseries)
        ts_data['id'].append(feature_id)
        # Create dictionaries for units and headers (Python equivalent for Java HashMaps)
        ts_data['units'].append(dict(zip(dataIRIs, units)))
        ts_data['headers'].append(dict(zip(dataIRIs, measurement)))
        #print(ts_data)

    # Retrieve all time series data for collected 'ts_data' from Java TimeSeriesClient at once
    ts_json = TSClient.convertToJSON(ts_data['ts'], ts_data['id'], ts_data['units'], ts_data['headers'])
    # Make JSON file readable in Python
    ts_json = json.loads(ts_json.toString())

    # Write GeoJSON dictionary formatted to file
    file_name = os.path.join(utils.OUTPUT_DIR, 'sensors', 'PostcodeEnergy.geojson')
    with open(file_name, 'w') as f:
        json.dump(geojson, indent=4, fp=f)
    file_name = os.path.join(utils.OUTPUT_DIR, 'sensors', 'PostcodeEnergy-meta.json')
    with open(file_name, 'w') as f:
        json.dump(metadata, indent=4, fp=f)
    file_name = os.path.join(utils.OUTPUT_DIR, 'sensors', 'PostcodeEnergy-timeseries.json')
    with open(file_name, 'w') as f:
        json.dump(ts_json, indent=4, fp=f)

if __name__ == '__main__':
    query_solar_energy()
