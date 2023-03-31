###############################################
# Authors: Toby Latcham (tjl47@cam.ac.uk)     #
#          Sophie Hall (sh2000@cam.ac.uk)     #
#          Markus Hofmeister (mh807cam.ac.uk) #    
# Date: 11 Feb 2022                           #
###############################################


import json
import uuid
import pandas as pd
import pgeocode

from Utils.data_reader_module import *
# Get the JVM module view (via jpsBaseLibGateWay instance) from the jpsSingletons module to access
# the TimeSeriesClient in the JPB_BASE_LIB
from Utils.jpsSingletons import jpsBaseLibView
# Get settings and functions from utils module
import Utils.utils as utils
# Power calculation module
from Utils.power_module import *


# ===============================================================================

# Function extracts desired data from relevant csv files
def extract_data():
    postcode_data = pd.read_csv('./Data/energy_data/AnnualPostcodeElectricity.csv')
    postcode_coordinates = pd.read_csv('./Data/energy_data/NSPL_FEB_2019_UK.csv')
    #postcode_coordinates0 = []
    #for postcode in postcode_coordinates['pcds']:
        #postcode_coordinates0.append(postcode)

    # extracting a list of unique sensor IDs
    postcodes = postcode_data['POSTCODE'][10000:10100]

    # producing a dictionary of sensors and their corresponding data
    energy_data = {}
    for postcode in postcodes:
        data_rows = get_df_selected_rows(postcode_data, 'POSTCODE', postcode)
        #print(postcode)
        #if postcode in postcode_coordinates0:
        coordinate_rows = get_df_selected_rows(postcode_coordinates, 'pcds', postcode)
        if coordinate_rows.empty:
            location = pgeocode.Nominatim('gb')
            location = location.query_postal_code(str(postcode))
            lat = str(location.iloc[9])
            lon = str(location.iloc[10])
            print('nay')
        else:
            lat = str(coordinate_rows['lat'].iloc[0])
            lon = str(coordinate_rows['long'].iloc[0])
            

        meters = float(get_df_column_data(data_rows, 'Number of meters').iloc[0])
        annual_energy = get_df_column_data(data_rows, 'Consumption (kWh)').iloc[0]
        mean_hhc = get_df_column_data(data_rows, 'Mean consumption (kWh)').iloc[0]
        median_hhc = get_df_column_data(data_rows, 'Median consumption (kWh)').iloc[0]

        # estimating hourly usage for each month

        tstamps = [ '2021-01-01T00:00:00Z', '2021-01-01T01:00:00Z', '2021-01-01T02:00:00Z', '2021-01-01T03:00:00Z', '2021-01-01T04:00:00Z', 
                    '2021-01-01T05:00:00Z', '2021-01-01T06:00:00Z', '2021-01-01T07:00:00Z', '2021-01-01T08:00:00Z', 
                    '2021-01-01T09:00:00Z', '2021-01-01T10:00:00Z', '2021-01-01T11:00:00Z', '2021-01-01T12:00:00Z', 
                    '2021-01-01T13:00:00Z', '2021-01-01T14:00:00Z', '2021-01-01T15:00:00Z', '2021-01-01T16:00:00Z', 
                    '2021-01-01T17:00:00Z', '2021-01-01T18:00:00Z', '2021-01-01T19:00:00Z', '2021-01-01T20:00:00Z', 
                    '2021-01-01T21:00:00Z', '2021-01-01T22:00:00Z', '2021-01-01T23:00:00Z' ]
        # mh807: potentially easier
        #tstamps = pd.date_range('2021-01-01T00:00:00Z', '2021-01-01T23:59:59Z', freq='h')
        #print(tstamps)
        #tstamps = t.strftime('%H:%M:%SZ').tolist()

        hourly_data = pd.read_csv('./Data/energy_data/2019_energy_usage.csv')
        hourly_data.columns
        january = []
        for usage in hourly_data['january']:
            january.append(float(usage)*annual_energy/122.101)
        february = []
        for usage in hourly_data['february']:
            february.append(float(usage)*annual_energy/122.101)
        march = []
        for usage in hourly_data['march']:
            march.append(float(usage)*annual_energy/122.101)
        april = []
        for usage in hourly_data['april']:
            april.append(float(usage)*annual_energy/122.101)
        may = []
        for usage in hourly_data['may']:
            may.append(float(usage)*annual_energy/122.101)
        june = []
        for usage in hourly_data['june']:
            june.append(float(usage)*annual_energy/122.101)
        july = []
        for usage in hourly_data['july']:
            july.append(float(usage)*annual_energy/122.101)
        august = []
        for usage in hourly_data['august']:
            august.append(float(usage)*annual_energy/122.101)
        september = []
        for usage in hourly_data['september']:
            september.append(float(usage)*annual_energy/122.101)
        october = []
        for usage in hourly_data['october']:
            october.append(float(usage)*annual_energy/122.101)
        november = []
        for usage in hourly_data['november']:
            november.append(float(usage)*annual_energy/122.101)
        december = []
        for usage in hourly_data['december']:
            december.append(float(usage)*annual_energy/122.101)


        energy_data[postcode] = [meters, annual_energy, mean_hhc, median_hhc, lon, lat, tstamps, january, february, 
                                 march, april, may, june, july, august, september, october, november, december]
    return energy_data


# Function puts data into correct desired format
def format_data(sensor_data):
    sensor_instances = []
    n = 0
    for sensor in sensor_data.keys():
        sensor_dict = {}
        sensor_dict['postcode'] = sensor
        sensor_dict['lat'] = sensor_data[sensor][5]
        sensor_dict['lon'] = sensor_data[sensor][4]
        sensor_dict['meters'] = sensor_data[sensor][0]
        sensor_dict['annual_energy'] = sensor_data[sensor][1]
        sensor_dict['mean_hhc'] = sensor_data[sensor][2]
        sensor_dict['median_hhc'] = sensor_data[sensor][3]
        sensor_dict['times'] = sensor_data[sensor][6]
        sensor_dict['timeseries'] = {'January' : sensor_data[sensor][7], 'February' : sensor_data[sensor][8], 'March' : sensor_data[sensor][9],
                                     'April' : sensor_data[sensor][10], 'May' : sensor_data[sensor][11], 'June' : sensor_data[sensor][12],
                                     'July' : sensor_data[sensor][13], 'August' : sensor_data[sensor][14], 'September' : sensor_data[sensor][15],
                                     'October' : sensor_data[sensor][16], 'November' : sensor_data[sensor][17], 'December' : sensor_data[sensor][18], 'NearestWindPower' : []}
        sensor_instances.append(sensor_dict)
        if n == 0:
            print(sensor_dict)
        n+=1
    return sensor_instances


# Function to instantiate annual postcode statistics
def instantiate_annual_statistics(KGClient, data, postcode):
    """
    Instantiates postcode with annual electricity consumption statistics

    :param KGClient - KG client Java object (allows KG access via py4jps)
    :param dict data - Dictionary of postcode data to be instantiated
    :param str postcode - String of postcode IRI

    :returns None
    """
    # Create query
    query = utils.create_sparql_prefix('ex') + \
            utils.create_sparql_prefix('rdf') + \
            utils.create_sparql_prefix('rdfs') + \
            utils.create_sparql_prefix('geolit') + \
            '''INSERT DATA { \
                <%s> rdf:type ex:Postcode ; \
                rdfs:label "%s" ; \
                ex:hasMeters "%s" ; \
                ex:hasAnnualusage "%s" ; \
                ex:hasMeanhouseholdusage "%s" ; \
                ex:hasMedianhouseholdusage "%s" ; \
                ex:hasLocation "%s . }''' % (postcode, data['postcode'], data['meters'], data['annual_energy'], 
                                             data['mean_hhc'], data['median_hhc'], 
                                             data['lat'] + '#' + data['lon'] + '\"^^geolit:lat-lon')
    # Execute query
    KGClient.executeUpdate(query)


# Function to instantiate monthly consumption pattern
def instantiate_consumption_pattern(KGClient, postcode, timeseries, consumption, units):
    """
    Instantiates monthly electricity consumption pattern for a postcode

    :param KGClient - KG client Java object (allows KG access via py4jps)
    :param str postcode - Post code IRI to instantiate consumption for
    :param list timeseries - List of timeseries keys to instantiate
    :param dict consumption - Dictionary with consumption IRIs for timeseries keys
    :param dict units - Dictionary with consumption units for timeseries keys

    :returns None
    """

    # Open query 
    query = utils.create_sparql_prefix('ex') + \
            utils.create_sparql_prefix('rdf') + \
            utils.create_sparql_prefix('rdfs') + \
            'INSERT DATA {'
    # Create list of "subqueries" to add all available consumption timeseries
    subquery = ['''<%s> ex:measures <%s> . \
                   <%s> rdf:type ex:measurements ; \
                        rdfs:label "%s" ; \
                        ex:unit "%s" .''' % (postcode, consumption[ts], consumption[ts],  ts +' measurements', units[ts])
                        for ts in timeseries]
    # Append subqueries
    query += ''.join(map(str, subquery))

    # Close query
    query += '}'

    # Execute query
    KGClient.executeUpdate(query)



###==============QUERYING============================================================



def get_sensors_in_circle(center, radius, KGClient):
    '''
        Returns all instantiated sensors within a radius of 'radius' km from 'center' as list
    '''

    # Define query
    query = utils.create_sparql_prefix('ontoweather') + \
            utils.create_sparql_prefix('rdf') + \
            utils.create_sparql_prefix('geo') + \
            utils.create_sparql_prefix('geolit') + \
            '''SELECT ?cons \
               WHERE { \
                 SERVICE geo:search \
                 { \
                    ?cons geo:search "inCircle" . \
                    ?cons geo:searchDatatype geolit:lat-lon . \
                    ?cons geo:predicate ontoweather:WGS84LatitudeLongitude . \
                    ?cons geo:spatialCircleCenter "%s" . \
                    ?cons geo:spatialCircleRadius "%s" . \
                 } \
                 ?cons rdf:type ontoweather:WindSensor \
               }''' % (str(center), str(radius))

    # Execute query
    response = KGClient.execute(query)

    # Convert JSONArray String back to list
    response = json.loads(response)
    # Unpack all sensors to list
    sensors = [r['cons'] for r in response]

    return sensors



def get_metadata(sensor, KGClient):
    '''
        Returns meta data for given 'sensor'
    '''

    # Define query
    query = utils.create_sparql_prefix('ontoweather') + \
            utils.create_sparql_prefix('rdfs') + \
            '''SELECT ?loc ?measurements \
               WHERE { <%s> ontoweather:WGS84LatitudeLongitude ?loc ; \
                            ontoweather:observesWindProperty ?dataIRI . \
                       ?dataIRI ontoweather:hasTimeseries ?measurements }''' % sensor
    # Execute query
    response = KGClient.execute(query)

    # Convert JSONArray String back to list
    response = json.loads(response)

    # Unpack sensor location
    coordinates = response[0]['loc'].split('#')
    lon = coordinates[1]
    lat = coordinates[0]

    return lon, lat

def get_all_time_series(sensor, KGClient, TSClient):
    '''
        Returns data for all time series associated with given 'sensor'
    '''

    # Define query
    query = utils.create_sparql_prefix('ontoweather') + \
            utils.create_sparql_prefix('rdfs') + \
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
    utilities = []
    units = []
    # Append lists with all query results
    for r in response:
        dataIRIs.append(r['dataIRI'])
        utilities.append(r['measurements'])
        units.append((r['unit']))
    print(dataIRIs)

    # Retrieve time series data for retrieved set of dataIRIs
    timeseries = TSClient.getTimeSeries(dataIRIs)

    # Return time series and associated lists of variables and units
    return timeseries, dataIRIs, utilities, units

def nearest_wind_data(postcode, KGClient, TSClient):
    print(postcode)
    p_lat = str(postcode['lat'])
    p_lon = str(postcode['lon'])
    postcode_coords = p_lat + '#' + p_lon
    sensors = get_sensors_in_circle(postcode_coords, 240, KGClient)
    min_distance = 10000000
    close_sensors = []
    for sensor in sensors:
        lon, lat = get_metadata(sensor, KGClient)
        distance = ((float(p_lat)-float(lat))**2 + (float(p_lon)-float(lon))**2)**0.5
        if distance < min_distance:
            min_distance = distance
            close_sensors.append(sensor)
    closest_sensor = close_sensors[-1]
    timeseries, dataIRIs, measurement, units = get_all_time_series(closest_sensor, KGClient, TSClient)

    # Retrieve all time series data for collected 'ts_data' from Java TimeSeriesClient at once
    #print([timeseries], [1], [dict(zip(dataIRIs, units))], [dict(zip(dataIRIs, measurement))])
    ts_json = TSClient.convertToJSON([timeseries], [1], [dict(zip(dataIRIs, units))], [dict(zip(dataIRIs, measurement))])
    # Make JSON file readable in Python
    ts_json = json.loads(ts_json.toString())
    print(ts_json)

    # Sorting data
    #if len(ts_json[0]['values'][0]) == 0:
        #return [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,]
    p = 0
    while p <= 4:
        if ts_json[0]['values'][p][0] == 1:
            windspeed = ts_json[0]['values'][p]
        elif ts_json[0]['values'][p][0] == 3:
            gustspeed = ts_json[0]['values'][p]
        p += 1
    ts_json[0]['time'] = ts_json[0]['time'][1:]

    v_op, k = optimal_windspeed(windspeed[1:])
    turbine = optimal_turbine(v_op, gustspeed[1:])
    powers = turbine_power(turbine, windspeed[1:], k)
    powers = powers[:-1]
    print(windspeed)
    print(v_op)
    print(powers)
    print('------------------------')

    return powers


def instantiate_energy_data():
    # Create specified PostgreSQL database
    utils.create_postgres_db()
    # Create specified Blazegraph namespace
    utils.create_blazegraph_namespace()

    ###======================QUERYING===============================================
    # Initialise remote KG client with only query endpoint specified
    #KGClient = jpsBaseLibView.RemoteStoreClient(utils.QUERY_ENDPOINT)
    KGClient = jpsBaseLibView.RemoteStoreClient(utils.QUERY_ENDPOINT, utils.UPDATE_ENDPOINT)

    # Retrieve Java's Instant class to initialise TimeSeriesClient
    offsetTime = jpsBaseLibView.java.time.Instant # OffsetTime
    offsetTime_class = offsetTime.now().getClass()
    # Initialise TimeSeriesClass
    TSClient = jpsBaseLibView.TimeSeriesClient(offsetTime_class, utils.PROPERTIES_FILE)
    ###=============================================================================

    postcode_data = extract_data()
    postcode_instances = format_data(postcode_data)
    units = {'January': 'kWh', 'February': 'kWh', 'March': 'kWh', 'April': 'kWh', 'May': 'kWh', 'June': 'kWh', 
             'July': 'kWh', 'August': 'kWh', 'September': 'kWh', 'October': 'kWh', 'November': 'kWh', 'December': 'kWh', 'NearestWindPower': 'MW'}

    # Initialise remote KG client with query AND update endpoints specified
    #KGClient = jpsBaseLibView.RemoteStoreClient(utils.QUERY_ENDPOINT, utils.UPDATE_ENDPOINT)

    # Retrieve Java classes for time entries (Instant) and data (ALL Double)
    # (required for time series client instantiation)
    double_class = jpsBaseLibView.java.lang.Double.TYPE

    # Loop over all postcodes
    for postcode in postcode_instances:
        print('Current postcode: ', postcode['postcode'])

        w_powers = nearest_wind_data(postcode, KGClient, TSClient)
        postcode['timeseries']['NearestWindPower'] = w_powers

        # Create IRI for current postcode
        postcodeIRI = utils.PREFIXES['ex'] + 'Postcode_' + str(uuid.uuid4())

        # Instantiate postcode and annual electricity consumption statistics
        instantiate_annual_statistics(KGClient, postcode, postcodeIRI)
        print("Postcode triples and annual statistics successfully instantiated.")

        # Instantiate monthly consumption pattern
        months = list(postcode['timeseries'].keys())
        dataIRIs = [utils.PREFIXES['ex'] + ts + '_' + str(uuid.uuid4()) for ts in months]
        data = dict(zip(months, dataIRIs))
        instantiate_consumption_pattern(KGClient, postcodeIRI, months, data, units)
        print("Triples independent of Java TimeSeriesClient successfully instantiated.")
            

        # Perform SPARQL update for time series related triples (i.e. via TimeSeriesClient)
        # Initialise time series in both KG and RDB using TimeSeriesClass
        TSClient.initTimeSeries(dataIRIs, [double_class]*len(dataIRIs), utils.FORMAT)
        print("Time series triples via Java TimeSeriesClient successfully instantiated.")

        # Add actual time series data: Create Java TimeSeries object with data to attach
        times = postcode['times']
        values = [postcode['timeseries'][v] for v in list(postcode['timeseries'].keys())]
        timeseries = jpsBaseLibView.TimeSeries(times, dataIRIs, values)
        # Add data
        TSClient.addTimeSeriesData(timeseries)

        print("Time series data successfully added.\n")

    print("All data successfully instantiated")

# ===============================================================================
# Instantiate postcode Data

if __name__ == '__main__':
    instantiate_energy_data()