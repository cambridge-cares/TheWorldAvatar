###########################################
# Authors: Toby Latcham (tjl47@cam.ac.uk) #
#          Sophie Hall (sh2000@cam.ac.uk) #
# Date: 11 Feb 2022                       #
###########################################




# ===============================================================================

import uuid
import pandas as pd

from Utils.data_reader_module import get_df_selected_rows, get_df_column_data
# Get the JVM module view (via jpsBaseLibGateWay instance) from the jpsSingletons module to access
# the TimeSeriesClient in the JPB_BASE_LIB
from Utils.jpsSingletons import jpsBaseLibView
# Get settings and functions from utils module
import Utils.utils as utils
# Import sensor coordinates download function
from Utils.met_office_wind_sensor_coordinates import get_sensor_coords

# ===============================================================================

property_mean_wind_speed = 'MeanWindSpeed'
property_mean_wind_direction = 'MeanWindDirection'
property_max_gust_speed = 'MaxWindGustSpeed'
property_max_gust_direction = 'MaxWindGustDirection'
property_max_gust_time = 'MaxWindGustTime'

properties = [property_mean_wind_speed, property_mean_wind_direction,
              property_max_gust_speed, property_max_gust_direction,
              property_max_gust_time]

data_path = '' ### fix later


def get_coords():
    '''Returns a dictionary of sensor coordinates from the
    file downloaded by the get_sensor_coords function.
    '''
    station_loc = pd.read_csv('./Data/MIDAS_wind_data/stationlocations2.csv')
    lat_list = station_loc['Latitude']
    lon_list = station_loc['Longitude']
    id_list = station_loc['src_id']

    coordinates = {}
    n = 0
    for sensor_id in id_list:
        coordinates[sensor_id] = [str(lat_list[n]), str(lon_list[n])]
        n += 1
    return coordinates

def merge_midas():
    '''Function merges MIDAS data file with the column headers file to
    create a new file (data downloaded using seperate script).
    '''
    # Convert txt file to csv file
    file = pd.read_csv (r'./Data/MIDAS_wind_data/midas_wind_202101-202112.txt')
    file.to_csv(r'./Data/MIDAS_wind_data/midas_wind_202101-202112.csv', index=None)

    # Load data
    headers = pd.read_csv('./Data/MIDAS_wind_data/WM_Column_Headers.csv')
    data = pd.read_csv('./Data/MIDAS_wind_data/midas_wind_202101-202112.csv', header=None)

    # Condition data
    data_extract = data.iloc[:, :20]
    headers_extract = [c.strip() for c in headers.columns[:20]]

    # Combine data with headers
    combined = data_extract.copy()
    combined.columns = headers_extract

    # Write data to combined file
    combined.to_csv('./Data/MIDAS_wind_data/MIDASwind.csv', index=False)

def extract_data(coordinates):
    '''Returns a dictionary containing all desired data for each sensor.

    Data extracted from the data file created by the merge_midas funtion,
    and the coordinates dictionary created by the get_coords function.
    '''
    wind_data = pd.read_csv('./Data/MIDAS_wind_data/MIDASwind.csv')

    # Extract a list of the desired unique sensor IDs
    sensor_ids = wind_data['SRC_ID']
    sensor_list = []
    for sensor_id in sensor_ids:
        if sensor_id not in sensor_list:
            sensor_list.append(sensor_id)

    # Produce a dictionary of sensors and their corresponding data
    sensor_data = {}
    for sensor in sensor_list:
        # Check to see if we have coordinates for this sensor, we can't visualise it if we don't
        if sensor in coordinates.keys():  
            data_rows = get_df_selected_rows(wind_data, 'SRC_ID', sensor)

            lat = str(coordinates[sensor][0])
            lon = str(coordinates[sensor][1])

            # Getting timeseries data
            # A dummy value is placed at the beginning of each timeseries so that the data can be sorted later
            tstamps_df = get_df_column_data(data_rows, 'OB_END_TIME')
            time_string = str(tstamps_df.iloc[0])
            tstamps = [time_string[0:10] + 'T' + time_string[11:19] + ':00Z']  # Timestamps (in correct format)
            MWSvalues_df = get_df_column_data(data_rows, 'MEAN_WIND_SPEED')
            MWSvalues = [1]  # Mean wind speed
            MWDvalues_df = get_df_column_data(data_rows, 'MEAN_WIND_DIR')
            MWDvalues = [2]  # Mean wind direction
            MGSvalues_df = get_df_column_data(data_rows, 'MAX_GUST_SPEED')
            MGSvalues = [3]  # Max gust speed
            MGDvalues_df = get_df_column_data(data_rows, 'MAX_GUST_DIR')
            MGDvalues = [4]  # Max gust direction
            MGTvalues_df = get_df_column_data(data_rows, 'MAX_GUST_CTIME')
            MGTvalues = [5]  # Max gust time

            n = 0
            for tstamp in tstamps_df:
                time_string = str(tstamp)
                if time_string[5:7] == '01':  # Make sure we only get data for the desired time period
                    tstamps.append(time_string[0:10] + 'T' +time_string[11:19] + ':00Z')  # Get timestamp into the correct format

                    # Test for valid data values, if valid the values are appended to the appropriate list.
                    # Zeroes are used for now for invalid values, but Null may be possible.
                    if str(MWSvalues_df.iloc[n]) == ' ':
                        MWSvalues.append(float(0))  
                    else:
                        MWSvalues.append(float(MWSvalues_df.iloc[n]))
                    if str(MWDvalues_df.iloc[n]) == ' ':
                        MWDvalues.append(float(0))
                    else:
                        MWDvalues.append(float(MWDvalues_df.iloc[n]))
                    if str(MGSvalues_df.iloc[n]) == ' ':
                        MGSvalues.append(float(0))
                    else:
                        MGSvalues.append(float(MGSvalues_df.iloc[n]))
                    if str(MGDvalues_df.iloc[n]) == ' ':
                        MGDvalues.append(float(0))
                    else:
                        MGDvalues.append(float(MGDvalues_df.iloc[n]))
                    if str(MGTvalues_df.iloc[n]) == ' ':
                        MGTvalues.append(float(0))
                    else:
                        MGTvalues.append(float(MGTvalues_df.iloc[n]))
                n += 1
            # Check for sensors that didn't have any measurements (these cause issues),
            # then add the sensor data to the sensor_data dictionary
            if len(tstamps) > 1:
                sensor_data[sensor] = [tstamps, MWSvalues, MWDvalues, MGSvalues,
                                       MGDvalues, MGTvalues, lon, lat]
            # Add virtual cambridge sensor, with sensor data copied from the sensor near Huntingdon
            if str(sensor) == '456':
                sensor_data['Virtual Cambridge Sensor'] = [tstamps, MWSvalues, MWDvalues, MGSvalues,
                                                           MGDvalues, MGTvalues, '0.1218', '52.2053']
    return sensor_data

def format_data(sensor_data):
    '''Returns a formatted dictionary of all sensor data.'''
    sensor_instances = []
    n = 0
    for sensor in sensor_data.keys():
        sensor_dict = {}
        sensor_dict['sensor name'] = sensor
        sensor_dict['lat'] = sensor_data[sensor][7]
        sensor_dict['lon'] = sensor_data[sensor][6]
        sensor_dict['times'] = sensor_data[sensor][0]
        sensor_dict['timeseries'] = {'Windspeed' : sensor_data[sensor][1],
                                     'WindDirection' : sensor_data[sensor][2],
                                     'MaxGustSpeed' : sensor_data[sensor][3],
                                     'MaxGustDirection' : sensor_data[sensor][4],
                                     'MaxGustTime' : sensor_data[sensor][5]}
        sensor_instances.append(sensor_dict)
        # Print off data for the first sensor to help with troubleshooting
        if n == 0:
            print(sensor_dict)
        n+=1
    return sensor_instances

def instantiate_weather_sensor(sensor, units, KGClient, instant_class, double_class):
    '''Instantiates a given sensor along with all its data'''
    print('Current sensor: ', sensor['sensor name'])

    speed_description = 'Wind speed is usually measured by a cup anemometer mounted on a vertical spindle. ' \
                        'The wind blowing into the cups causes the spindle to rotate. The cups are designed ' \
                        'such that the rate of rotation is proportional to the speed of the wind to a ' \
                        'sufficiently close approximation. Mean wind speed values are averages of the 60 ' \
                        'values measured per hour (one a minute, each rounded to the nearest knot). ' \
                        'The anemometers are attached to horizontal supporting arms at the top of 10 m masts.'
    direction_description = 'Wind direction is usually measured by a vane consisting of a thin horizontal arm ' \
                            'carrying a vertical flat plate at one end with its edge to the wind and at the ' \
                            'other end a balance weight which also serves as a pointer. The arm is carried on ' \
                            'a vertical spindle mounted on bearings which allow it to turn freely in the wind. ' \
                            'Mean wind direction values are averages of the 60 values measured per hour (one a ' \
                            'minute, each rounded to the nearest 10 degrees). The wind vanes are attached to ' \
                            'horizontal supporting arms at the top of 10 m masts.'
    gust_speed_description = 'Wind speed is sampled four times a second and wind gust is defined as the maximum ' \
                             'three second average wind speed of a given time period. The maximum gust is the gust with' \
                             'the highest speed during the hour. ' \
                             'Wind speed is usually measured by a cup anemometer mounted on a vertical spindle. ' \
                             'The wind blowing into the cups causes the spindle to rotate. The cups are designed ' \
                             'such that the rate of rotation is proportional to the speed of the wind to a ' \
                             'sufficiently close approximation. The anemometers are attached to horizontal ' \
                             'supporting arms at the top of 10 m masts.'
    gust_direction_description = 'Wind speed is sampled four times a second and wind gust is defined as the maximum ' \
                                 'three second average wind speed of a given time period. The maximum gust is the gust with' \
                                 'the highest speed during the hour. ' \
                                 'Wind direction is usually measured by a vane consisting of a thin horizontal arm ' \
                                 'carrying a vertical flat plate at one end with its edge to the wind and at the ' \
                                 'other end a balance weight which also serves as a pointer. The arm is carried on ' \
                                 'a vertical spindle mounted on bearings which allow it to turn freely in the wind. ' \
                                 'The wind vanes are attached to horizontal supporting arms at the top of 10 m masts.'
    gust_time_description = 'Wind speed is sampled four times a second and wind gust is defined as the maximum ' \
                            'three second average wind speed of a given time period. The maximum gust is the gust with' \
                            'the highest speed during the hour.' ### I don't actually know what exactly gust time is

    descriptions = [speed_description, direction_description, gust_speed_description,
                    gust_direction_description, gust_time_description]

    # Create IRI for current sensor
    sensorIRI = utils.PREFIXES['ssn'] + 'Sensor_' + str(uuid.uuid4())

    # Perform SPARQL update for non-time series related triples (i.e. without TimeSeriesClient)
    query = utils.create_sparql_prefix('ontoweather') + \
            utils.create_sparql_prefix('ssn') + \
            utils.create_sparql_prefix('rdf') + \
            utils.create_sparql_prefix('geolit') + \
            '''INSERT DATA { \
            <%s> rdf:type ontoweather:WindSensor ; \
                 ontoweather:hasName "%s" ; \
                 ontoweather:WGS84LatitudeLongitude "%s . }''' % ( sensorIRI,
                                                                   sensor['sensor name'],
                                                                   sensor['lat'] + '#' + sensor['lon'] + '\"^^geolit:lat-lon' )
    print(query)
    KGClient.executeUpdate(query)

    # Initialise list of dataIRIs, which will be represented as time series
    dataIRIs = []

    # Loop over all time series for this sensor
    k = 0
    for ts in list(sensor['timeseries'].keys()):
        dataIRI = instantiate_timeseries(ts, descriptions[k], units, sensorIRI, KGClient)
        dataIRIs.append(dataIRI)
        k += 1
        
    # Perform SPARQL update for time series related triples (i.e. via TimeSeriesClient)
    # Initialise time series in both KG and RDB using TimeSeriesClass
    TSClient = jpsBaseLibView.TimeSeriesClient(instant_class, utils.PROPERTIES_FILE)
    TSClient.initTimeSeries(dataIRIs, [double_class]*len(dataIRIs), utils.FORMAT)
    print('Time series triples via Java TimeSeriesClient successfully instantiated.')

    # Add actual time series data
    # Create Java TimeSeries object with data to attach
    times = sensor['times']
    values = [sensor['timeseries'][v] for v in list(sensor['timeseries'].keys())]
    timeseries = jpsBaseLibView.TimeSeries(times, dataIRIs, values)
    TSClient.addTimeSeriesData(timeseries)
    print('Time series data successfully added.\n')

def instantiate_timeseries(ts, description, units, sensorIRI, KGClient):
    '''Instantiates a given timeseries.'''
    # Create IRI for current time series and attach to dataIRI list
    dataIRI = utils.PREFIXES['ssn'] + ts + '_' + str(uuid.uuid4())
    
    # Perform SPARQL update for non-time series related triples (i.e. without TimeSeriesClient)
    query = utils.create_sparql_prefix('ontoweather') + \
            utils.create_sparql_prefix('om') + \
            utils.create_sparql_prefix('ssn') + \
            utils.create_sparql_prefix('rdf') + \
            '''INSERT DATA { \
            <%s> rdf:type ssn:Property ; \
                 ontoweather:hasTimeseries '%s' ; \
                 om:hasUnit '%s' ; \
                 ontoweather:hasDescription '%s' . \
            <%s> ontoweather:observesWindProperty <%s> . }''' % ( dataIRI,
                                                                  ts+' measurements',
                                                                  units[ts],
                                                                  description,
                                                                  sensorIRI, dataIRI )
    KGClient.executeUpdate(query)
    print('Triples independent of Java TimeSeriesClient successfully instantiated.')
    return dataIRI

def process_weather_sensor_data():
    '''Retrieves all desired data and returns a formatted dictionary
    of sensor data, along with the units for each measurement.
    '''
    get_sensor_coords()
    coordinates = get_coords()
    merge_midas()
    sensor_data = extract_data(coordinates)
    sensor_instances = format_data(sensor_data)
    units = {'Windspeed': 'knots', 'WindDirection': 'degrees',
             'MaxGustSpeed': 'knots', 'MaxGustDirection': 'degrees',
             'MaxGustTime': 'Seconds'}
    return sensor_instances, units

def instantiate_all_data(sensor_instances, units):
    '''Instantiates all solar data extracted from MIDAS website'''
    sensor_instances, units = process_weather_sensor_data()

    # Initialise remote KG client with query AND update endpoints specified
    KGClient = jpsBaseLibView.RemoteStoreClient(utils.QUERY_ENDPOINT, utils.UPDATE_ENDPOINT)

    # Retrieve Java classes for time entries (Instant) and data (ALL Double)
    # (required for time series client instantiation)
    Instant = jpsBaseLibView.java.time.Instant
    instant_class = Instant.now().getClass()
    double_class = jpsBaseLibView.java.lang.Double.TYPE

    for sensor in sensor_instances:
        instantiate_weather_sensor(sensor, units, KGClient, instant_class, double_class)

def instantiate_midas_wind():
    
    # Creates a triple store and a database to represent data and knowledge about wind data
    utils.create_knowledge_graph()
    #Extracts and instantiates all wind data reported by MET Office
    sensor_instances, units = process_weather_sensor_data()
    instantiate_all_data(sensor_instances, units)
    print('All data successfully instantiated')

# ===============================================================================
# Instantiate MIDAS Data

if __name__ == '__main__':
    instantiate_midas_wind()