###########################################
# Authors: Toby Latcham (tjl47@cam.ac.uk) #
#          Sophie Hall (sh2000@cam.ac.uk) #
# Date: 11 Feb 2022                       #
###########################################




# ===============================================================================

import uuid
import pandas as pd
from Utils.data_reader_module import *

# Get the JVM module view (via jpsBaseLibGateWay instance) from the jpsSingletons module to access
# the TimeSeriesClient in the JPB_BASE_LIB
from Utils.jpsSingletons import jpsBaseLibView
# Get settings and functions from utils module
import Utils.utils as utils
# Import sensor coordinates download function from Markus H's module
from Utils.met_office_solar_sensor_coordinates import get_sensor_coords

# ===============================================================================

def get_coords():
    '''Returns a dictionary of sensor coordinates from the
    file downloaded by the get_sensor_coords function.
    '''
    station_loc = pd.read_csv('./Data/MIDAS_solar_data/stationlocationssolar.csv')
    lat_list = station_loc['Latitude']
    lon_list = station_loc['Longitude']
    id_list = station_loc['src_id']

    coordinates = {}
    n = 0
    for sensor_id in id_list:
        coordinates[sensor_id] = [str(lat_list[n]), str(lon_list[n])]
        n+=1
    return coordinates

def merge_midas():
    '''Function merges MIDAS data file with the column headers file to
    create a new file (data downloaded using seperate script).
    '''
    # convert txt file to csv file
    file = pd.read_csv (r'./Data/MIDAS_solar_data/midas_radtob_202101-202112.txt')
    file.to_csv (r'./Data/MIDAS_solar_data/midas_radtob_202101-202112.csv', index=None)

    # load data
    headers = pd.read_csv('./Data/MIDAS_solar_data/RO_Column_Headers.csv')
    data = pd.read_csv('./Data/MIDAS_solar_data/midas_radtob_202101-202112.csv', header=None)

    # condition data
    data_extract = data.iloc[:, :20]
    headers_extract = [c.strip() for c in headers.columns[:20]]

    # combine data with headers
    combined = data_extract.copy()
    combined.columns = headers_extract

    # write data to combined file
    combined.to_csv('./Data/MIDAS_solar_data/MIDASsolar.csv', index=False)

def extract_data(coordinates):
    '''Returns a dictionary containing all desired data for each sensor.

    Data extracted from the data file created by the merge_midas funtion,
    and the coordinates dictionary created by the get_coords function.
    '''
    solar_data = pd.read_csv('./Data/MIDAS_solar_data/MIDASsolar.csv')

    # extracting a list of unique sensor IDs
    sensor_ids = solar_data['SRC_ID']
    sensor_list = []
    for sensor_id in sensor_ids:
        if sensor_id not in sensor_list:
            sensor_list.append(sensor_id)
    # Producing a dictionary of sensors and their corresponding data
    sensor_data = {}
    for sensor in sensor_list:
        # Checking to see if we have coordinates for this sensor, can't visualise if we don't
        if sensor in coordinates.keys():

            data_rows = get_df_selected_rows(solar_data, 'SRC_ID', sensor)

            # Getting coordinates
            lat = str(coordinates[sensor][0])
            lon = str(coordinates[sensor][1])

            # Getting timeseries data
            tstamps = [] # timestamps
            tstamps0 = get_df_column_data(data_rows, 'OB_END_TIME')
            GIRvalue = [] # global irradiation
            GIRvalue0 = get_df_column_data(data_rows, 'GLBL_IRAD_AMT')


            # Iterating through measurements
            n = 0
            for tstamp in tstamps0:
                time_string = str(tstamp)
                #print(time_string[6:11])

                # Making sure we only get data for the desired time period
                if time_string[6:11] == '01-01':
                    # Getting timestamp into correct format
                    tstamps.append(time_string[1:11]+'T'+time_string[12:19]+':00Z')

                    # Testing for valid data values, if valid the values are appended to the appropriate list
                    if str(GIRvalue0.iloc[n]) == ' ':
                        # Zeroes are used for now for invalid values, but Null may be possible
                        GIRvalue.append(float(0))
                    else:
                        # Value divided by 1800 seconds to convert from energy to power
                        GIRvalue.append(float(GIRvalue0.iloc[n])/1800)
                n += 1
            # Final value in each day is total solar energy, so must be removed
            GIRvalue = GIRvalue[:-1]
            tstamps = tstamps[:-1]
            # Checking for sensors that didn't have any measurements (these cause issues)
            if len(tstamps) > 0:
                sensor_data[sensor] = [tstamps, GIRvalue, lon, lat]
            # Adding virtual cambridge sensor, copying sensor data from sensor near Huntingdon
            if str(sensor) == '456':
                sensor_data['Virtual Cambridge Sensor'] = [tstamps, GIRvalue, '0.1218', '52.2053']
    #print(sensor_data)
    return sensor_data

def format_data(sensor_data):
    '''Returns a formatted dictionary of all sensor data.'''
    sensor_instances = []
    n = 0
    for sensor in sensor_data.keys():
        sensor_dict = {}
        sensor_dict['sensor name'] = sensor
        sensor_dict['lat'] = sensor_data[sensor][3]
        sensor_dict['lon'] = sensor_data[sensor][2]
        sensor_dict['times'] = sensor_data[sensor][0]
        sensor_dict['timeseries'] = {'GlobalIrradiation' : sensor_data[sensor][1]}
        sensor_instances.append(sensor_dict)
        if n == 0:
            print(sensor_dict)
        n+=1
    return sensor_instances

def instantiate_weather_sensor(sensor, units, KGClient, instant_class, double_class):
    '''Instantiates a given sensor along with all its data'''
    print('Current sensor: ', sensor['sensor name'])

    solar_description = 'INSERT DESCRIPTION HERE'

    # Create IRI for current sensor
    sensorIRI = utils.PREFIXES['ssn'] + 'Sensor_' + str(uuid.uuid4())

    # Perform SPARQL update for non-time series related triples (i.e. without TimeSeriesClient)
    query = utils.create_sparql_prefix('ontoweather') + \
            utils.create_sparql_prefix('ssn') + \
            utils.create_sparql_prefix('rdf') + \
            utils.create_sparql_prefix('geolit') + \
            '''INSERT DATA { \
            <%s> rdf:type ontoweather:SolarSensor ; \
                 ontoweather:hasName "%s" ; \
                 ontoweather:WGS84LatitudeLongitude "%s . }''' % ( sensorIRI,
                                                                   sensor['sensor name'],
                                                                   sensor['lat'] + '#' + sensor['lon'] + '\"^^geolit:lat-lon' )
    #print(query)
    KGClient.executeUpdate(query)

    # Initialise list of dataIRIs, which will be represented as time series
    dataIRIs = []

    # Loop over all time series for this sensor
    k = 0
    for ts in list(sensor['timeseries'].keys()):
        dataIRI = instantiate_timeseries(ts, solar_description, units, sensorIRI, KGClient)
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
    variables = dataIRIs
    values = [sensor['timeseries'][v] for v in list(sensor['timeseries'].keys())]
    timeseries = jpsBaseLibView.TimeSeries(times, variables, values)
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
            <%s> ontoweather:observesSolarProperty <%s> . }''' % ( dataIRI,
                                                                  ts+' measurements',
                                                                  units[ts],
                                                                  description,
                                                                  sensorIRI, dataIRI )
    #print(query)
    KGClient.executeUpdate(query)
    print('Triples independent of Java TimeSeriesClient successfully instantiated.')
    return dataIRI


#def initialise_knowledge_graph():
    '''Initialises the knowledge graph.'''
    # Create specified PostgreSQL database
    #utils.create_postgres_db()
    # Create specified Blazegraph namespace
    #utils.create_blazegraph_namespace()

def process_weather_sensor_data():
    '''Retrieves all desired data and returns a formatted dictionary
    of sensor data, along with the units for each measurement.
    '''
    # Get data
    get_sensor_coords()
    coordinates = get_coords()
    merge_midas()
    sensor_data = extract_data(coordinates)
    sensor_instances = format_data(sensor_data)
    units = {'GlobalIrradiation': 'kW/m2'}
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

def instantiate_midas_solar():
    '''Extracts and instantiates all MIDAS solar data'''
    sensor_instances, units = process_weather_sensor_data()
    instantiate_all_data(sensor_instances, units)
    print('All data successfully instantiated')

# ===============================================================================
# Instantiate MIDAS Data

if __name__ == '__main__':
    instantiate_midas_solar()