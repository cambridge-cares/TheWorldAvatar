###########################################
# Authors: Toby Latcham (tjl47@cam.ac.uk) #
#          Sophie Hall (sh2000@cam.ac.uk) #
# Date: 11 Feb 2022                       #
###########################################




import uuid
from owlready2 import *

# Data Reader and data retrieval modules
from Utils.data_reader_module import *
from Utils.urban_observatory_data_retrieval_module import *
# Instantiation modules
from Utils.urban_observatory_solar_module import *
# Get the JVM module view (via jpsBaseLibGateWay instance) from the jpsSingletons module to access
# the TimeSeriesClient in the JPB_BASE_LIB
from Utils.jpsSingletons import jpsBaseLibView
# Get settings and functions from utils module
import Utils.utils as utils

#def initialise_knowledge_graph():
    #'''Initialises the knowledge graph.'''
    # Create specified PostgreSQL database
    #utils.create_postgres_db()
    # Create specified Blazegraph namespace
    #utils.create_blazegraph_namespace()

def extract_data():
    '''Extract all desired data from Newcastle Observatory API'''
    units = {'Radiation': 'W/m^2'}
    solar_response = retrieve_data('Solar Radiation', 'Solar Radiation', datetime.datetime(2022, 1, 10, 0), datetime.datetime(2022, 1, 11), '5mins')
    return solar_response, units

def process_data(solar_response):
    '''Processes the data and returns it in the proper format.'''
    solar_sensor_names = process_solar_data(solar_response, 'newcastle_data_solar.csv')
    solar_sensors = format_solar_sensors(solar_sensor_names)
    return solar_sensors

def instantiate_urban_observatory_solar_sensors(solar_sensors, units):
    '''Instantiates all extracted Newcastle Observatory sensors'''
    # Initialise remote KG client with query AND update endpoints specified
    KGClient = jpsBaseLibView.RemoteStoreClient(utils.QUERY_ENDPOINT, utils.UPDATE_ENDPOINT)

    # Retrieve Java classes for time entries (Instant) and data (ALL Double)
    # (required for time series client instantiation)
    Instant = jpsBaseLibView.java.time.Instant
    instant_class = Instant.now().getClass()
    double_class = jpsBaseLibView.java.lang.Double.TYPE
    # Loop over all sensors
    for sensor in solar_sensors:
        instantiate_solar_sensor(sensor, units, KGClient, instant_class, double_class)        

def instantiate_solar_sensor(sensor, units, KGClient, instant_class, double_class):
    '''Instantiates a given solar sensor.'''
    print('Current sensor: ', sensor['sensor name'])

    ts = list(sensor['timeseries'].keys())[0]
    # Create IRI for current sensor
    sensorIRI = utils.PREFIXES['ssn'] + 'Sensor_' + str(uuid.uuid4())
    # Create IRI for time series
    dataIRI = utils.PREFIXES['ssn'] + ts + '_' + str(uuid.uuid4())
    # Initialise list of dataIRIs, which will be represented as time series
    dataIRIs = [dataIRI]
        
    # 1) Perform SPARQL update for non-time series related triples (i.e. without TimeSeriesClient)
    query = utils.create_sparql_prefix('ontoweather') + \
            utils.create_sparql_prefix('rdf') + \
            utils.create_sparql_prefix('ssn') + \
            utils.create_sparql_prefix('om') + \
            utils.create_sparql_prefix('geolit') + \
            '''INSERT DATA { \
            <%s> rdf:type ssn:Sensor ; \
                 ontoweather:hasName "%s" ; \
                 ontoweather:observesSolarProperty <%s> ; \
                 ontoweather:WGS84LatitudeLongitude "%s . \
            <%s> rdf:type ontoweather:MeanGlobalSolarRadiation ; \
                 ontoweather:hasTimeseries "%s" ; \
                 ontoweather:hasDescription "%s" ; \
                 om:hasUnit "%s" . }''' % (sensorIRI,
                                            sensor['sensor name'],
                                            dataIRI,
                                            sensor['lat'] + '#' + sensor['lon'] + '\"^^geolit:lat-lon',
                                            dataIRI,
                                            ts + ' measurement',
                                            "INSERT DESCRIPTION HERE",
                                            units[ts])

    KGClient.executeUpdate(query)
    print("Triples independent of Java TimeSeriesClient successfully instantiated.")

    # 2) Perform SPARQL update for time series related triples (i.e. via TimeSeriesClient)
    # Initialise time series in both KG and RDB using TimeSeriesClass
    TSClient = jpsBaseLibView.TimeSeriesClient(instant_class, utils.PROPERTIES_FILE)
    TSClient.initTimeSeries(dataIRIs, [double_class] * len(dataIRIs), utils.FORMAT)

    print("Time series triples via Java TimeSeriesClient successfully instantiated.")

    # 3) Add actual time series data
    # Create Java TimeSeries object with data to attach
    times = sensor['times']
    values = [sensor['timeseries'][v] for v in list(sensor['timeseries'].keys())]
    timeseries = jpsBaseLibView.TimeSeries(times, dataIRIs, values)
    # Add data
    TSClient.addTimeSeriesData(timeseries)

    print("Time series data successfully added.\n")

def instantiate_urban_observatory_solar_data():
    '''Extracts and instantiates all desired Newcastle Observatory data'''
    solar_response, units = extract_data()
    solar_sensors = process_data(solar_response)
    instantiate_urban_observatory_solar_sensors(solar_sensors, units)
    print("All sensors successfully instantiated")

if __name__ == '__main__':
    instantiate_urban_observatory_solar_data()