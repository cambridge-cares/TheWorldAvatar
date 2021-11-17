# This module instantiates several sample geospatial time series (i.e. sample
# consumption time series for various utilities (water, gas, electricity) for
# several consumers with location given as geospatial point
# ===============================================================================

import datetime as dt
import random
import uuid


# Get the JVM module view (via jpsBaseLibGateWay instance) from the jpsSingletons module to access
# the TimeSeriesClient in the JPB_BASE_LIB
from jpsSingletons import jpsBaseLibView
# Get settings and functions from utils module
import utils

# ===============================================================================
# Specify Example Data
# All coordinates are given in EPSG:4326 CRS, neglecting any elevation (i.e. Z coordinate)

# Specify length of sample time series
n = 10

# Specify 3 sample consumers and generate sample time series data
consumer1 = {'consumer': 'Pembroke College',
             'lat': '52.201781',   # latitude
             'lon': '0.118648',    # longitude
             'times': [(dt.datetime.now() - dt.timedelta(hours=i)).strftime(utils.FORMAT) for i in range(n)],
             'timeseries': {
                 'Electricity': [float(random.randrange(10, 50000)/10) for i in range(n)],
                 #'water': [float(random.randrange(0, 1000)/10) for i in range(n)],
                 #'gas': [float(random.randrange(0, 25000)/10) for i in range(n)],
             },
}

consumer2 = {'consumer': 'Churchill College',
             'lat': '52.212679',
             'lon': '0.103013',
             'times': [(dt.datetime.now() - dt.timedelta(hours=2*i)).strftime(utils.FORMAT) for i in range(n)],
             'timeseries': {
                 'Electricity': [float(random.randrange(10, 50000)/10) for i in range(n)],
                 'Water': [float(random.randrange(0, 1000)/10) for i in range(n)],
                 #'gas': [float(random.randrange(0, 25000)/10) for i in range(n)],
             },
}

consumer3 = {'consumer': 'Department of Chemical Engineering and Biotechnology',
             'lat': '52.209452',
             'lon': '0.085788',
             'times': [(dt.datetime.now() - dt.timedelta(minutes=30*i)).strftime(utils.FORMAT) for i in range(n)],
             'timeseries': {
                 'Electricity': [float(random.randrange(10, 50000)/10) for i in range(n)],
                 'Water': [float(random.randrange(0, 1000)/10) for i in range(n)],
                 'Gas': [float(random.randrange(0, 25000)/10) for i in range(n)],
             },
}

units = {'Electricity': 'kWh/h',
         'Water': 'm^3/h',
         'Gas': 'Btu/h'
         }

# ===============================================================================
# Instantiate Example Data

if __name__ == '__main__':

    # Create specified PostgreSQL database
    utils.create_postgres_db()
    # Create specified Blazegraph namespace
    utils.create_blazegraph_namespace()

    # Initialise remote KG client with query AND update endpoints specified
    KGClient = jpsBaseLibView.RemoteStoreClient(utils.QUERY_ENDPOINT, utils.UPDATE_ENDPOINT)

    # Retrieve Java classes for time entries (Instant) and data (ALL Double)
    # (required for time series client instantiation)
    Instant = jpsBaseLibView.java.time.Instant
    instant_class = Instant.now().getClass()
    double_class = jpsBaseLibView.java.lang.Double.TYPE

    # Loop over all consumers
    consumers = [consumer1, consumer2, consumer3]
    for c in consumers:
        print('Current consumer: ', c['consumer'])

        # Create IRI for current consumer
        consumerIRI = utils.PREFIXES['ex'] + 'Consumer_' + str(uuid.uuid4())

        # Initialise list of dataIRIs, which will be represented as time series
        dataIRIs = []

        # Loop over all time series for this consumer
        for ts in list(c['timeseries'].keys()):

            # Create IRI for current time series and attach to dataIRI list
            dataIRI = utils.PREFIXES['ex'] + ts + '_' + str(uuid.uuid4())
            dataIRIs.append(dataIRI)

            # 1) Perform SPARQL update for non-time series related triples (i.e. without TimeSeriesClient)
            query = utils.create_sparql_prefix('ex') + \
                    utils.create_sparql_prefix('rdf') + \
                    utils.create_sparql_prefix('rdfs') + \
                    utils.create_sparql_prefix('geolit') + \
                    '''INSERT DATA { \
                    <%s> rdf:type ex:Consumer ; \
                         rdfs:label "%s" ; \
                         ex:consumes <%s> ; \
                         ex:hasLocation "%s . \
                    <%s> rdf:type ex:Consumption ; \
                         rdfs:label "%s" ; \
                         ex:unit "%s" . }''' % ( consumerIRI, c['consumer'], dataIRI,
                                                 c['lat'] + '#' + c['lon'] + '\"^^geolit:lat-lon',
                                                 dataIRI, ts+' consumption', units[ts] )

            KGClient.executeUpdate(query)
            print("Triples independent of Java TimeSeriesClient successfully instantiated.")

        # 2) Perform SPARQL update for time series related triples (i.e. via TimeSeriesClient)
        # Initialise time series in both KG and RDB using TimeSeriesClass
        TSClient = jpsBaseLibView.TimeSeriesClient(instant_class, utils.PROPERTIES_FILE)
        TSClient.initTimeSeries(dataIRIs, [double_class]*len(dataIRIs), utils.FORMAT)

        print("Time series triples via Java TimeSeriesClient successfully instantiated.")

        # 3) Add actual time series data
        # Create Java TimeSeries object with data to attach
        times = c['times']
        variables = dataIRIs
        values = [c['timeseries'][v] for v in list(c['timeseries'].keys())]
        timeseries = jpsBaseLibView.TimeSeries(times, variables, values)
        # Add data
        TSClient.addTimeSeriesData(timeseries)

        print("Time series data successfully added.\n")

    print("All data successfully instantiated")


