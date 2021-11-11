# This module instantiates several sample geospatial time series (i.e. sample
# consumption time series for various utilities (water, gas, electricity) for
# several consumers with location given as geospatial point
# ===============================================================================

import datetime as dt
import random
import uuid
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


def get_all_consumers():
    '''
        Returns all consumers instantiated in KG as list
    '''

    # Define query
    var = 'cons'
    query = utils.create_sparql_prefix('ex') + \
            utils.create_sparql_prefix('rdf') + \
            '''SELECT ?%s \
               WHERE { ?%s rdf:type ex:Consumer }''' % (var, var)
    # Execute query
    response = KGClient.execute(query)

    # Convert JSONArray String back to list
    response = json.loads(response)
    # Unpack all consumers to list
    consumers = [r[var] for r in response]

    return consumers


def get_consumers_in_circle(center, radius):
    '''
        Returns all instantiated consumers within a radius of 'radius' km from 'center' as list
    '''

    # Define query
    var = 'cons'
    query = utils.create_sparql_prefix('ex') + \
            utils.create_sparql_prefix('rdf') + \
            utils.create_sparql_prefix('geo') + \
            utils.create_sparql_prefix('geolit') + \
            '''SELECT ?%s \
               WHERE { \
                 SERVICE geo:search \
                 { \
                    ?%s geo:search "inCircle" . \
                    ?%s geo:searchDatatype geolit:lat-lon . \
                    ?%s geo:predicate ex:hasLocation . \
                    ?%s geo:spatialCircleCenter "%s" . \
                    ?%s geo:spatialCircleRadius "%s" . \
                 } \
                 ?%s rdf:type ex:Consumer \
               }''' % (var, var, var, var, var, str(center), var, str(radius), var)

    # Execute query
    response = KGClient.execute(query)

    # Convert JSONArray String back to list
    response = json.loads(response)
    # Unpack all consumers to list
    consumers = [r[var] for r in response]

    return consumers



# ===============================================================================
# Query Example Data from KG

if __name__ == '__main__':

    # Initialise remote KG client with only query endpoint specified
    KGClient = jpsBaseLibView.RemoteStoreClient(utils.QUERY_ENDPOINT)

    # Retrieve Java's Instant class to initialise TimeSeriesClient
    Instant = jpsBaseLibView.java.time.Instant
    instant_class = Instant.now().getClass()
    # Initialise TimeSeriesClass
    TSClient = jpsBaseLibView.TimeSeriesClient(instant_class, utils.PROPERTIES_FILE)

    # Get consumers
    #consumers = get_all_consumers()
    consumer = get_consumers_in_circle(center, radius)

    print('')





