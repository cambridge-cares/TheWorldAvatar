'''
This module wraps all function regarding timeseries Client of JPS_BASE_LIB
'''
import psycopg2
from .java_gateway import jpsBaseLibView
from data_types import ts_data_classes
import logging
from typing import List


def register_timeseries(property_file_path:str,ts_meta: ts_data_classes.TimeSeriesMeta):
    # Retrieve Java classes for time entries (Instant) and data (ALL Double)
    # (required for time series client instantiation)
    Instant = jpsBaseLibView().getView().java.time.Instant
    instant_class = Instant.now().getClass()
    double_class = jpsBaseLibView().getView().java.lang.Double.TYPE
    TSClient = jpsBaseLibView().getView().TimeSeriesClient(instant_class, property_file_path)
    time_units = ts_meta.time_unit
    src_iris = [ ts_meta.src_iri ]
    TSClient.initTimeSeries(src_iris, [double_class], time_units)

# Note that different property files might be requested as target endpoints might be different for each TS instance
def add_timeseries(property_file_path:str,tsinstance: ts_data_classes.TimeSeriesInstance):
    LocalDate = jpsBaseLibView().getView().java.time.LocalDate
    data_class = LocalDate.now().getClass()
    TSClient = jpsBaseLibView().getView().TimeSeriesClient(data_class, property_file_path)
    times = tsinstance.times
    times = [ "{}-01-01".format(t) for t in times]
    print(times)
    values = [tsinstance.values]
    dataIRIs = [tsinstance.src_iri]
    timeseries = jpsBaseLibView().getView().TimeSeries(times, dataIRIs, values)
    # Add data
    TSClient.addTimeSeriesData(timeseries)

#TODO
def query_timeseries(consumer):
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

def create_postgres_db_if_not_exists(db_name, db_usr, db_pw):
    """
        Creates PostgreSQL database with name as specified in db.url field in the properties file
        Please note: The PostgreSQL server is assumed to be available at DEFAULT HOST (i.e. localhost)
        and PORT (i.e. 5432)
    """

    # Extract database name from DB URL provided in properties file
    # (for details see: https://www.postgresql.org/docs/7.4/jdbc-use.html)

    # Create PostgreSQL database with extracted name
    # (for details see: https://www.psycopg.org/docs/module.html)
    conn = None
    try:
        # Connect to PostgreSQL server (via DEFAULT host and port)
        conn = psycopg2.connect(user=db_usr, password=db_pw, host='localhost')
        conn.autocommit = True
        # Create cursor object
        cur = conn.cursor()
        # Create db table
        cur.execute("SELECT datname FROM pg_database;")
        list_database = cur.fetchall()
        if (db_name,) in list_database:
            logging.info('postgresql db {} already exists, skip creation'.format(db_name))
        else:
            cur.execute('CREATE DATABASE ' + db_name)
        # Close communication with the PostgreSQL database server
        cur.close()
    except (Exception, psycopg2.DatabaseError) as error:
        print(error)
    finally:
        if conn is not None:
            conn.close()


if __name__ == "__main__":
    pass