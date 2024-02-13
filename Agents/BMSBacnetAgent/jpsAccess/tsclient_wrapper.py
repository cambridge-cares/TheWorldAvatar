'''
This module wraps all function regarding timeseries Client of JPS_BASE_LIB
'''
import psycopg2
from jpsAccess.java_gateway import jpsBaseLibView
import config.config as config

TS_FORMAT = '%Y-%m-%dT%H:%M:%SZ'


def initialize(dataIRIs):
    # Retrieve Java classes for time entries (Instant) and data (ALL Double)
    # (required for time series client instantiation)
    Instant = jpsBaseLibView().getView().java.time.Instant
    instant_class = Instant.now().getClass()
    double_class = jpsBaseLibView().getView().java.lang.Double.TYPE
    TSClient = jpsBaseLibView().getView().TimeSeriesClient(instant_class, config.PROPERTIES_FILE)
    TSClient.initTimeSeries(dataIRIs, [double_class] * len(dataIRIs), TS_FORMAT)


def update(timevalues, dataIRIs, values):
    Instant = jpsBaseLibView().getView().java.time.Instant
    instant_class = Instant.now().getClass()
    TSClient = jpsBaseLibView().getView().TimeSeriesClient(instant_class, config.PROPERTIES_FILE)
    times = [(t).strftime(TS_FORMAT) for t in timevalues]

    timeseries = jpsBaseLibView().getView().TimeSeries(times, dataIRIs, values)
    # Add data
    TSClient.addTimeSeriesData(timeseries)

def create_postgres_db(db_name, db_usr, db_pw, db_host):
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
        conn = psycopg2.connect(user=db_usr, password=db_pw, host=db_host)
        conn.autocommit = True
        # Create cursor object
        cur = conn.cursor()
        # Create db table
        cur.execute('CREATE DATABASE ' + db_name)
        print (db_name)
        # Close communication with the PostgreSQL database server
        cur.close()
    except (Exception, psycopg2.DatabaseError) as error:
        print(error)
    finally:
        if conn is not None:
            conn.close()


if __name__ == "__main__":
    pass