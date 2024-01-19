'''
This module wraps all function regarding timeseries Client of JPS_BASE_LIB
'''
import psycopg2
from .java_gateway import jpsBaseLibView
from data_classes.ts_data_classes import *
import logging
import datetime
from typing import List








def get_ts_client(property_file_path:str, data_class_name:str = 'Instant'):
    TSClient = jpsBaseLibView().getView().TimeSeriesClient(get_java_time_object(data_class_name), property_file_path)
    return TSClient


def get_java_time_object(data_class_name:str='Instant'):
    JavaTimeClass = getattr(jpsBaseLibView().getView().java.time, data_class_name)
    data_class = JavaTimeClass.now().getClass()
    return data_class


class TSClient():
    def __init__(self, property_file_path:str, time_class:str='Instant'):
        self.client = get_ts_client(property_file_path,time_class)
        self.time_class =  time_class

    def register_timeseries(self, ts_meta: TimeSeriesMeta):
        # Retrieve Java classes for time entries (Instant) and data (ALL Double)
        # (required for time series client instantiation)
        double_class = jpsBaseLibView().getView().java.lang.Double.TYPE
        time_units = ts_meta.time_unit
        src_iris = [ ts_meta.src_iri ]
        self.client.initTimeSeries(src_iris, [double_class], time_units)

    # Note that different property files might be requested as target endpoints might be different for each TS instance
    def add_timeseries(self, tsinstance: TimeSeriesInstance):
        times = tsinstance.times
        times = [parse_time_to_format(t, self.time_class) for t in times]
        values = [tsinstance.values]
        dataIRIs = [tsinstance.src_iri]
        timeseries = jpsBaseLibView().getView().TimeSeries(times, dataIRIs, values)
        # Add data
        self.client.addTimeSeriesData(timeseries)

    def check_timeseries_exist(self, datairi):
        tsiri = self.client.getTimeSeriesIRI(datairi)
        return False if not tsiri else self.client.checkTimeSeriesExists(tsiri)

    def update_timeseries_if_new(self,tsinstance: TimeSeriesInstance) -> bool:
        new_times = tsinstance.times
        new_times = [parse_time_to_format(t, self.time_class) for t in new_times]
        old_times,old_values = self.get_timeseries( tsinstance.src_iri)
        if old_times is not None: # Have existing records, check if API values has change compared to record
            old_times = [t for i,t in enumerate(old_times) if old_values[i]-0.0 > 0] #remove paddings for prediction
            to_update_idx = [ idx for idx,t in enumerate(new_times) if t not in old_times]
            if not to_update_idx:
                logging.info('API for {} has not updated since last time. No update is made to KG.'.format(tsinstance.src_iri))
                return False
        new_values = [tsinstance.values]
        dataIRIs = [tsinstance.src_iri]
        timeseries = jpsBaseLibView().getView().TimeSeries(new_times, dataIRIs, new_values)
        lowb = jpsBaseLibView().getView().java.time.Instant.parse(new_times[0])
        upb = jpsBaseLibView().getView().java.time.Instant.parse(new_times[-1])
        self.client.deleteTimeSeriesHistory(tsinstance.src_iri,lowb,upb)
        self.client.addTimeSeriesData(timeseries)
        return True



    def get_timeseries(self, ts_iri:str):
        ts = self.client.getTimeSeriesWithinBounds([ts_iri], None, None)
        times = ts.getTimes()
        values = ts.getValues(ts_iri)
        # Unwrap Java time objects
        times = [t.toString() for t in times]
        return times, list(values)



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