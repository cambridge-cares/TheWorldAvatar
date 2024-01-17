################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #
# Date: 20 Jul 2022                            #
################################################

# The purpose of this module is to provide functionality to use
# the TimeSeriesClient from the JPS_BASE_LIB

import pandas as pd
from contextlib import contextmanager

from dhoptimisation.utils import *
from dhoptimisation.utils.baselib_gateway import jpsBaseLibGW
from dhoptimisation.utils.env_configs import DB_URL, DB_USER, DB_PASSWORD


class TSException(Exception):
    """Raise in case of exception when using the TimeSeriesClient."""


class TSClient:

    # Create ONE JVM module view on class level and import all required java classes
    jpsBaseLibView = jpsBaseLibGW.createModuleView()
    jpsBaseLibGW.importPackages(jpsBaseLibView, "uk.ac.cam.cares.jps.base.query.*")
    jpsBaseLibGW.importPackages(jpsBaseLibView, "uk.ac.cam.cares.jps.base.timeseries.*")

    # Date/Time data type: Instant
    # PostgreSQL supported data types: https://www.jooq.org/javadoc/dev/org.jooq/org/jooq/impl/SQLDataType.html
    Instant = jpsBaseLibView.java.time.Instant
    INSTANT = Instant.now().getClass()

    def __init__(self, kg_client, timeclass=INSTANT, rdb_url=DB_URL, 
                 rdb_user=DB_USER, rdb_password=DB_PASSWORD):
        """
        Initialise TimeSeriesClient (default properties taken from environment variables)
        
        Arguments:
            kg_client (KGClient): KGClient object (as per `kgclient.py`)
            timeclass: Java time class objects supported by PostgreSQL
                       (see: https://www.jooq.org/javadoc/dev/org.jooq/org/jooq/impl/SQLDataType.html)
            rdb_url (str): URL of relational database
            rdb_user (str): Username for relational database
            rdb_password (str): Password for relational database
        """

        # 1) Create an instance of a RemoteStoreClient (to retrieve RDB connection)
        try:
            self.connection = TSClient.jpsBaseLibView.RemoteRDBStoreClient(rdb_url, rdb_user, rdb_password)
        except Exception as ex:
            logger.error("Unable to initialise TS Remote Store client.")
            raise TSException("Unable to initialise TS Remote Store client.") from ex

        # 2) Initiliase TimeSeriesClient
        try:
            self.tsclient = TSClient.jpsBaseLibView.TimeSeriesClient(kg_client.kg_client, timeclass)
        except Exception as ex:
            logger.error("Unable to initialise TS client.")
            raise TSException("Unable to initialise TS client.") from ex


    @contextmanager
    def connect(self):
        """
        Create context manager for RDB connection using getConnection method of Java
        TimeSeries client (i.e. to ensure connection is closed after use)
        """
        conn = None
        try:            
            conn = self.connection.getConnection()
            yield conn
        finally:
            if conn is not None:
                conn.close()


    @staticmethod
    def create_timeseries(times: list, dataIRIs: list, values: list):
        """
        Create Java TimeSeries object (i.e. to attach via TSClient)
        
        Arguments:
            times (list): List of time stamps
            dataIRIs (list): List of dataIRIs
            values (list): List of list of values per dataIRI     
        """
        try:
            timeseries = TSClient.jpsBaseLibView.TimeSeries(times, dataIRIs, values)
        except Exception as ex:
            logger.error("Unable to create TimeSeries object.")
            raise TSException("Unable to create timeseries.") from ex
        
        return timeseries


    def init_timeseries(self, dataIRI, times, values, ts_type, time_format):
        """
        This method instantiates a new time series and immediately adds data to it.
        
        Arguments:
            dataIRI (str): IRI of instance with hasTimeSeries relationship
            times (list): List of times/dates
            values (list): List of actual values
            ts_type (Java class): Java class of time series values
            time_format (str): Time format (e.g. "%Y-%m-%dT%H:%M:%SZ")
        """

        with self.connect() as conn:
            self.tsclient.initTimeSeries([dataIRI], [ts_type], time_format, conn)
            ts = TSClient.create_timeseries(times, [dataIRI], [values])
            self.tsclient.addTimeSeriesData(ts, conn)
        logger.info(f"Time series successfully initialised in KG and RDB for dataIRI: {dataIRI}")


    def add_ts_data(self, dataIRI, times, values):
        """
        This method adds time series data to an already instantiated time series
        (potentially already existing values for same time stamp get overwritten)
        
        Arguments:
            dataIRI (str): IRI of instance with hasTimeSeries relationship
            times (list): List of times/dates
            values (list): List of actual values
        """

        with self.connect() as conn:
            ts = TSClient.create_timeseries(times, [dataIRI], [values])
            self.tsclient.addTimeSeriesData(ts, conn)
        logger.info(f"Time series data successfully added to dataIRI: {dataIRI}")


    def replace_ts_data(self, dataIRI, times, values):
        """
        This method replaces all data of an already instantiated time series
        (i.e., clears all existing data and adds new data)
        
        Arguments:
            dataIRI (str): IRI of instance with hasTimeSeries relationship
            times (list): List of times/dates
            values (list): List of actual values
        """

        with self.connect() as conn:
            # Clear existing data
            t1 = self.tsclient.getMinTime(dataIRI, conn)
            t2 = self.tsclient.getMaxTime(dataIRI, conn)
            self.tsclient.deleteTimeSeriesHistory(dataIRI, t1, t2, conn)
            # Add new data
            ts = TSClient.create_timeseries(times, [dataIRI], [values])
            self.tsclient.addTimeSeriesData(ts, conn)
        logger.info(f"Time series data successfully added to dataIRI: {dataIRI}")


    def retrieve_timeseries(self, dataIRI, lowerbound=None, upperbound=None):
        """
        This method retrieves the time series data for a given dataIRI
        
        Arguments:
            dataIRI (str): IRI of instance with hasTimeSeries relationship
            lowerbound (str): Lower bound of time series data
            upperbound (str): Upper bound of time series data
        """
        
        with self.connect() as conn:
            ts = self.tsclient.getTimeSeriesWithinBounds([dataIRI], lowerbound, 
                                                         upperbound, conn)
        times = ts.getTimes()
        values = ts.getValues(dataIRI)

        # Unwrap Java time objects
        times = [t.toString() for t in times]
        
        return times, values


    def retrieve_timeseries_as_dataframe(self, dataIRI, column_name, lowerbound=None, 
                                         upperbound=None, index_name="time"):
        """
        Retrieve time series data for provided dataIRI between given bounds and 
        returns as pandas dataframe with given column names

        Arguments:
            dataIRI (str): IRI of instance with hasTimeSeries relationship
            lowerbound (str): Lower bound of time series data
            upperbound (str): Upper bound of time series data
            column_name {str}: column name for values
            index_name {str}: column/index name for times
        """
        
        # Retrieve time series data
        times, values = self.retrieve_timeseries(dataIRI, lowerbound, upperbound)
        if len(values) == 0:
            logger.info(f'No time series data available for dataIRI \"{dataIRI}" '
                        + f'between {lowerbound} and {upperbound}.')
            raise ValueError
        logger.info(f'Loaded {len(values)} values for dataIRI "{dataIRI}" '
                    + f'from "{lowerbound}" to "{upperbound}".')

        # Create Dataframe
        df = pd.DataFrame(zip(values, times), columns=[column_name, index_name])
        # Remove time zone and convert to datetime
        df[index_name] = pd.to_datetime(df[index_name]).dt.tz_convert('UTC').dt.tz_localize(None)
        # Set time column as index (to ease later merging with other ts)
        df.set_index(index_name, inplace=True)
        
        return df