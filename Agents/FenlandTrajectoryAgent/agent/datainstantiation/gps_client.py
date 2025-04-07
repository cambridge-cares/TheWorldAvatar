import pandas as pd
import os
import sys
import agent.kgutils.utils as utils
import glob
import datetime as dt
import random
import uuid
# import agent.datainstantiation.jpsSingletons as jpsSingletons
from agent.datainstantiation.jpsSingletons import stackClientsGw
from agent.kgutils.stackclients import StackClient
from agent.kgutils.kgclient import KGClient
from agent.kgutils.tsclient import TSClient
from datetime import datetime
from agent.kgutils.utils import FORMAT
from agent.utils.stack_configs import DB_URL, DB_USER, DB_PASSWORD, SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT
import logging


logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

def setup_clients():
    """Sets up and returns the KGClient, TSClient and point_class."""
    try:
        # Instant = jpsBaseLibView.java.time.Instant
        Instant = StackClient.stackClients_view.java.time.Instant
        
        instant_class = Instant.now().getClass()
        double_class = StackClient.stackClients_view.java.lang.Double.TYPE
        kg_client = KGClient(utils.SPARQL_QUERY_ENDPOINT, utils.SPARQL_UPDATE_ENDPOINT)
        ts_client = TSClient(kg_client, timeclass=instant_class, rdb_url=DB_URL, rdb_user=DB_USER, rdb_password=DB_PASSWORD)
        point_class = StackClient.stackClients_view.Point().getClass()
        # point_class = StackClient.stackClients_view.Point
        try:
            testpoint = StackClient.stackClients_view.Point(0.0, 0.0)
            logger.info(f"Point class imported and instance creadted successfully")
        except Exception as e:
            logger.error(f"Failed to create Point instance: {e}")
            raise e
        logger.info("KGClient, TSClient and point class setup successfully.")
        return kg_client, ts_client, double_class, point_class
    except Exception as e:
        logger.error(f"Failed to setup clients: {e}")
        raise e


def transform_datetime(date_str, time_str):
    """Transforms separate date and time strings into a single datetime string in ISO 8601 format."""
    try:
        combined_str = date_str.strip() + " " + time_str.strip()
        datetime_obj = datetime.strptime(combined_str, "%Y/%m/%d %H:%M:%S")
        return datetime_obj.strftime('%Y-%m-%dT%H:%M:%SZ')
    except Exception as e:
        logger.error(f"Error transforming datetime: {date_str}, {time_str} - {e}")
        return None

def process_gps_csv_file(csv_file):
    """Processes a single GPS CSV file and returns its contents as a data object."""
    try:
        df = pd.read_csv(csv_file)
        df.columns = df.columns.str.strip()
        required_columns = ['UTC DATE', 'UTC TIME', 'SPEED', 'DISTANCE', 'HEIGHT','LATITUDE','LONGITUDE']
        if not all(column in df.columns for column in required_columns) or df[required_columns].isnull().any().any():
            logger.warning(f"Skipping {csv_file} due to missing or incomplete required columns.")
            return None
        # try:
        #     point_class = StackClient.stackClients_view.Point().getClass()
        #     logger.info(f"Point class imported successfully")
        # except Exception as e:
        #     logger.error(f"Failed to create Point instance: {e}")
        #     raise e
        # point_class = StackClient.stackClients_view.Point
        points = [StackClient.stackClients_view.Point(row['LONGITUDE'], row['LATITUDE']) for _, row in df.iterrows()]
        return {
            'object': os.path.basename(csv_file).replace('.csv', ''),
            'times': [transform_datetime(row['UTC DATE'], row['UTC TIME']) for _, row in df.iterrows()],
            'timeseries': {
                'Speed': df['SPEED'].tolist(),
                'Distance': df['DISTANCE'].tolist(),
                'Height': df['HEIGHT'].tolist(),
                'Heading': df.get('HEADING', []).tolist(),
                'Latitude': df.get('LATITUDE', []).tolist(),
                'Longitude': df.get('LONGITUDE', []).tolist(),
                'Point': points
            },
            'units': {
                'Speed': 'kilometer per hour',
                'Distance': 'meter',
                'Height': 'meter',
                'Heading': 'degree',
                'Latitude': 'degree',
                'Longitude': 'degree',
                'Point': 'geom'
            }
        }
    except Exception as e:
        logger.error(f"Failed to process file {csv_file}: {e}")
        return None

def instantiate_gps_data(gps_object, kg_client, ts_client, double_class, point_class):
    """Takes a processed GPS data object and instantiates it into the RDF store and time series database."""
    try:
        objectIRI = utils.PREFIXES['ontodevice'] + 'GPSDevice/' + str(uuid.uuid4())
        dataIRIs = []
        for ts in gps_object['timeseries']:
            # Add a slash after 'owl'
            dataIRI = utils.PREFIXES['ontodevice'] + ts + '/' + str(uuid.uuid4())
            dataIRIs.append(dataIRI)
            unit = gps_object['units'][ts] if gps_object['units'][ts] else ""
            query = utils.create_sparql_prefix('om') + \
                    utils.create_sparql_prefix('rdf') + \
                    utils.create_sparql_prefix('rdfs') + \
                    utils.create_sparql_prefix('ontodevice') + \
                    utils.create_sparql_prefix('geolit') + \
                    f'''INSERT DATA {{
                    <{objectIRI}> rdf:type ontodevice:GPSDevice ;
                         rdfs:label "{gps_object['object']}" ;
                         ontodevice:has{ts} <{dataIRI}> .
                    <{dataIRI}> rdf:type ontodevice:{ts} ;
                         rdfs:label "{ts} data for {gps_object['object']}" ;
                         om:unit "{unit}" . }}'''
            kg_client.performUpdate(query)
        
        # Ensure times are properly formatted
        times = gps_object['times']
        values_list = [gps_object['timeseries'][ts] for ts in gps_object['timeseries']]
        # ts_type = jpsBaseLibView.java.lang.Double.TYPE
        ts_type = [double_class if ts != 'Point' else point_class for ts in gps_object['timeseries']]
        time_format = FORMAT
            
        # Detailed logging before calling init_timeseries
        logger.debug(f"DataIRIs: {dataIRIs}")
        logger.debug(f"Times: {times}")
        logger.debug(f"Values: {values_list}")
        logger.debug(f"TS Type: {ts_type}")
        logger.debug(f"Time Format: {time_format}")
        logger.debug(f"DB_USER: {DB_USER}")
        logger.debug(f"DB_URL: {DB_URL}")
        logger.debug(f"DB_PASSWORD: {DB_PASSWORD}")


       # Logging the types of the variables to ensure correct types for java constructors
        logger.debug(f"DataIRIs: {dataIRIs} - Type: {type(dataIRIs)}")
        for dataIRI in dataIRIs:
            logger.debug(f"DataIRI element: {dataIRI} - Type: {type(dataIRI)}")
        logger.debug(f"Times: {times} - Type: {type(times)}")
        for t in times:
            logger.debug(f"Time element: {t} - Type: {type(t)}")
        logger.debug(f"Values: {values_list} - Type: {type(values_list)}")
        for v in values_list:
            logger.debug(f"Value element: {v} - Type: {type(v)}")
        logger.debug(f"TS Type: {ts_type} - Type: {type(ts_type)}")
        logger.debug(f"Time Format: {time_format} - Type: {type(time_format)}")



        # Logging to ensure dataIRI and values_list are lists of the same length
        logger.info(f"Initializing time series for DataIRIs: {dataIRIs}")
        logger.info(f"Initializing time series for Times: {times}")
        logger.info(f"Initializing time series for Values: {values_list}")
        logger.info(f"Initializing time series for ts_type(dataClass): {ts_type}")
        logger.info(f"Initializing time series for time_format(timeUnit): {time_format}")

        # Core function for instantiation
        # ts_client.init_timeseries(dataIRI=dataIRIs, times=times, values=values_list, ts_type=[ts_type]*len(dataIRIs), time_format=time_format)
        ts_client.init_timeseries(dataIRI=dataIRIs, times=times, values=values_list, ts_type=ts_type, time_format=time_format)

        logger.info(f"Data for {gps_object['object']} successfully instantiated.")
    except Exception as e:
        logger.error(f"Error instantiating data for {gps_object['object']}: {e}")
        raise

def update_unix_time(ts_client, dataIRI):
    """
    This function is used to add a UNIX column for the timeseries data instantiated by TSClient
    Note that TSClient has autoCommit settings, thus conn.commit() has been removed in this function
    """
    try:
        with ts_client.connect() as conn:
            stmt = conn.createStatement()
            sql_find = f'''
                SELECT "tableName"
                FROM "dbTable"
                WHERE "dataIRI" = '{dataIRI}'
            '''
            rs = stmt.executeQuery(sql_find)
            if rs.next():
                table_name = rs.getString("tableName")
                sql_alter = f'''
                    ALTER TABLE "{table_name}"
                    ADD COLUMN IF NOT EXISTS "UNIX_time" BIGINT;
                '''
                stmt.execute(sql_alter)
                sql_update = f'''
                    UPDATE "{table_name}"
                    SET "UNIX_time" = (EXTRACT(EPOCH FROM "time") * 1000)::BIGINT
                '''
                stmt.execute(sql_update)
                logger.info(f"UNIX_time column updated for table: {table_name}")
            else:
                logger.warning(f"No matching table found in dbTable for dataIRI={dataIRI}")
            stmt.close()
        return True
    except Exception as e:
        logger.error("Error updating UNIX_time column via TSClient: %s", e)
        return False

if __name__ == '__main__':
    try:
        utils.create_postgres_db()
        utils.create_blazegraph_namespace()
        kg_client, ts_client, double_class, point_class = setup_clients()
        csv_files = glob.glob(os.path.join(os.path.dirname(os.path.realpath(__file__)), '..', 'raw_data', 'gps_target_folder', '*.csv'))
        for csv_file in csv_files:
            gps_object = process_gps_csv_file(csv_file)
            if gps_object:
                instantiate_gps_data(gps_object, kg_client, ts_client, double_class, point_class)
    except Exception as e:
        logger.error(f"Initialization failed: {e}")
        raise e

