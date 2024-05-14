import pandas as pd
import os
import sys
import agent.kgutils.utils as utils
import glob
import datetime as dt
import random
import uuid
from agent.datainstantiation.jpsSingletons import jpsBaseLibView
from agent.kgutils.kgclient import KGClient
from agent.kgutils.tsclient import TSClient
from datetime import datetime
from agent.utils.stack_configs import DB_URL, DB_USER, DB_PASSWORD, SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT

def setup_clients():
    """Sets up and returns the KGClient and TSClient."""
    Instant = jpsBaseLibView.java.time.Instant
    instant_class = Instant.now().getClass()
    double_class = jpsBaseLibView.java.lang.Double.TYPE
    kg_client = KGClient(utils.SPARQL_QUERY_ENDPOINT, utils.SPARQL_UPDATE_ENDPOINT)
    ts_client = TSClient(kg_client, timeclass=instant_class, rdb_url=utils.DB_URL, rdb_user=utils.DB_USER, rdb_password=utils.DB_PASSWORD)
    return kg_client, ts_client, double_class

def transform_datetime(date_str, time_str):
    """Transforms separate date and time strings into a single datetime string in ISO 8601 format."""
    try:
        combined_str = date_str.strip() + " " + time_str.strip()
        datetime_obj = datetime.strptime(combined_str, "%Y/%m/%d %H:%M:%S")
        return datetime_obj.strftime('%Y-%m-%dT%H:%M:%SZ')
    except Exception as e:
        print(f"Error transforming datetime: {date_str}, {time_str} - {e}")
        return None

def process_gps_csv_file(csv_file):
    """Processes a single GPS CSV file and returns its contents as a data object."""
    try:
        df = pd.read_csv(csv_file)
        df.columns = df.columns.str.strip()
        required_columns = ['UTC DATE', 'UTC TIME', 'SPEED', 'DISTANCE', 'HEIGHT']
        if not all(column in df.columns for column in required_columns) or df[required_columns].isnull().any().any():
            print(f"Skipping {csv_file} due to missing or incomplete required columns.")
            return None
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
            },
            'units': {
                'Speed': 'km/h',
                'Distance': 'M',
                'Height': 'M',
                'Heading': None,
                'Latitude': None,
                'Longitude': None
            }
        }
    except Exception as e:
        print(f"Failed to process file {csv_file}: {e}")
        return None

def instantiate_gps_data(gps_object, kg_client, ts_client, double_class):
    """Takes a processed GPS data object and instantiates it into the RDF store and time series database."""
    try:
        objectIRI = utils.PREFIXES['ontodevice'] + 'Object_' + str(uuid.uuid4())
        dataIRIs = []
        for ts, values in gps_object['timeseries'].items():
            dataIRI = utils.PREFIXES['ontodevice'] + ts + '_' + str(uuid.uuid4())
            dataIRIs.append(dataIRI)
            unit = gps_object['units'][ts] if gps_object['units'][ts] else ""
            query = utils.create_sparql_prefix('ex') + \
                    utils.create_sparql_prefix('rdf') + \
                    utils.create_sparql_prefix('rdfs') + \
                    utils.create_sparql_prefix('ontodevice') + \
                    utils.create_sparql_prefix('geolit') + \
                    f'''INSERT DATA {{
                    <{objectIRI}> rdf:type ontodevice:Object ;
                         rdfs:label "{gps_object['object']}" ;
                         ontodevice:has{ts} <{dataIRI}> .
                    <{dataIRI}> rdf:type ontodevice:{ts} ;
                         rdfs:label "{ts} data for {gps_object['object']}" ;
                         ex:unit "{unit}" . }}'''
            kg_client.performUpdate(query)
        ts_client.init_timeseries(dataIRI=dataIRIs, times=gps_object['times'], values=[gps_object['timeseries'][ts] for ts in gps_object['timeseries']], ts_type=[double_class] * len(dataIRIs), time_format=utils.FORMAT)
        print(f"Data for {gps_object['object']} successfully instantiated.")
    except Exception as e:
        print(f"Error instantiating data for {gps_object['object']}: {e}")

# Example usage within the script:
if __name__ == '__main__':
    try:
        utils.create_postgres_db()
        utils.create_blazegraph_namespace()
        kg_client, ts_client, double_class = setup_clients()
        csv_files = glob.glob(os.path.join(os.path.dirname(os.path.realpath(__file__)), '..', 'raw_data', 'gps_target_folder', '*.csv'))
        for csv_file in csv_files:
            gps_object = process_gps_csv_file(csv_file)
            if gps_object:
                instantiate_gps_data(gps_object, kg_client, ts_client, double_class)
    except Exception as e:
        print(f"Initialization failed: {e}")