# This module instantiates GPS trajectory time series data collected in Fenland, Cambridgeshire, UK
# Each .csv file in the target folder in a time series for various utilities in GPS recording (coordinates, speed, distance, etc)
# ===============================================================================

import os
import sys
import pandas as pd
import glob
import uuid
from datetime import datetime
import agent.kgutils.utils as utils
from agent.datainstantiation.jpsSingletons import jpsBaseLibView
from agent.kgutils.kgclient import KGClient
from agent.kgutils.tsclient import TSClient
from agent.utils.stack_configs import DB_URL, DB_USER, DB_PASSWORD, SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT

# ===============================================================================
# Data Preparation
# ===============================================================================

script_directory = os.path.dirname(os.path.realpath(__file__))
target_folder_path = os.path.join(script_directory, '..', 'raw_data', 'gps_target_folder', '*.csv')

# Added transform function to make the UTC DATE Aand UTC TIME columns suitable as the input of TSClient
# The transform_datetime function merges separate date and time strings into a unified datetime string in ISO 8601 format. 
# It also cleans the input strings of any leading or trailing spaces, combines them, and then formats the result as an internationally recognized datetime string

def transform_datetime(date_str, time_str):
    """Transforms separate date and time strings into a single datetime string in ISO 8601 format."""
    try:
        combined_str = date_str.strip() + " " + time_str.strip()
        datetime_obj = datetime.strptime(combined_str, "%Y/%m/%d %H:%M:%S")
        return datetime_obj.strftime('%Y-%m-%dT%H:%M:%SZ')
    except Exception as e:
        print(f"Error transforming datetime: {date_str}, {time_str} - {e}")
        return None

# ===============================================================================
# Main Processing
# ===============================================================================
# Create database and RDF store, instantiate and upload data using kgclient and tsclient


def main():
    try:
        utils.create_postgres_db()
        utils.create_blazegraph_namespace()
    except Exception as e:
        print(f"Failed to initialize databases: {e}")
        sys.exit(1)

    try:
        # Retrieve Java classes for time entries (Instant) and data (ALL Double)
        # (required for time series client instantiation)
        Instant = jpsBaseLibView.java.time.Instant
        instant_class = Instant.now().getClass()
        double_class = jpsBaseLibView.java.lang.Double.TYPE
        
        # Initialize KGClient for RDF data manipulation
        kg_client = KGClient(SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT)

        # Initialize the new TSClient with necessary parameters
        ts_client = TSClient(kg_client, timeclass=instant_class, rdb_url=DB_URL, rdb_user=DB_USER, rdb_password=DB_PASSWORD)
    except Exception as e:
        print(f"Failed to initialize clients: {e}")
        sys.exit(1)

    # Set the target folder
    # Access all the csv file stored in the target folder
    # target each csv file containing gps data one by one, launch a loop to instantiate utilities
    csv_files = glob.glob(target_folder_path)
    for csv_file in csv_files:
        try:
            df = pd.read_csv(csv_file)
            df.columns = df.columns.str.strip()
            required_columns = ['UTC DATE', 'UTC TIME', 'SPEED', 'DISTANCE', 'HEIGHT']
            if not all(column in df.columns for column in required_columns) or df[required_columns].isnull().any().any():
                print(f"Skipping {csv_file} due to missing or incomplete required columns.")
                continue
        except Exception as e:
            print(f"Failed to process file {csv_file}: {e}")
            continue

        try:
            gps_object = {
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
            
            # Perform SPARQL update for time series related triples (i.e. via TimeSeriesClient)
            # Initialise time series in both KG and RDB using TimeSeriesClass
            TS_times = gps_object['times']
            TS_values = [gps_object['timeseries'][ts] for ts in gps_object['timeseries']]
            ts_client.init_timeseries(dataIRI = dataIRIs, times=TS_times, values=TS_values, ts_type = [double_class] * len(dataIRIs), time_format = utils.FORMAT)  # Using utils.FORMAT if it's correctly defined
        except Exception as e:
            print(f"Error processing or updating data for {csv_file}: {e}")

    print("All GPS trajectory files have been processed.")


if __name__ == '__main__':
    main()