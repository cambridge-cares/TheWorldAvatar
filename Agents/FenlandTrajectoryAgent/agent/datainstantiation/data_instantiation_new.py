# This module instantiates GPS trajectory time series data collected in Fenland, Cambridgeshire, UK
# Each .csv file in the target folder in a time series for various utilities in GPS recording (coordinates, speed, distance, etc)
# ===============================================================================

import pandas as pd
import os
import sys
from agent.kgutils.utils import utils
import glob
import datetime as dt
import random
import uuid
from jpsSingletons import jpsBaseLibView
from agent.utils.baselib_gateway import jpsBaseLibGW
from kgutils.kgclient import KGClient
from kgutils.tsclient import TSClient
from agent.utils.env_configs import DB_URL, DB_USER, DB_PASSWORD
from datetime import datetime
from datamodel.data_class import DATACLASS


# ===============================================================================
# Data Preparation
# ===============================================================================

script_directory = os.path.dirname(os.path.realpath(__file__))

# Construct the absolute path to the target folder
target_folder_path = os.path.join(script_directory, '..', 'raw_data', 'gps_target_folder', '*.csv')


# Added transform function to make the UTC DATE Aand UTC TIME columns suitable as the input of TSClient
# The transform_datetime function merges separate date and time strings into a unified datetime string in ISO 8601 format. 
# It also cleans the input strings of any leading or trailing spaces, combines them, and then formats the result as an internationally recognized datetime string
def transform_datetime(date_str, time_str):
    """Transforms separate date and time strings into a single datetime string in ISO 8601 format."""
    combined_str = date_str.strip() + " " + time_str.strip()  # Ensure no leading/trailing spaces
    datetime_obj = datetime.strptime(combined_str, "%Y/%m/%d %H:%M:%S")
    return datetime_obj.strftime('%Y-%m-%dT%H:%M:%SZ')

# ===============================================================================
# Main Processing
# ===============================================================================
# Create database and RDF store, and the name and url for database is set in resources/.properties file
if __name__ == '__main__':

    utils.create_postgres_db()  # Initialize PostgreSQL database
    utils.create_blazegraph_namespace()  # Initialize Blazegraph namespace for RDF data

    # Initialize KGClient for RDF data manipulation
    kg_client = KGClient(utils.QUERY_ENDPOINT, utils.UPDATE_ENDPOINT)

    # Initialize the new TSClient with necessary parameters
    ts_client = TSClient(kg_client, DB_URL, DB_USER, DB_PASSWORD)

    # Retrieve Java classes for time entries (Instant) and data (ALL Double)
    # (required for time series client instantiation)
    Instant = jpsBaseLibView.java.time.Instant
    instant_class = Instant.now().getClass()
    double_class = jpsBaseLibView.java.lang.Double.TYPE

    # Set the target folder
    # Access all the csv file stored in the target folder
    # target each csv file containing gps data one by one, launch a loop to instantiate utilities
    csv_files = glob.glob(target_folder_path)
    
    for csv_file in csv_files:
        df = pd.read_csv(csv_file)  # Read CSV file into DataFrame
        print(df.columns)

         # Unit Cleaning, Extract numeric values and handle units for SPEED, DISTANCE, and HEIGHT
        for column in ['SPEED', 'DISTANCE', 'HEIGHT']:
            if column in df.columns:
                df[column] = df[column].astype(str).str.split().str[0].astype(float)

        # Extract object (GPS trajectory) information from the CSV file
        gps_object = {
            'object': csv_file.split('/')[-1].replace('.csv', ''),  # Use file name as object name
            # 'times': [f"{row['UTC DATE']} {row['UTC TIME']}" for _, row in df.iterrows()],
            'times': [transform_datetime(row['UTC DATE'], row['UTC TIME']) for _, row in df.iterrows()],  # Transform dates and times
            'timeseries': {
                'Speed': df['SPEED'].tolist() if 'SPEED' in df.columns else [],
                'Distance': df['DISTANCE'].tolist() if 'DISTANCE' in df.columns else [],
                'Height': df['HEIGHT'].tolist() if 'HEIGHT' in df.columns else [],
                'Heading': df['HEADING'].tolist() if 'HEADING' in df.columns else [],
                'Latitude': df['LATITUDE'].tolist() if 'LATITUDE' in df.columns else [],
                'Longitude': df['LONGITUDE'].tolist() if 'LONGITUDE' in df.columns else [],
            },
            'units': {
                'Speed': 'km/h',
                'Distance': 'M',
                'Height': 'M',
                'Heading': None,  # No unit for Heading in the raw dataset
                'Latitude': None,  # No actual unit needed, but for consistency
                'Longitude': None
            }
        }

# ===============================================================================
# Start to instantiate GPS trajectory data
# ===============================================================================

        # Generate uuid and IRI for the GPS object
        objectIRI = utils.PREFIXES['ontodevice'] + 'Object_' + str(uuid.uuid4())

        # Initialise list of dataIRIs, which will be represented as time series
        dataIRIs = []

        # Create and execute SPARQL queries for RDF data insertion
        # This design utilizes the .items() method insead of .key for iterating over a dictionary of GPS trajectory attributes and their corresponding numerical time series data. 
        # It allows for simultaneous access to attribute names (keys) and their values, facilitating efficient preparation and execution of SPARQL updates for RDF data insertion. 
        # By directly pairing each attribute with its numerical values within the loop, it streamlines the process of mapping and storing time series data

        for ts, values in gps_object['timeseries'].items():
            dataIRI = utils.PREFIXES['ontodevice'] + ts + '_' + str(uuid.uuid4())
            dataIRIs.append(dataIRI)

            # Prepare SPARQL query, assigning units where applicable
            unit = gps_object['units'][ts] if gps_object['units'][ts] else ""
            
             # Perform SPARQL update for non-time series related triples (i.e. without TimeSeriesClient)
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
            print("Triples independent of TimeSeriesClient are successfully instantiated.")
        
            # Perform SPARQL update for time series related triples (i.e. via TimeSeriesClient)
            # Initialise time series in both KG and RDB using TimeSeriesClass
            times = gps_object['times']
            ts_client.init_timeseries(dataIRI, times, values, jpsBaseLibView.java.lang.Double.TYPE, utils.FORMAT)  # Using utils.FORMAT if it's correctly defined
            print("Time series triples and data via TimeSeriesClient successfully instantiated.")
    


        print(f"GPS trajectory data for {gps_object['object']} successfully added.")

    print("All GPS trajectory files have been processed.")

    