# This module instantiates GPS trajectory time series data collected in Fenland, Cambridgeshire, UK
# Each .csv file in the target folder in a time series for various utilities in GPS recording (coordinates, speed, distance, etc)
# ===============================================================================

import pandas as pd
import glob
import datetime as dt
import random
import uuid
from jpsSingletons import jpsBaseLibView
import utils

# Create database and RDF store, and the name and url for database is set in resources/.properties file
if __name__ == '__main__':
    utils.create_postgres_db()  # Initialize PostgreSQL database
    utils.create_blazegraph_namespace()  # Initialize Blazegraph namespace for RDF data

    # Initialize KGClient for RDF data manipulation
    KGClient = jpsBaseLibView.RemoteStoreClient(utils.QUERY_ENDPOINT, utils.UPDATE_ENDPOINT)

    # Retrieve Java classes for handling time data and values
    Instant = jpsBaseLibView.java.time.Instant
    instant_class = Instant.now().getClass()
    double_class = jpsBaseLibView.java.lang.Double.TYPE

    # Set the target folder
    # read all the csv file stored in the target folder
    # find each csv file containing gps data, launch a loop to instantiate utilities
    target_folder_path = 'path/to/your/target/folder/*.csv'  # Please adjust this path as necessary
    csv_files = glob.glob(target_folder_path)
    
    for csv_file in csv_files:
        df = pd.read_csv(csv_file)  # Read CSV file into DataFrame

         # Unit Cleaning, Extract numeric values and handle units for SPEED, DISTANCE, and HEIGHT
        for column in ['SPEED', 'DISTANCE', 'HEIGHT']:
            if column in df.columns:
                df[column] = df[column].astype(str).str.split().str[0].astype(float)

        # Extract object (GPS trajectory) information from the CSV file
        gps_object = {
            'object': csv_file.split('/')[-1].replace('.csv', ''),  # Use file name as object name
            'lat': str(df['LATITUDE'].iloc[0]),  # Use first latitude value as example
            'lon': str(df['LONGITUDE'].iloc[0]),  # Adjust to match your CSV structure if necessary
            'times': [f"{row['UTC DATE']} {row['UTC TIME']}" for _, row in df.iterrows()],
            'timeseries': {
                'Speed': [float(row['SPEED'].split()[0]) for _, row in df.iterrows()],
            },
        }

        # Generate uuid and IRI for the GPS object
        objectIRI = utils.PREFIXES['ex'] + 'Object_' + str(uuid.uuid4())
        dataIRIs = []

        # Create and execute SPARQL queries for RDF data insertion
        for ts, values in gps_object['timeseries'].items():
            dataIRI = utils.PREFIXES['ex'] + ts + '_' + str(uuid.uuid4())
            dataIRIs.append(dataIRI)

            # SPARQL query to insert GPS object and time series information
            query = utils.create_sparql_prefix('ex') + \
                    utils.create_sparql_prefix('rdf') + \
                    utils.create_sparql_prefix('rdfs') + \
                    utils.create_sparql_prefix('geolit') + \
                    f'''INSERT DATA {{
                    <{objectIRI}> rdf:type ex:Object ;
                         rdfs:label "{gps_object['object']}" ;
                         ex:hasTrajectory <{dataIRI}> ;
                         ex:hasLocation "{gps_object['lat']}#{gps_object['lon']}"^^geolit:lat-lon .
                    <{dataIRI}> rdf:type ex:Trajectory ;
                         rdfs:label "{ts} trajectory" ;
                         ex:unit "km/h" . }}'''
            KGClient.executeUpdate(query)
            # Initialize and populate time series data in both KG and RDB
        TSClient = jpsBaseLibView.TimeSeriesClient(instant_class, utils.PROPERTIES_FILE)
        TSClient.initTimeSeries(dataIRIs, [double_class] * len(dataIRIs), 'yyyy/MM/dd HH:mm:ss')

        
    