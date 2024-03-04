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

    # Initialize KG client for RDF data manipulation
    KGClient = jpsBaseLibView.RemoteStoreClient(utils.QUERY_ENDPOINT, utils.UPDATE_ENDPOINT)

    # Retrieve Java classes for handling time data and values
    Instant = jpsBaseLibView.java.time.Instant
    instant_class = Instant.now().getClass()
    double_class = jpsBaseLibView.java.lang.Double.TYPE

    # Specify the target folder, and read all the csv file stored in the target folder
    target_folder_path = 'path/to/your/target/folder/*.csv'  # Please adjust this path as necessary
    csv_files = glob.glob(target_folder_path)
    
    for csv_file in csv_files:
        df = pd.read_csv(csv_file)  # Read CSV file into DataFrame
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

        
    