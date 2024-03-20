import pandas as pd
import os
import glob
import uuid
from agent.kgutils.utils import utils
from agent.utils.baselib_gateway import jpsBaseLibGW
from kgutils.kgclient import KGClient
from kgutils.tsclient import TSClient
from agent.utils.env_configs import DB_URL, DB_USER, DB_PASSWORD
from datetime import datetime
from jpsSingletons import jpsBaseLibView

def transform_datetime(date_str, time_str):
    """Transforms separate date and time strings into a single datetime string in ISO 8601 format."""
    combined_str = date_str.strip() + " " + time_str.strip()  # Ensure no leading/trailing spaces
    datetime_obj = datetime.strptime(combined_str, "%Y/%m/%d %H:%M:%S")
    return datetime_obj.strftime('%Y-%m-%dT%H:%M:%SZ')

def process_gps_csv_file(csv_file):
    df = pd.read_csv(csv_file)  # Read CSV file into DataFrame
    print(df.columns)

    # Unit Cleaning, Extract numeric values and handle units for SPEED, DISTANCE, and HEIGHT
    for column in ['SPEED', 'DISTANCE', 'HEIGHT']:
        if column in df.columns:
            df[column] = df[column].astype(str).str.split().str[0].astype(float)

    # Extract object (GPS trajectory) information from the CSV file
    gps_object = {
        'object': csv_file.split('/')[-1].replace('.csv', ''),  # Use file name as object name
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

    return gps_object

def instantiate_gps_data(gps_object, kg_client, ts_client):
    # Generate uuid and IRI for the GPS object
    objectIRI = utils.PREFIXES['ontodevice'] + 'Object_' + str(uuid.uuid4())

    # Initialise list of dataIRIs, which will be represented as time series
    dataIRIs = []

    # Create and execute SPARQL queries for RDF data insertion
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