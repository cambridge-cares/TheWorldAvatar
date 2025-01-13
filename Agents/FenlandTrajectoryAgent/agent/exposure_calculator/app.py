from flask import Flask, request, jsonify
from flask_cors import CORS
import pandas as pd
import math
import requests
import logging
import psycopg2
from typing import Optional, Any

import os
import sys
import configparser

app = Flask(__name__)
CORS(app)
logging.basicConfig(level=logging.INFO)

CONFIG_FILE = os.path.join(os.path.dirname(__file__), "config.properties")

config = configparser.ConfigParser()

if not os.path.exists(CONFIG_FILE):
    logging.error(f"Config file not found at {CONFIG_FILE}. Please provide one.")
    sys.exit(1)  # or raise SystemExit, etc.

config.read(CONFIG_FILE)

try:
    ENV_DATA_ENDPOINT_URL = config.get("DEFAULT", "ENV_DATA_ENDPOINT_URL")  
    TRAJECTORY_DB_HOST = config.get("DEFAULT", "TRAJECTORY_DB_HOST")
    TRAJECTORY_DB_PASSWORD = config.get("DEFAULT", "TRAJECTORY_DB_PASSWORD")
except configparser.NoOptionError as e:
    logging.error(f"Missing configuration key in config.properties: {e}")
    sys.exit(1)
except configparser.NoSectionError as e:
    logging.error(f"Missing section [DEFAULT] in config.properties: {e}")
    sys.exit(1)

def connect_to_database(
    host: str, port: int, user: str, password: str, database: str
) -> psycopg2.extensions.connection:
    try:
        logging.info(f"Connecting to database {database} at {host}:{port}...")
        connection = psycopg2.connect(
            host=host,
            port=port,
            user=user,
            password=password,
            database=database,
            connect_timeout=10
        )
        logging.info("Database connection successful.")
        return connection
    except psycopg2.Error as e:
        logging.error(f"Database connection failed: {e}")
        raise e

def execute_query(connection: psycopg2.extensions.connection, query: str, params: Optional[tuple] = None) -> Any:
    try:
        with connection.cursor() as cursor:
            logging.debug(f"Executing query: {query}, params={params}")
            cursor.execute(query, params)
            results = cursor.fetchall()
            logging.info(f"Query returned {len(results)} rows." if results else "No results.")
            return results
    except psycopg2.Error as e:
        logging.error(f"Query execution error: {e}")
        raise e

def get_table_name_for_timeseries(connection, timeseriesIRI: str) -> str:
    query = """
    SELECT DISTINCT "tableName"
    FROM "dbTable"
    WHERE "timeseriesIRI" = %s;
    """
    results = execute_query(connection, query, (timeseriesIRI,))
    if not results:
        raise ValueError(f"No tableName found for timeseriesIRI: {timeseriesIRI}")
    return results[0][0]

def get_timeseries_data(connection, table_name: str) -> pd.DataFrame:
    query = f"""
    SELECT "time", "column1", "column2", "column3", "column4", "column5", "column6", "column7"
    FROM "{table_name}";
    """
    results = execute_query(connection, query)
    df = pd.DataFrame(results, columns=["time","column1","column2","column3","column4","column5","column6","column7"])
    return df

def fetch_env_data(domain_iri, endpoint_url=ENV_DATA_ENDPOINT_URL) -> pd.DataFrame:
    sparql_query = f"""
    PREFIX fh: <http://www.theworldavatar.com/ontology/OntoFHRS/>
    SELECT ?id ?businessName ?businessType ?ratingTime ?ratingValue ?longitude ?latitude
    WHERE {{
        ?id fh:isPartOfDomain <{domain_iri}> ;
            fh:hasBusinessName ?businessName ;
            fh:hasBusinessType ?businessType ;
            fh:hasRatingDate ?ratingTime ;
            fh:hasRatingValue ?ratingValue .
        ?geo fh:isPartOf ?id ;
             fh:hasLongitude ?longitude ;
             fh:hasLatitude ?latitude .
    }}
    """

    headers = {
        "Content-Type": "application/sparql-query",
        "Accept": "application/json",
    }

    response = requests.post(endpoint_url, data=sparql_query, headers=headers, timeout=30)
    response.raise_for_status()
    results = response.json()
    data = []
    for binding in results["results"]["bindings"]:
        row = {
            "ID": binding.get("id", {}).get("value", "N/A"),
            "Business Name": binding.get("businessName", {}).get("value", "N/A"),
            "Business Type": binding.get("businessType", {}).get("value", "N/A"),
            "Rating Time": binding.get("ratingTime", {}).get("value", "N/A"),
            "Rating Value": binding.get("ratingValue", {}).get("value", "N/A"),
            "Longitude": binding.get("longitude", {}).get("value", "N/A"),
            "Latitude": binding.get("latitude", {}).get("value", "N/A"),
        }
        data.append(row)
    return pd.DataFrame(data)

def fetch_trajectory_data_from_db(trajectory_iri: str) -> pd.DataFrame:
    logging.info("==== fetch_trajectory_data_from_db() was called ====")
    
    host = TRAJECTORY_DB_HOST
    port = 5432
    user = "postgres"
    password = TRAJECTORY_DB_PASSWORD
    database = "postgres"
    with connect_to_database(host, port, user, password, database) as conn:
        table_name = get_table_name_for_timeseries(conn, trajectory_iri)
        df = get_timeseries_data(conn, table_name)

    df = df.rename(columns={
        "column1": "SPEED",
        "column2": "DISTANCE",
        "column3": "HEIGHT",
        "column4": "HEADING",
        "column5": "LATITUDE",
        "column6": "LONGITUDE",
        "column7": "POINT"
    })

    if 'time' in df.columns:
        df['time'] = pd.to_datetime(df['time'], errors='coerce')
        df = df.sort_values('time')

    return df

@app.route('/fenland-trajectory-agent/exposure', methods=['POST'])
def calculate_exposure():
    data = request.json
    trajectoryIRIs = data.get('trajectoryIRIs', [])
    exposure_radius = data['exposure_radius']
    DataIRIs = data.get('DataIRIs', [])

    iri_to_name_map = {
        "http://www.theworldavatar.com/ontology/OntoFHRS/FoodHygieneRating": "Food hygiene rating"
    }

    results = []
    for trajectoryIRI in trajectoryIRIs:
        try:
            trajectory_df = fetch_trajectory_data_from_db(trajectoryIRI)
        except Exception as e:
            return jsonify({"error": str(e)}), 500

        all_exposures_for_trajectory = {}
        for env_data_iri in DataIRIs:
            try:
                env_df = fetch_env_data(env_data_iri)
            except Exception as e:
                return jsonify({"error": str(e)}), 500

            exposures = []
            for _, row in trajectory_df.iterrows():
                lat = float(row['LATITUDE'])
                lon = float(row['LONGITUDE'])

                lat_diff = exposure_radius / 111320.0
                try:
                    denominator = 40075000 * math.cos(lat * math.pi / 180) / 360
                    if abs(denominator) < 1e-9:
                        long_diff = 0.0
                    else:
                        long_diff = exposure_radius / denominator
                except:
                    long_diff = 0.0

                try:
                    env_exposure = env_df[
                        (env_df['Latitude'].astype(float) >= (lat - lat_diff)) &
                        (env_df['Latitude'].astype(float) <= (lat + lat_diff)) &
                        (env_df['Longitude'].astype(float) >= (lon - long_diff)) &
                        (env_df['Longitude'].astype(float) <= (lon + long_diff))
                    ].to_dict('records')
                except Exception as exc:
                    logging.error(f"Error filtering env data: {exc}")
                    env_exposure = []

                exposures.append(env_exposure)

            all_exposures_for_trajectory[env_data_iri] = exposures

        data_list = []
        for i, row in trajectory_df.iterrows():
            entry = {
                "time": row['time'].isoformat() if pd.notnull(row['time']) else None,
                "LATITUDE": float(row['LATITUDE']),
                "LONGITUDE": float(row['LONGITUDE'])
            }

            for env_data_iri in DataIRIs:
                column_name = iri_to_name_map.get(env_data_iri, env_data_iri)
                exposure_data = all_exposures_for_trajectory[env_data_iri][i]
                if exposure_data and len(exposure_data) > 0:
                    entry[column_name] = exposure_data
                else:
                    entry[column_name] = "None"

            data_list.append(entry)

        # "results" [
        #    {
        #       "trajectoryIRI": "....",
        #       "data": [...]
        #    }
        # ]
        results.append({
            "trajectoryIRI": trajectoryIRI,
            "data": data_list
        })

    return jsonify({"results": results})

if __name__ == '__main__':
    app.run(port=3840)