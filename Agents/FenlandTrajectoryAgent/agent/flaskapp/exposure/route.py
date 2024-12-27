from flask import Flask, request, jsonify
from flask_cors import CORS
from SPARQLWrapper import SPARQLWrapper, JSON
import pandas as pd
from shapely.geometry import Point, Polygon
import math
import requests
import configparser

app = Flask(__name__)

cache = {}

config = configparser.ConfigParser()
config.read('CONFIG.PROPERTIES')

TRAJECTORY_ENDPOINT = config.get('DEFAULT', 'TRAJECTORY_ENDPOINT')
FEATURE_ENDPOINTS = config.items('FEATURE_ENDPOINTS')

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


def fetch_food_hygiene_data(domain_iri, endpoint_url):
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

    response = requests.post(endpoint_url, data=sparql_query, headers=headers)

    if response.status_code == 200:
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

        df = pd.DataFrame(data)
        return df
    else:
        raise Exception(f"SPARQL query failed with status code {response.status_code}: {response.text}")

def fetch_greenspace_data(domain_iri: str, endpoint_url: str) -> pd.DataFrame:
    sparql_query = f"""
    PREFIX gs: <https://www.theworldavatar.com/kg/ontogreenspace/>

    SELECT ?feature ?function ?geometry
    WHERE {{
      ?feature gs:isPartOfDomain <{domain_iri}> ;
               gs:hasFunction ?function .
      ?geoPoint gs:isPartOf ?feature ;
                gs:hasGeometry ?geometry .
    }}
    """

    headers = {
        "Content-Type": "application/sparql-query",
        "Accept": "application/json",
    }

    response = requests.post(endpoint_url, data=sparql_query, headers=headers)

    if response.status_code == 200:
        results = response.json()
        data = []
        for binding in results["results"]["bindings"]:
            feature = binding.get("feature", {}).get("value", "N/A")
            function = binding.get("function", {}).get("value", "N/A")
            geometry = binding.get("geometry", {}).get("value", "N/A")

            data.append({
                "Feature": feature,
                "Function": function,
                "Geometry": geometry,
            })
        
        df = pd.DataFrame(data)
        return df
    else:
        raise Exception(f"Error: {response.status_code} - {response.text}")

def fetch_trajectory_data(trajectory_iri, endpoint_url=TRAJECTORY_ENDPOINT):
    sparql_query = f"""
    PREFIX timeseries: <https://www.theworldavatar.com/kg/ontotimeseries/>
    PREFIX ontodevice: <https://www.theworldavatar.com/kg/ontodevice.owl/>
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

    SELECT ?time ?lat ?long
    WHERE {{
        <{trajectory_iri}> 
            timeseries:hasTime ?time ;
            ontodevice:hasLatitude ?lat ;
            ontodevice:hasLongitude ?long .
    }}
    Limit 200
    """

    headers = {
        "Content-Type": "application/sparql-query",
        "Accept": "application/json",
    }

    response = requests.post(endpoint_url, data=sparql_query, headers=headers)

    if response.status_code == 200:
        results = response.json()
        data = []
        for binding in results["results"]["bindings"]:
            row = {
                "time": binding.get("time", {}).get("value", "N/A"),
                "lat": binding.get("lat", {}).get("value", "N/A"),
                "long": binding.get("long", {}).get("value", "N/A"),
            }
            data.append(row)

        df = pd.DataFrame(data)
        df['time'] = pd.to_datetime(df['time']) 
        return df
    else:
        raise Exception(f"SPARQL query failed with status code {response.status_code}: {response.text}")

def fetch_trajectory_data_from_db(trajectory_iri: str) -> pd.DataFrame:
    host = "174.138.27.240"
    port = 5432
    user = "postgres"
    password = "1111"
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
    trajectory_iri = data['trajectoryIRI']
    exposure_radius = data['exposure_radius'] 
    feature_iris = data['feature_IRIs']

    if trajectory_iri in cache:
        return jsonify(cache[trajectory_iri])

    try:
        trajectory_df = fetch_trajectory_data(trajectory_iri)

        feature_dfs = []
        for feature_iri, endpoint_url in zip(feature_iris, [url for _, url in FEATURE_ENDPOINTS]):
            if 'OntoFHRS' in feature_iri:
                feature_df = fetch_food_hygiene_data(feature_iri, endpoint_url)
            elif 'ontogreenspace' in feature_iri:
                feature_df = fetch_greenspace_data(feature_iri, endpoint_url)
            else:
                raise ValueError(f"Unsupported feature IRI: {feature_iri}")
            feature_dfs.append(feature_df)

    except Exception as e:
        return jsonify({"error": str(e)}), 500

    exposures = []
    for _, row in trajectory_df.iterrows():
        lat = float(row['lat']) 
        long = float(row['long'])

        lat_diff = exposure_radius / 111320
        long_diff = exposure_radius / (40075000 * math.cos(lat * math.pi / 180) / 360)

        exposure = []
        for feature_df in feature_dfs:
            if 'Latitude' in feature_df.columns:
                exposure.extend(feature_df[
                    (abs(feature_df['Latitude'].astype(float) - lat) <= lat_diff) &
                    (abs(feature_df['Longitude'].astype(float) - long) <= long_diff)  
                ].to_dict('records'))
            else:
                # TODO: handle other feature types, e.g., GreenSpace
                pass

        exposures.append({
            'time': row['time'],
            'exposure': exposure
        })

    response = {
        'trajectory_iri': trajectory_iri,
        'exposures': exposures
    }
    cache[trajectory_iri] = response
    return jsonify(response)

if __name__ == '__main__':
    app.run(port=3840)